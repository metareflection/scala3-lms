package scala.lms
package internal

import util.GraphUtil
import scala.collection.mutable
import java.util.IdentityHashMap
import scala.jdk.CollectionConverters._

trait Scheduling {
  val IR: Expressions
  import IR._

  def getUnsortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    getSchedule(scope)(result, false)
  }
  
  // checks if a and b share at least one element. O(N^2), but with no allocation and possible early exit.
  def containsAny(a: List[Sym[Any]], b: List[Sym[Any]]): Boolean = {
    var aIter = a
    while (aIter.nonEmpty) {
      val aElem = aIter.head
      aIter = aIter.tail
      var bIter = b
      while (bIter.nonEmpty) {
        if (bIter.head eq aElem) return true
        bIter = bIter.tail
      }
    }
    false
  }
   
  //TBD: not used?
  def getStronglySortedSchedule(scope: List[Stm])(result: Any): List[Stm] = {
    def deps(st: List[Sym[Any]]): List[Stm] = 
      scope.filter(d => containsAny(st, infix_lhs(d)))
      // scope.filter(d => (st intersect d.lhs).nonEmpty)
    def allSyms(r: Any) = syms(r) ++ softSyms(r)
    
    val xx = GraphUtil.stronglyConnectedComponents[Stm](deps(allSyms(result)), t => deps(allSyms(infix_rhs(t))))
    xx.foreach { x => 
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception).printStackTrace()
      }
    }
    xx.flatten.reverse
  }

  //performance hotspot!
  //should be O(1) wrt 'scope' (nodes in graph), try to keep this as efficient as possible
  protected def scheduleDepsWithIndex(syms: List[Sym[Any]], cache: IdentityHashMap[Sym[Any], (Stm,Int)]): List[Stm] = {
    //syms.map(cache.get(_)).filter(_ ne null).distinct.sortBy(_._2).map(_._1)
    val sortedSet = new java.util.TreeSet[(Stm,Int)](
      new java.util.Comparator[(Stm,Int)] { def compare(a:(Stm,Int), b:(Stm,Int)) = if (b._2 < a._2) -1 else if (b._2 == a._2) 0 else 1 }
    )
    
    for (sym <- syms) {
      val stm = cache.get(sym)
      if (stm ne null) sortedSet.add(stm)
    }

    var res: List[Stm] = Nil
    val iter = sortedSet.iterator //return stms in the original order given by 'scope'
    while (iter.hasNext) {
      res ::= iter.next._1
    }
    res
  }

  protected def buildScopeIndex(scope: List[Stm]): IdentityHashMap[Sym[Any], (Stm,Int)] = {
    val cache = new IdentityHashMap[Sym[Any], (Stm,Int)]
    var idx = 0
    for (stm <- scope) {
      for (s <- infix_lhs(stm)) cache.put(s, (stm,idx)) //remember the original order of the stms
      idx += 1
    }
    cache
  }

  def getSchedule(scope: List[Stm])(result: Any, sort: Boolean = true): List[Stm] = {
    val scopeIndex = buildScopeIndex(scope)

    val xx = GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(syms(result), scopeIndex), t => scheduleDepsWithIndex(syms(infix_rhs(t)), scopeIndex))
    if (sort) xx.foreach { x => 
      if (x.length > 1) {
        printerr("warning: recursive schedule for result " + result + ": " + x)
        (new Exception).printStackTrace()
      }
    }
    xx.flatten.reverse
  }

  def getScheduleM(scope: List[Stm])(result: Any, cold: Boolean, hot: Boolean): List[Stm] = {
    def mysyms(st: Any) = {
      val db = symsFreq(st).groupBy(_._1).mapValues(_.map(_._2).sum).toList
      assert(syms(st).toSet == db.map(_._1).toSet, "different list of syms: "+syms(st)+"!="+db+" for "+st)
      if (cold && hot) db.map(_._1)
      else if (cold && !hot) db.withFilter(_._2 < 100.0).map(_._1)
      else if (!cold && hot) db.withFilter(_._2 > 0.75).map(_._1)
      else db.withFilter(p=>p._2 > 0.75 && p._2 < 100.0).map(_._1)
    }

    val scopeIndex = buildScopeIndex(scope)

    GraphUtil.stronglyConnectedComponents[Stm](scheduleDepsWithIndex(mysyms(result), scopeIndex), t => scheduleDepsWithIndex(mysyms(infix_rhs(t)), scopeIndex)).flatten.reverse
  }
    
  
  /** begin performance hotspot **/
  
  /*
  for each symbol s in sts, find all statements that depend on it.
  we need to stop when we reach the statement where s is bound.
  
  it would be tempting to do only one scc call but then we mix
  up the locations where different symbols are bound.
  */
  
  def getFatDependentStuff(scope: List[Stm])(sts: List[Sym[Any]]): List[Stm] = {
    if (sts.isEmpty) return Nil
    /*
     precompute:
     s => all d in scope such that: d.lhs contains s || syms(d.rhs).contains(s)
     st => all d in scope such that: boundSyms(d.rhs) contains st
    */
    
    //type IdentityHashMap[K,V] = HashMap[K,V]
    
    // IdentityHashMap appears faster than scala.collection.mutable.HashMap here (based on perf. testing)
    // possible improvement: use an integer hashmap that works directly with sym ids
    
    val lhsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val symsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    val boundSymsCache = new IdentityHashMap[Sym[Any], List[Stm]]()
    //val boundSymsCache = new IdentityHashMap[Sym[Any], Set[Stm]]()
    
    def infix_getOrElse[K,V](map: IdentityHashMap[K, V], s: K, f: => V) = {
      var res = map.get(s) //map(s)
      if (res == null) res = f
      res
    }
    
    def putDef(map: IdentityHashMap[Sym[Any], List[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s) //map(s)
      if (res == null) res = Nil
      //map.getOrElse(s, Nil) match {
      res match {
        case `d`::ds =>
        case ds => map.asScala.update(s,d::ds) //map.put(s,d::ds)
      }
    }
    
    def putDefSet(map: IdentityHashMap[Sym[Any], Set[Stm]], s: Sym[Any], d: Stm): Unit = {
      var res = map.get(s) //map.get(s)
      if (res == null) {
        res = Set[Stm]()
        map.asScala.update(s,res) //map.put(s,res)
      }
      res += d
    }
    
    for (d <- scope) {
      infix_lhs(d).foreach(s => putDef(lhsCache, s, d))
      syms(infix_rhs(d)).foreach(s => putDef(symsCache, s, d))      
      boundSyms(infix_rhs(d)).foreach(st => putDef(boundSymsCache, st, d))
      tunnelSyms(infix_rhs(d)).foreach(st => putDef(boundSymsCache, st, d)) // treat tunnel like bound
    }
    
    /*
    optimization:
      traverse syms by ascending id. if sym s1 is used by s2, do not evaluate further 
      uses of s2 because they are already there.

    CAVEAT: TRANSFORMERS !!!

    assumption: if s2 uses s1, the scope of s2 is completely included in s1's scope:

      val A = loop { s1 => ... val B = sum { s2 => ... val y = s2 + s1; .../* use y */ ... } }

      once we reach y the second time (from s2) we can stop, because the uses of
      y have been tracked up to A, which includes all of B
    */

    val seen = new mutable.HashSet[Sym[Any]]
    
    def getDepStuff(st: Sym[Any]) = {
      // could also precalculate uses, but computing all combinations eagerly is also expensive
      def uses(s: Sym[Any]): List[Stm] = if (seen(s)) Nil else { 
        //seen += s
        lhsCache.asScala.getOrElse(s,Nil) ::: symsCache.asScala.getOrElse(s,Nil) filterNot (boundSymsCache.asScala.getOrElse(st, Nil) contains _)
      }
      GraphUtil.stronglyConnectedComponents[Stm](
        uses(st),
        t => infix_lhs(t) flatMap uses
      ).flatten
    }
    
    /* 
    reference impl:*/
    val res = sts.flatMap(getDepStuff).distinct
    
    /*if (sts.contains(Sym(1064))) {
      println("dep on x1064:")
      res.foreach { r =>
        println("   " + r)
      }
    }*/
    res

    // CAVEAT: TRANSFORMERS !!!  see CloseWorldRestage app in Delite
    //sts.sortBy(_.id).flatMap(getDepStuff)
  }
  
  /** end performance hotspot **/

}
