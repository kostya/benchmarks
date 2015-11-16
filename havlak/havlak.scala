// Copyright 2011 Google Inc.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

//======================================================
// Scaffold Code
//======================================================

//
// class BasicBlock
//
// BasicBlock only maintains a vector of in-edges and
// a vector of out-edges.
//
class BasicBlock(name: Int) {
  var inEdges  = List[BasicBlock]()
  var outEdges = List[BasicBlock]()
  BasicBlock.numBasicBlocks += 1

  override def toString = "BB#" + name
  def getNumPred  : Int = inEdges.length
  def getNumSucc  : Int = outEdges.length
  def addInEdge (bb : BasicBlock) = inEdges  = bb :: inEdges
  def addOutEdge(bb : BasicBlock) = outEdges = bb :: outEdges // forces prepend

  def dump = {
    var res = "BB#" + name + " "
    if (inEdges.length > 0)
      res += "\tin : " + inEdges
    if (outEdges.length > 0)
      res += "\tout: " + outEdges
    println(res)
  }
}

// BasicBlock's static members
//
object BasicBlock {
  var numBasicBlocks = 0
  def getNumBasicBlocks : Int = numBasicBlocks
}


//
// class CFG
//
// CFG maintains a list of nodes, plus a start node.
// That's it.
//
class CFG {
  var startNode     : BasicBlock = _
  var basicBlockMap = Map[Int, BasicBlock]()
  var edgeList      = List[BasicBlockEdge]()

  def createNode(name : Int) = {
    val node = if (basicBlockMap.contains(name)) basicBlockMap(name) else {
      val tmp = new BasicBlock(name)
      basicBlockMap += (name -> tmp)
      tmp
    }

    if (getNumNodes == 1)
      startNode = node
    node
  }

  def dump = {
    for (bb <- basicBlockMap.values)
      bb.dump
  }

  def addEdge(edge : BasicBlockEdge) = {
    edgeList = edge :: edgeList
  }

  def getNumNodes : Int = basicBlockMap.size
  def getDst(edge : BasicBlockEdge) : BasicBlock = edge.to
  def getSrc(edge : BasicBlockEdge) : BasicBlock = edge.from
}

//
// class BasicBlockEdge
//
// These data structures are stubbed out to make the code below easier
// to review.
//
// BasicBlockEdge only maintains two pointers to BasicBlocks.
//
class BasicBlockEdge(cfg : CFG, fromName : Int, toName : Int) {
  var from : BasicBlock = cfg.createNode(fromName)
  var to   : BasicBlock = cfg.createNode(toName)

  from.addOutEdge(to)
  to.addInEdge(from)

  cfg.addEdge(this)
}


//
// class SimpleLoop
//
// Basic representation of loops, a loop has an entry point,
// one or more exit edges, a set of basic blocks, and potentially
// an outer loop - a "parent" loop.
//
// Furthermore, it can have any set of properties, e.g.,
// it can be an irreducible loop, have control flow, be
// a candidate for transformations, and what not.
//
class SimpleLoop {
  var basicBlocks  = Set[BasicBlock]()
  var children     = Set[SimpleLoop]()
  var parent       : SimpleLoop = null
  var header       : BasicBlock = null

  var isRoot       : Boolean = false
  var isReducible  : Boolean = true
  var counter      : Int = 0
  var nestingLevel : Int = 0
  var depthLevel   : Int = 0

  def addNode(bb : BasicBlock) = basicBlocks += bb
  def addChildLoop(loop : SimpleLoop) = children += loop

  def dump(indent : Int) {
    for (i <- 0 until indent)
      System.out.format("  ")

    System.out.format("loop-%d nest: %d depth %d %s\n",
                      counter.asInstanceOf[AnyRef],
                      nestingLevel.asInstanceOf[AnyRef],
                      depthLevel.asInstanceOf[AnyRef],
                      if (isReducible) ""
                      else "(Irreducible) " );

  }

  def setParent(parent : SimpleLoop) = {
    this.parent = parent
    this.parent.addChildLoop(this)
  }
  def setHeader(bb : BasicBlock) = {
    basicBlocks += bb
    header = bb
  }
  def setNestingLevel(level : Int) = {
    nestingLevel = level
    if (level == 0) isRoot_=(true)
  }
}


//
// LoopStructureGraph
//
// Maintain loop structure for a given CFG.
//
// Two values are maintained for this loop graph, depth, and nesting level.
// For example:
//
// loop        nesting level    depth
//----------------------------------------
// loop-0      2                0
//   loop-1    1                1
//   loop-3    1                1
//     loop-2  0                2
//
class LSG {
  var loops = List[SimpleLoop]()
  var root  = new SimpleLoop()
  root.setNestingLevel(0)
  root.counter_=(LSG.loopCounter)
  LSG.loopCounter += 1
  addLoop(root)

  def createNewLoop : SimpleLoop = {
    var loop = new SimpleLoop
    loop.counter_=(LSG.loopCounter)
    LSG.loopCounter += 1
    return loop
  }

  def addLoop(loop : SimpleLoop) = loops = loop :: loops

  def dump = dumpRec(root,0)

  // Interesting - needs return type
  def dumpRec(loop : SimpleLoop, indent : Int) : Unit = {
    loop.dump(indent)

    for (liter <- loop.children)
      dumpRec(liter, indent + 1)
  }

  def calculateNestingLevel = {
    for (liter <- loops) {
      if (!liter.isRoot)
        if (liter.parent == null)
          liter.setParent(root)
    }

    calculateNestingLevelRec(root, 0)
  }

  def max(a : Int, b : Int) = if (a > b) a else b

  def calculateNestingLevelRec(loop : SimpleLoop, depth : Int) {
    loop.depthLevel_=(depth)
    for (liter <- loop.children) {
      calculateNestingLevelRec(liter, depth+1)

      loop.setNestingLevel(max(loop.nestingLevel,
                               1+liter.nestingLevel))
    }
  }

  def getNumLoops : Int = loops.size
}

object LSG {
  var loopCounter = 0
}

//======================================================
// Main Algorithm
//======================================================

class HavlakLoopFinder(cfg : CFG, lsg : LSG) {
  /**
   * enum BasicBlockClass
   *
   * Basic Blocks and Loops are being classified as regular, irreducible,
   * and so on. This enum contains a symbolic name for all these classifications
   */
  class BasicBlockClass extends Enumeration {
  }
  object BasicBlockClass extends Enumeration {
    val BB_TOP,      // uninitialized
    BB_NONHEADER,    // a regular BB
    BB_REDUCIBLE,    // reducible loop
    BB_SELF,         // single BB loop
    BB_IRREDUCIBLE,  // irreducible loop
    BB_DEAD,         // a dead BB
    BB_LAST = Value  // Sentinel
  }

  /**
   * class UnionFindNode
   *
   * The algorithm uses the Union/Find algorithm to collapse
   * complete loops into a single node. These nodes and the
   * corresponding functionality are implemented with this class
   */
  class UnionFindNode {

    var parent    : UnionFindNode = null
    var bb        : BasicBlock    = null
    var loop      : SimpleLoop    = null
    var dfsNumber : Int           = 0

    // Initialize this node.
    //
    def initNode(bb : BasicBlock, dfsNumber : Int) = {
      this.parent     = this
      this.bb         = bb
      this.dfsNumber  = dfsNumber
      this.loop       = null
    }

    // Union/Find Algorithm - The find routine.
    //
    // Implemented with Path Compression (inner loops are only
    // visited and collapsed once, however, deep nests would still
    // result in significant traversals).
    //
    def findSet : UnionFindNode = {
      var nodeList = List[UnionFindNode]()

      var node = this
      while (node != node.parent) {
        if (node.parent != node.parent.parent) {
          nodeList = node :: nodeList
        }
        node = node.parent
      }

      // Path Compression, all nodes' parents point to the 1st level parent.
      for (iter <- nodeList)
         iter.parent_=(node.parent)
      node
    }

    // Union/Find Algorithm - The union routine.
    //
    // Trivial. Assigning parent pointer is enough,
    // we rely on path compression.
    //
    def union(basicBlock : UnionFindNode) = {
      parent_=(basicBlock);
    }
  }

  //
  // Constants
  //
  // Marker for uninitialized nodes.
  val UNVISITED : Int = -1

  // Safeguard against pathologic algorithm behavior.
  val MAXNONBACKPREDS : Int = (32 * 1024)

  //
  // IsAncestor
  //
  // As described in the paper, determine whether a node 'w' is a
  // "true" ancestor for node 'v'.
  //
  // Dominance can be tested quickly using a pre-order trick
  // for depth-first spanning trees. This is why DFS is the first
  // thing we run below.
  //
  def isAncestor(w : Int, v : Int, last : Array[Int]) : Boolean = {
    (w <= v) && (v <= last(w))
  }

  //
  // DFS - Depth-First-Search
  //
  // DESCRIPTION:
  // Simple depth first traversal along out edges with node numbering.
  //
  def DFS(currentNode  : BasicBlock,
          nodes        : Array[UnionFindNode],
          number       : scala.collection.mutable.Map[BasicBlock, Int],
          last         : Array[Int],
    current      : Int) : Int = {
    nodes(current).initNode(currentNode, current)
    number +=(currentNode -> current)

    var lastid = current;
    for (target <- currentNode.outEdges
         if (number(target) == UNVISITED)) {
           lastid = DFS(target, nodes, number, last, lastid + 1)
         }
    last(number(currentNode)) = lastid
    return lastid
  }

  //
  // findLoops
  //
  // Find loops and build loop forest using Havlak's algorithm, which
  // is derived from Tarjan. Variable names and step numbering has
  // been chosen to be identical to the nomenclature in Havlak's
  // paper (which, in turn, is similar to the one used by Tarjan).
  //
  def findLoops : Int = {
    if (cfg.startNode == null) {
      return 0
    }

    var size = cfg.getNumNodes

    var nonBackPreds    = new Array[Set[Int]](size)
    var backPreds       = new Array[List[Int]](size)

    var number          = scala.collection.mutable.Map[BasicBlock, Int]()

    var header          = new Array[Int](size)
    var types           = new Array[BasicBlockClass.Value](size)
    var last            = new Array[Int](size)
    var nodes           = new Array[UnionFindNode](size)

    for (i <- 0 until size) {
      nonBackPreds(i) = Set[Int]()
      backPreds(i)    = List[Int]()
      nodes(i)        = new UnionFindNode()
    }

    // Step a:
    //   - initialize all nodes as unvisited.
    //   - depth-first traversal and numbering.
    //   - unreached BB's are marked as dead.
    //
    for ((key, value) <- cfg.basicBlockMap) {
      number(value) = UNVISITED
    }

    DFS(cfg.startNode, nodes, number, last, 0)

    // Step b:
    //   - iterate over all nodes.
    //
    //   A backedge comes from a descendant in the DFS tree, and non-backedges
    //   from non-descendants (following Tarjan).
    //
    //   - check incoming edges 'v' and add them to either
    //     - the list of backedges (backPreds) or
    //     - the list of non-backedges (nonBackPreds)
    //
    for (w <- 0 until size) {
      header(w) = 0
      types(w)  = BasicBlockClass.BB_NONHEADER

      val nodeW = nodes(w).bb
      if (nodeW == null) {
        types(w) = BasicBlockClass.BB_DEAD
        // No 'continue'
      }
      else {
        if (nodeW.getNumPred > 0) {
          for (nodeV <- nodeW.inEdges) {
            val v = number(nodeV)
            if (v != UNVISITED) {
              if (isAncestor(w, v, last)) {
                backPreds(w) = v :: backPreds(w)
              } else {
                nonBackPreds(w) += v
              }
            }
          }
        }
      }
    }

    // Start node is root of all other loops.
    header(0) = 0

    // Step c:
    //
    // The outer loop, unchanged from Tarjan. It does nothing except
    // for those nodes which are the destinations of backedges.
    // For a header node w, we chase backward from the sources of the
    // backedges adding nodes to the set P, representing the body of
    // the loop headed by w.
    //
    // By running through the nodes in reverse of the DFST preorder,
    // we ensure that inner loop headers will be processed before the
    // headers for surrounding loops.
    //
    for (w <- size - 1 to 0 by -1) {
      // this is 'P' in Havlak's paper
      var nodePool = List[UnionFindNode]()

      val nodeW = nodes(w).bb
      if (nodeW != null) { // dead BB

        // Step d:
        for (v <- backPreds(w)) {
          if (v != w) {
            nodePool = nodes(v).findSet :: nodePool
          } else {
            types(w) = BasicBlockClass.BB_SELF
          }
        }

        // Copy nodePool to workList.
        //
        var workList = List[UnionFindNode]()
        workList = nodePool filter (p => true)

        if (nodePool.size != 0) {
          types(w) = BasicBlockClass.BB_REDUCIBLE;
        }

        // work the list...
        //
        while (!workList.isEmpty) {
          val x = workList.head
          workList = workList.tail

          // Step e:
          //
          // Step e represents the main difference from Tarjan's method.
          // Chasing upwards from the sources of a node w's backedges. If
          // there is a node y' that is not a descendant of w, w is marked
          // the header of an irreducible loop, there is another entry
          // into this loop that avoids w.
          //

          // The algorithm has degenerated. Break and
          // return in this case.
          //
          val nonBackSize = nonBackPreds(x.dfsNumber).size
          if (nonBackSize > MAXNONBACKPREDS) {
            return 0
          }

          for (iter <- nonBackPreds(x.dfsNumber)) {
            val y = nodes(iter)
            val ydash = y.findSet

            if (!isAncestor(w, ydash.dfsNumber, last)) {
              types(w) = BasicBlockClass.BB_IRREDUCIBLE
              nonBackPreds(w) += ydash.dfsNumber
            } else {
              if (ydash.dfsNumber != w) {
                if (!nodePool.contains(ydash)) {
                  workList = ydash :: workList
                  nodePool = ydash :: nodePool
                }
              }
            }
          }
        }

        // Collapse/Unionize nodes in a SCC to a single node
        // For every SCC found, create a loop descriptor and link it in.
        //
        if ((nodePool.size > 0) || (types(w) == BasicBlockClass.BB_SELF)) {
          var loop = lsg.createNewLoop

          loop.setHeader(nodeW)
          loop.isReducible_=(types(w) != BasicBlockClass.BB_IRREDUCIBLE)

          // At this point, one can set attributes to the loop, such as:
          //
          // the bottom node:
          //    iter  = backPreds(w).begin();
          //    loop bottom is: nodes(iter).node;
          //
          // the number of backedges:
          //    backPreds(w).size()
          //
          // whether this loop is reducible:
          //    types(w) != BB_IRREDUCIBLE
          //
          nodes(w).loop_=(loop)

          for (node <- nodePool) {
            // Add nodes to loop descriptor.
            header(node.dfsNumber) = w
            node.union(nodes(w))

            // Nested loops are not added, but linked together.
            if (node.loop != null) {
              node.loop.setParent(loop)
            } else {
              loop.addNode(node.bb)
            }
          }

          lsg.addLoop(loop)
        }  // nodePool.size
      }  // dead BB
    }  // Step c

    return lsg.getNumLoops
  }  // findLoops
}


//======================================================
// Testing Code
//======================================================

object LoopTesterApp {
  var cfg = new CFG
  var lsg = new LSG

  def buildDiamond(start : Int) : Int = {
    var bb0 = start
    new BasicBlockEdge(cfg, bb0, bb0 + 1)
    new BasicBlockEdge(cfg, bb0, bb0 + 2)
    new BasicBlockEdge(cfg, bb0 + 1, bb0 + 3)
    new BasicBlockEdge(cfg, bb0 + 2, bb0 + 3)
    bb0 + 3
  }

  def buildConnect(start : Int, end : Int) = {
    new BasicBlockEdge(cfg, start, end)
  }

  def buildStraight(start : Int, n : Int) : Int = {
    for (i <- 0 until n) {
      buildConnect(start + i, start + i + 1)
    }
    start + n
  }

  def buildBaseLoop(from : Int ) : Int = {
    var header   = buildStraight(from, 1)
    var diamond1 = buildDiamond(header)
    var d11      = buildStraight(diamond1, 1)
    var diamond2 = buildDiamond(d11)
    var footer   = buildStraight(diamond2, 1)
    buildConnect(diamond2, d11)
    buildConnect(diamond1, header)

    buildConnect(footer, from)
    footer = buildStraight(footer, 1)
    footer
  }

  def main(args: Array[String]) {
    println("Welcome to LoopTesterApp, Scala edition")

    println("Constructing Simple CFG...")

    cfg.createNode(0)  // top
    buildBaseLoop(0)
    cfg.createNode(1)  // bottom
    buildConnect(0, 2)

    // execute loop recognition 15000 times to force compilation
    println("15000 dummy loops")
    for (dummyloop <- 0 until 15000) {
      var finder = new HavlakLoopFinder(cfg, new LSG)
      finder.findLoops
    }

    println("Constructing CFG...")
    var n = 2

    for (parlooptrees <- 0 until 10) {
      cfg.createNode(n + 1)
      buildConnect(2, n + 1)
      n = n + 1
      for (i <- 0 until 100) {
        var top = n
        n = buildStraight(n, 1)
        for (i <- 0 until 25) {
          n = buildBaseLoop(n)
        }
        var bottom = buildStraight(n, 1)
        buildConnect(n, top)
        n = bottom
      }
      buildConnect(n, 1)
    }

    println("Performing Loop Recognition\n1 Iteration")
    var finder = new HavlakLoopFinder(cfg, lsg)
    val loops = finder.findLoops

    println("Another 50 iterations...")

    val start_time = System.nanoTime

    var sum = 0
    for (i <- 0 until 50) {
      print(".")
      var finder = new HavlakLoopFinder(cfg, new LSG)
      sum += finder.findLoops
    }

    println("\nFound " + loops + " loops (including artificial root node) (" + sum + "), " + lsg.calculateNestingLevel + "\n")
    println("time: "+(System.nanoTime-start_time)/1e9+"s")
  }
}
