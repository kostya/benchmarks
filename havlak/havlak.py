# Copyright 2011 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

import sys

#======================================================
# Scaffold Code
#======================================================

# BasicBlock's static members
#
numBasicBlocks = 0
def getNumBasicBlocks():
  return numBasicBlocks

#
# class BasicBlock
#
# BasicBlock only maintains a vector of in-edges and
# a vector of out-edges.
#
class BasicBlock:
  def __init__(self, name):
    global numBasicBlocks

    self.name = name
    self.inEdges  = []
    self.outEdges = []

    numBasicBlocks = numBasicBlocks + 1

  def toString(self): return "BB#" + self.name
  def getNumPred(self): return len(self.inEdges)
  def getNumSucc(self): return len(self.outEdges)
  def addInEdge(self, bb): self.inEdges.append(bb)
  def addOutEdge(self, bb): self.outEdges.append(bb)

  def dump(self):
    res = "BB#" + str(self.name) + " "
    if len(self.inEdges) > 0:
      res += "\tin :"
      for edge in self.inEdges:
        res += " " + str(edge.name)
    if len(self.outEdges) > 0:
      res += "\tout:"
      for edge in self.outEdges:
        res += " " + str(edge.name)

    print res

#
# class CFG
#
# CFG maintains a list of nodes, plus a start node.
# That's it.
#
class CFG:
  def __init__(self):
    self.startNode     = None
    self.basicBlockMap = {}
    self.edgeList      = []

  def createNode(self, name):
    node = None
    if name in self.basicBlockMap:
      node = self.basicBlockMap[name]
    else:
      node = BasicBlock(name)
      self.basicBlockMap[name] = node

    if self.getNumNodes() == 1:
      self.startNode = node
    return node

  def dump(self):
    for (k,v) in self.basicBlockMap.items():
      v.dump()

  def addEdge(self, edge): self.edgeList.append(edge)
  def getNumNodes(self): return len(self.basicBlockMap)
  def getDst(edge):  return edge.to_
  def getSrc(edge):  return edge.from_

#
# class BasicBlockEdge
#
# These data structures are stubbed out to make the code below easier
# to review.
#
# BasicBlockEdge only maintains two pointers to BasicBlocks.
# Note: from is apparently a keyword in python. Changed to uppercase
#
class BasicBlockEdge:
    def __init__(self, cfg, fromName, toName):
      self.From = cfg.createNode(fromName)
      self.To   = cfg.createNode(toName)

      self.From.addOutEdge(self.To)
      self.To.addInEdge(self.From)

      cfg.addEdge(self)

#
# class SimpleLoop
#
# Basic representation of loops, a loop has an entry point,
# one or more exit edges, a set of basic blocks, and potentially
# an outer loop - a "parent" loop.
#
# Furthermore, it can have any set of properties, e.g.,
# it can be an irreducible loop, have control flow, be
# a candidate for transformations, and what not.
#
class SimpleLoop:
  def __init__(self):
      self.basicBlocks  = []
      self.children     = []
      self.parent       = None
      self.header       = None

      self.isRoot       = False
      self.isReducible  = True
      self.counter      = 0
      self.nestingLevel = 0
      self.depthLevel   = 0

  def addNode(self, bb):
    self.basicBlocks.append(bb)

  def addChildLoop(self, loop):
    self.children.append(loop)

  def dump(self, indent):
    for i in range(indent):
      print "  ",

    print "loop-%d nest: %d depth %d" % (
                      self.counter,
                      self.nestingLevel,
                      self.depthLevel),
    if self.isReducible == True:
      print ""
    else:
      print "irreducible"


  def setParent(self, parent):
    self.parent = parent
    parent.addChildLoop(self)

  def setHeader(self, bb):
    self.basicBlocks.append(bb)
    self.header = bb

  def setNestingLevel(self, level):
    self.nestingLevel = level
    if level == 0:
      self.isRoot = True


#
# LoopStructureGraph
#
# Maintain loop structure for a given CFG.
#
# Two values are maintained for this loop graph, depth, and nesting level.
# For example:
#
# loop        nesting level    depth
#----------------------------------------
# loop-0      2                0
#   loop-1    1                1
#   loop-3    1                1
#     loop-2  0                2
#
loopCounter = 0

class LSG:
  def __init__(self):
    global loopCounter
    self.loops = []
    self.root  = SimpleLoop()
    self.root.setNestingLevel(0)
    self.root.counter = loopCounter
    loopCounter += 1
    self.addLoop(self.root)

  def createNewLoop(self):
    global loopCounter
    loop = SimpleLoop()
    loop.counter = loopCounter
    loopCounter += 1
    return loop

  def addLoop(self, loop): self.loops.append(loop)

  def dumpRec(self, loop, indent):
    loop.dump(indent)

    for liter in loop.children:
      self.dumpRec(liter, indent + 1)

  def dump(self): self.dumpRec(self.root,0)

  def calculateNestingLevel(self):
    for liter in self.loops:
      if not liter.isRoot:
        if liter.parent == None:
          liter.setParent(self.root)

    self.calculateNestingLevelRec(self.root, 0)

  def calculateNestingLevelRec(self, loop, depth):
    loop.depthLevel = depth
    for liter in loop.children:
      self.calculateNestingLevelRec(liter, depth+1)

      loop.setNestingLevel(max(loop.nestingLevel,
                               1+liter.nestingLevel))

  def getNumLoops(self): return len(self.loops)



#======================================================
# Main Algorithm
#======================================================

#
# class UnionFindNode
#
# The algorithm uses the Union/Find algorithm to collapse
# complete loops into a single node. These nodes and the
# corresponding functionality are implemented with this class
#
class UnionFindNode:
  def __init__(self):
    self.parent    = None
    self.bb        = None
    self.loop      = None
    self.dfsNumber = 0

  # Initialize this node.
  #
  def initNode(self, bb, dfsNumber):
    self.parent     = self
    self.bb         = bb
    self.dfsNumber  = dfsNumber
    self.loop       = None

  # Union/Find Algorithm - The find routine.
  #
  # Implemented with Path Compression (inner loops are only
  # visited and collapsed once, however, deep nests would still
  # result in significant traversals).
  #
  def findSet(self):
    nodeList = []

    node = self
    while node != node.parent:
      if node.parent != node.parent.parent:
        nodeList.append(node)

      node = node.parent

    # Path Compression, all nodes' parents point to the 1st level parent.
    for iter in nodeList:
      iter.parent = node.parent

    return node

  # Union/Find Algorithm - The union routine.
  #
  # Trivial. Assigning parent pointer is enough,
  # we rely on path compression.
  #
  def union(self, unionFindNode):
    self.parent = unionFindNode


class HavlakLoopFinder:
  def __init__(self, cfgParm, lsgParm):
      self.cfg = cfgParm
      self.lsg = lsgParm

      #
      # enum BasicBlockClass
      #
      # Basic Blocks and Loops are being classified as regular, irreducible,
      # and so on. This enum contains a symbolic name for all these
      # classifications. Python doesn't have enums, so we just create values.
      #
      self.BB_TOP          = 0 # uninitialized
      self.BB_NONHEADER    = 1 # a regular BB
      self.BB_REDUCIBLE    = 2 # reducible loop
      self.BB_SELF         = 3 # single BB loop
      self.BB_IRREDUCIBLE  = 4 # irreducible loop
      self.BB_DEAD         = 5 # a dead BB
      self.BB_LAST         = 6 # Sentinel

      #
      # Constants
      #
      # Marker for uninitialized nodes.
      self.UNVISITED = -1

      # Safeguard against pathologic algorithm behavior.
      self.MAXNONBACKPREDS = (32 * 1024)

  #
  # IsAncestor
  #
  # As described in the paper, determine whether a node 'w' is a
  # "true" ancestor for node 'v'.
  #
  # Dominance can be tested quickly using a pre-order trick
  # for depth-first spanning trees. This is why DFS is the first
  # thing we run below.
  #
  def isAncestor(self, w, v, last):
    return w <= v and v <= last[w]

  #
  # DFS - Depth-First-Search
  #
  # DESCRIPTION:
  # Simple depth first traversal along out edges with node numbering.
  #
  def DFS(self, currentNode, nodes, number, last, current):
    nodes[current].initNode(currentNode, current)
    number[currentNode] = current

    lastid = current
    for target in currentNode.outEdges:
      if number[target] == self.UNVISITED:
        lastid = self.DFS(target, nodes, number, last, lastid + 1)

    last[number[currentNode]] = lastid
    return lastid

  #
  # findLoops
  #
  # Find loops and build loop forest using Havlak's algorithm, which
  # is derived from Tarjan. Variable names and step numbering has
  # been chosen to be identical to the nomenclature in Havlak's
  # paper (which, in turn, is similar to the one used by Tarjan).
  #
  def findLoops(self):
    if self.cfg.startNode == None:
      return 0

    size = self.cfg.getNumNodes()

    nonBackPreds    = []
    backPreds       = []
    number          = {}
    header          = []
    types           = []
    last            = []
    nodes           = []

    for i in range(size):
      nonBackPreds.append(set())
      backPreds.append(list())
      header.append(0)
      types.append(0)
      last.append(0)
      nodes.append(UnionFindNode())

    # Step a:
    #   - initialize all nodes as unvisited.
    #   - depth-first traversal and numbering.
    #   - unreached BB's are marked as dead.
    #
    for k, v in self.cfg.basicBlockMap.items():
      number[v] = self.UNVISITED

    self.DFS(cfg.startNode, nodes, number, last, 0)

    # Step b:
    #   - iterate over all nodes.
    #
    #   A backedge comes from a descendant in the DFS tree, and non-backedges
    #   from non-descendants (following Tarjan).
    #
    #   - check incoming edges 'v' and add them to either
    #     - the list of backedges (backPreds) or
    #     - the list of non-backedges (nonBackPreds)
    #
    for w in range(size):
      header[w] = 0
      types[w]  = self.BB_NONHEADER

      nodeW = nodes[w].bb
      if nodeW == None:
        types[w] = self.BB_DEAD
      else:
        if nodeW.getNumPred() > 0:
          for nodeV in nodeW.inEdges:
            v = number[nodeV]
            if v != self.UNVISITED:
              if self.isAncestor(w, v, last):
                backPreds[w].append(v)
              else:
                nonBackPreds[w].add(v)

    # Start node is root of all other loops.
    header[0] = 0

    # Step c:
    #
    # The outer loop, unchanged from Tarjan. It does nothing except
    # for those nodes which are the destinations of backedges.
    # For a header node w, we chase backward from the sources of the
    # backedges adding nodes to the set P, representing the body of
    # the loop headed by w.
    #
    # By running through the nodes in reverse of the DFST preorder,
    # we ensure that inner loop headers will be processed before the
    # headers for surrounding loops.
    #
    for w in range(size-1, -1, -1):
      # this is 'P' in Havlak's paper
      nodePool = []

      nodeW = nodes[w].bb
      if nodeW != None: # dead BB

        # Step d:
        for v in backPreds[w]:
          if v != w:
            nodePool.append(nodes[v].findSet())
          else:
            types[w] = self.BB_SELF

        # Copy nodePool to workList.
        #
        workList = []
        for n in nodePool:
          workList.append(n)

        if len(nodePool) != 0:
          types[w] = self.BB_REDUCIBLE;

        # work the list...
        #
        while workList:
          x = workList.pop(0)

          # Step e:
          #
          # Step e represents the main difference from Tarjan's method.
          # Chasing upwards from the sources of a node w's backedges. If
          # there is a node y' that is not a descendant of w, w is marked
          # the header of an irreducible loop, there is another entry
          # into this loop that avoids w.
          #

          # The algorithm has degenerated. Break and
          # return in this case.
          #
          nonBackSize = len(nonBackPreds[x.dfsNumber])
          if nonBackSize > self.MAXNONBACKPREDS:
            return 0

          for iter in nonBackPreds[x.dfsNumber]:
            y = nodes[iter]
            ydash = y.findSet()

            if not self.isAncestor(w, ydash.dfsNumber, last):
              types[w] = self.BB_IRREDUCIBLE
              nonBackPreds[w].add(ydash.dfsNumber)
            else:
              if ydash.dfsNumber != w:
                if ydash not in nodePool:
                  workList.append(ydash)
                  nodePool.append(ydash)

        # Collapse/Unionize nodes in a SCC to a single node
        # For every SCC found, create a loop descriptor and link it in.
        #
        if (len(nodePool) > 0) or (types[w] == self.BB_SELF):
          loop = self.lsg.createNewLoop()

          loop.setHeader(nodeW)
          if types[w] != self.BB_IRREDUCIBLE:
            loop.isReducible = True
          else:
            loop.isReducible = False

          # At this point, one can set attributes to the loop, such as:
          #
          # the bottom node:
          #    iter  = backPreds(w).begin();
          #    loop bottom is: nodes(iter).node;
          #
          # the number of backedges:
          #    backPreds(w).size()
          #
          # whether this loop is reducible:
          #    types(w) != BB_IRREDUCIBLE
          #
          nodes[w].loop = loop

          for node in nodePool:
            # Add nodes to loop descriptor.
            header[node.dfsNumber] = w
            node.union(nodes[w])

            # Nested loops are not added, but linked together.
            if node.loop != None:
              node.loop.setParent(loop)
            else:
              loop.addNode(node.bb)

          self.lsg.addLoop(loop)

    return self.lsg.getNumLoops()


#======================================================
# Testing Code
#======================================================

def buildDiamond(start):
  global cfg
  bb0 = start
  BasicBlockEdge(cfg, bb0, bb0 + 1)
  BasicBlockEdge(cfg, bb0, bb0 + 2)
  BasicBlockEdge(cfg, bb0 + 1, bb0 + 3)
  BasicBlockEdge(cfg, bb0 + 2, bb0 + 3)
  return bb0 + 3


def buildConnect(start, end):
  global cfg
  BasicBlockEdge(cfg, start, end)

def buildStraight(start, n):
  global cfg
  for i in range(n):
    buildConnect(start + i, start + i + 1)
  return start + n

def buildBaseLoop(From):
  global cfg
  header   = buildStraight(From, 1)
  diamond1 = buildDiamond(header)
  d11      = buildStraight(diamond1, 1)
  diamond2 = buildDiamond(d11)
  footer   = buildStraight(diamond2, 1)
  buildConnect(diamond2, d11)
  buildConnect(diamond1, header)

  buildConnect(footer, From)
  footer = buildStraight(footer, 1)
  return  footer

cfg = CFG()
lsg = LSG()

print "Welcome to LoopTesterApp, Python edition"

print "Constructing Simple CFG..."
sys.setrecursionlimit(100000)

cfg.createNode(0)  # top
cfg.createNode(1)  #s bottom
buildBaseLoop(0)
buildConnect(0, 2)

# execute loop recognition 15000 times to force compilation
print "15000 dummy loops"
for dummyloop in range(15000):
  lsglocal = LSG()
  finder = HavlakLoopFinder(cfg, lsglocal)
  x = finder.findLoops()
  del lsglocal

print "Constructing CFG..."
n = 2

for parlooptrees in range(10):
  cfg.createNode(n + 1)
  buildConnect(2, n + 1)
  n = n + 1
  for i in range(100):
    top = n
    n = buildStraight(n, 1)
    for j in range(25):
      n = buildBaseLoop(n)

    bottom = buildStraight(n, 1)
    buildConnect(n, top)
    n = bottom

  buildConnect(n, 1)

print "Performing Loop Recognition\n1 Iteration"
finder = HavlakLoopFinder(cfg, lsg)
loops = finder.findLoops()

print "Another 50 iterations..."

sum = 0
for i in range(50):
  sys.stdout.write('.')
  sys.stdout.flush()
  sum += HavlakLoopFinder(cfg, LSG()).findLoops()

print "\nFound " + str(loops) + " loops (including artificial root node) (" + str(sum) + ")\n"
