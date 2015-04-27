import std.conv;
import std.algorithm: canFind;
import std.array;
import std.stdio;

final:

class BasicBlock {
  BasicBlock[] inEdges;
  BasicBlock[] outEdges;
  int name;

  this(int _name) { name = _name; }
}

struct BasicBlockEdge {
  BasicBlock from;
  BasicBlock to;

  this(CFG cfg, int fromName, int toName) {
    from = cfg.createNode(fromName);
    to = cfg.createNode(toName);
    from.outEdges ~= to;
    to.inEdges ~= from;
    cfg.addEdge(this);
  }
}

class CFG {
  BasicBlock[int] basicBlockMap;
  BasicBlockEdge[] edgeList;
  BasicBlock startNode;

  BasicBlock createNode(int name) {
    BasicBlock node = basicBlockMap.get(name, null);

    if (node is null) {
      node = new BasicBlock(name);
      basicBlockMap[name] = node;
    }

    if (startNode is null) startNode = node;
    return node;
  }

  void addEdge(BasicBlockEdge edge) { edgeList ~= edge; }
  int getNumNodes() { return to!int(basicBlockMap.length); }
}

class SimpleLoop {
  bool[BasicBlock] basicBlocks;
  bool[SimpleLoop] children;
  bool isRoot;
  bool isReducible;
  int counter;
  int nestingLevel;
  int depthLevel;
  SimpleLoop parent;
  BasicBlock header;

  this() {
    parent = null;
    header = null;
    counter = 0;
    depthLevel = 0;
    nestingLevel = 0;
    isRoot = false;
    isReducible = true;
  }

  void addNode(BasicBlock bb) { basicBlocks[bb] = true; }
  void addChildLoop(SimpleLoop loop) { children[loop] = true; }

  void setParent(SimpleLoop p) {
    parent = p;
    parent.addChildLoop(this);
  }

  void setHeader(BasicBlock bb) {
    basicBlocks[bb] = true;
    header = bb;
  }

  void setNestingLevel(int level) {
    nestingLevel = level;
    if (level == 0) isRoot = true;
  }
}

static int loopCounter = 0;

class LSG {
  SimpleLoop[] loops;
  SimpleLoop root;

  this() {
    root = createNewLoop();
    root.setNestingLevel(0);
    addLoop(root);
  }

  SimpleLoop createNewLoop() {
    SimpleLoop s = new SimpleLoop;
    loopCounter += 1;
    s.counter = loopCounter;
    return s;
  }

  void addLoop(SimpleLoop loop) { loops ~= loop; }
  int getNumLoops() { return to!int(loops.length); }
}

class UnionFindNode {
  UnionFindNode parent;
  BasicBlock bb;
  SimpleLoop loop;
  int dfsNumber;

  this() {
    bb = null;
    parent = null;
    loop = null;
    dfsNumber = 0;
  }

  void initNode(BasicBlock _bb, int dfs) {
    parent = this;
    bb = _bb;
    dfsNumber = dfs;
  }

  UnionFindNode findSet() {
    UnionFindNode[] nodeList;
    UnionFindNode node = this;

    while (node != node.parent) {
      parent = node.parent;
      if (parent != parent.parent) { nodeList ~= node; }
      node = parent;
    }

    foreach(iter; nodeList) { iter.parent = node.parent; }
    return node;
  }

  void union_parent(UnionFindNode ufn) {
    parent = ufn;
  }
}

class HavlakLoopFinder {
  CFG cfg;
  LSG lsg;

  enum UNVISITED = -1;
  enum BB_TOP = 0;
  enum BB_NONHEADER = 1;
  enum BB_REDUCIBLE = 2;
  enum BB_SELF = 3;
  enum BB_IRREDUCIBLE = 4;
  enum BB_DEAD = 5;
  enum BB_LAST = 6;
  enum MAXNONBACKPREDS = (32 * 1024);

  this(CFG _cfg, LSG _lsg) {
    cfg = _cfg;
    lsg = _lsg;
  }

  bool isAncestor(int w, int v, int[] last) {
    return (w <= v) && (v <= last[w]);
  }

  int DSF(BasicBlock currentNode, UnionFindNode[] nodes, int[BasicBlock] number, int []last, int current) {
    nodes[current].initNode(currentNode, current);
    number[currentNode] = current;
    int lastid = current;

    foreach(target; currentNode.outEdges) {
      if (number[target] == UNVISITED) {
        lastid = DSF(target, nodes, number, last, lastid + 1);
      }
    }

    last[number[currentNode]] = lastid;
    return lastid;
  }

  int findLoops() {
    BasicBlock startNode = cfg.startNode;
    if (!startNode) return 0;

    int size = cfg.getNumNodes();

    // init
    bool[int][] non_back_preds;
    int[][] back_preds;
    int[] header;
    int[] types;
    int[] last;
    UnionFindNode[] nodes;
    int[BasicBlock] number;

    foreach (_; 0..size) {
      bool[int] newset;
      non_back_preds ~= newset;
      int[] newarr;
      back_preds ~= newarr;
      header ~= 0;
      types ~= 0;
      last ~= 0;
      nodes ~= new UnionFindNode();
    }

    // Step a:
    //  - initialize all nodes as unvisited.
    //  - depth-first traversal and numbering.
    //  - unreached BB's are marked as dead.
    foreach( k, v; cfg.basicBlockMap) { number[v] = UNVISITED; }
    DSF(startNode, nodes, number, last, 0);

    // Step b:
    //  - iterate over all nodes.

    //  A backedge comes from a descendant in the DFS tree, and non-backedges
    //  from non-descendants (following Tarjan).

    //  - check incoming edges 'v' and add them to either
    //    - the list of backedges (backPreds) or
    //    - the list of non-backedges (nonBackPreds)
    foreach (w; 0..size) {
      header[w] = 0;
      types[w] = BB_NONHEADER;

      auto nodeW = nodes[w].bb;

      if (nodeW) {
        foreach (nodeV; nodeW.inEdges) {
          auto v = number[nodeV];
          if (v != UNVISITED) {
            if (isAncestor(w, v, last)) {
              back_preds[w] ~= v;
            } else {
              non_back_preds[w][v] = true;
            }
          }
        }
      } else {
        types[w] = BB_DEAD;
      }
    }

    // Start node is root of all other loops.
    header[0] = 0;

    // Step c:

    // The outer loop, unchanged from Tarjan. It does nothing except
    // for those nodes which are the destinations of backedges.
    // For a header node w, we chase backward from the sources of the
    // backedges adding nodes to the set P, representing the body of
    // the loop headed by w.

    // By running through the nodes in reverse of the DFST preorder,
    // we ensure that inner loop headers will be processed before the
    // headers for surrounding loops.
    foreach_reverse (w; 0 .. size) {
      // this is 'P' in Havlak's paper
      UnionFindNode[] nodePool;

      auto nodeW = nodes[w].bb;
      if (nodeW) { // dead BB
        // Step d:
        foreach(v; back_preds[w]) {
          if (v != w)
            nodePool ~= nodes[v].findSet;
          else
            types[w] = BB_SELF;
        }

        auto workList = nodePool.dup;

        if (nodePool.length != 0) types[w] = BB_REDUCIBLE;

        while (!workList.empty) {
          auto x = workList[0];
          workList.popFront();

          // Step e:

          // Step e represents the main difference from Tarjan's method.
          // Chasing upwards from the sources of a node w's backedges. If
          // there is a node y' that is not a descendant of w, w is marked
          // the header of an irreducible loop, there is another entry
          // into this loop that avoids w.

          // The algorithm has degenerated. Break and
          // return in this case.

          auto nonBackSize = non_back_preds[x.dfsNumber].length;
          if (nonBackSize > MAXNONBACKPREDS) return 0;

          foreach(iter; non_back_preds[x.dfsNumber]) {
            auto y = nodes[iter];
            auto ydash = y.findSet;

            if (!isAncestor(w, ydash.dfsNumber, last)) {
              types[w] = BB_IRREDUCIBLE;
              non_back_preds[w][ydash.dfsNumber] = true;
            } else {
              if (ydash.dfsNumber != w && !nodePool.canFind(ydash)) {
                workList ~= ydash;
                nodePool ~= ydash;
              }
            }
          }
        }

        // Collapse/Unionize nodes in a SCC to a single node
        // For every SCC found, create a loop descriptor and link it in.

        if ((nodePool.length > 0) || (types[w] == BB_SELF)) {
          auto loop = lsg.createNewLoop;
          loop.setHeader(nodeW);
          loop.isReducible = (types[w] != BB_IRREDUCIBLE);

          nodes[w].loop = loop;

          foreach(node; nodePool) {
            // Add nodes to loop descriptor.

            header[node.dfsNumber] = w;
            node.union_parent(nodes[w]);

            if (node.loop) node.loop.setParent(loop); else loop.addNode(node.bb);
          }

          lsg.addLoop(loop);
        }
      }
    }

    return lsg.getNumLoops();
  }
}

int findHavlakLoops(CFG cfg, LSG lsg)
{
  scope h = new HavlakLoopFinder(cfg, lsg);
  return h.findLoops();
}

int findHavlakLoops(CFG cfg)
{
  scope lsg = new LSG();
  return findHavlakLoops(cfg, lsg);
}

class LoopTesterApp {
  CFG cfg;
  LSG lsg;

  this() {
    cfg = new CFG();
    lsg = new LSG();
  }

  int buildDiamond(int start) {
    int bb0 = start;
    BasicBlockEdge(cfg, bb0, bb0 + 1);
    BasicBlockEdge(cfg, bb0, bb0 + 2);
    BasicBlockEdge(cfg, bb0 + 1, bb0 + 3);
    BasicBlockEdge(cfg, bb0 + 2, bb0 + 3);
    return bb0 + 3;
  }

  void buildConnect(int _start, int _end) {
    BasicBlockEdge(cfg, _start, _end);
  }

  int buildStraight(int start, int n) {
    foreach(i; 0..n) {
      buildConnect(start + i, start + i + 1);
    }
    return start + n;
  }

  int buildBaseLoop(int from) {
    auto header   = buildStraight(from, 1);
    auto diamond1 = buildDiamond(header);
    auto d11      = buildStraight(diamond1, 1);
    auto diamond2 = buildDiamond(d11);
    auto footer   = buildStraight(diamond2, 1);
    buildConnect(diamond2, d11);
    buildConnect(diamond1, header);

    buildConnect(footer, from);
    return buildStraight(footer, 1);
  }

  void run() {
    writeln("Welcome to LoopTesterApp, D edition");
    writeln("Constructing Simple CFG...");

    cfg.createNode(0);
    buildBaseLoop(0);
    cfg.createNode(1);
    buildConnect(0, 2);

    writeln("15000 dummy loops");
    foreach(_; 0..15000) {
      findHavlakLoops(cfg);
    }

    writeln("Constructing CFG...");
    int n = 2;

    foreach(parlooptrees; 0..10) {
      cfg.createNode(n + 1);
      buildConnect(2, n + 1);
      n = n + 1;

      foreach(i; 0..100) {
        int top = n;

        n = buildStraight(n, 1);
        foreach(_; 0..25) { n = buildBaseLoop(n); }

        int bottom = buildStraight(n, 1);
        buildConnect(n, top);
        n = bottom;
      }

      buildConnect(n, 1);
    }

    writeln("Performing Loop Recognition\n1 Iteration");

    int loops = findHavlakLoops(cfg);

    writeln("Another 50 iterations...");

    int sum = 0;
    foreach (_; 0..50) {
      write(".");
      stdout.flush();
      sum += findHavlakLoops(cfg);
    }
    writefln("\nFound %d loops (including artificial root node) (%d)", loops, sum);
  }
}

int main() {
  auto l = new LoopTesterApp();
  l.run();
  return 0;
}
