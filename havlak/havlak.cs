using System;
using System.Collections.Generic;

namespace Test
{
    class Program
    {
        static int loopCounter = 0;

        class BasicBlock
        {
            public List<BasicBlock> InEdges { get; private set; }
            public List<BasicBlock> OutEdges { get; private set; }
            public int Name { get; private set;}

            public BasicBlock(int name)
            {
                Name = name;
                InEdges = new List<BasicBlock>();
                OutEdges = new List<BasicBlock>();
            }
        }

        class BasicBlockEdge
        {
            public BasicBlock From { get; private set; }
            public BasicBlock To { get; private set; }

            public BasicBlockEdge(CFG cfg, int fromName, int toName)
            {
                From = cfg.CreateNode(fromName);
                To = cfg.CreateNode(toName);
                From.OutEdges.Add(To);
                To.InEdges.Add(From);
                cfg.AddEdge(this);
            }
        }

        class CFG
        {
            public Dictionary<int, BasicBlock> BasicBlockMap { get; private set; }
            public BasicBlock StartNode { get; private set; }
            private List<BasicBlockEdge> edgeList = new List<BasicBlockEdge>();

            public CFG()
            {
                BasicBlockMap = new Dictionary<int, BasicBlock>();
                StartNode = null;
            }

            public BasicBlock CreateNode(int name)
            {
                var node = BasicBlockMap.ContainsKey(name) ? BasicBlockMap[name] : null;
                if (node == null)
                {
                    node = new BasicBlock(name);
                    BasicBlockMap[name] = node;
                }

                if (StartNode == null) StartNode = node;
                return node;
            }

            public void AddEdge(BasicBlockEdge edge) { edgeList.Add(edge); }
            public int GetNumNodes() { return BasicBlockMap.Count; }
        }
            
        class SimpleLoop
        {
            Dictionary<BasicBlock,bool> basicBlocks = new Dictionary<BasicBlock, bool>();
            Dictionary<SimpleLoop,bool> children = new Dictionary<SimpleLoop, bool>();
            public bool IsReducible { get; set; }
            public int Counter { get; set; }
            //bool isRoot;
            //int nestingLevel;
            //int depthLevel;
            SimpleLoop parent;
            //BasicBlock header;

            public SimpleLoop()
            {
                parent = null;
                //header = null;
                Counter = 0;
                //depthLevel = 0;
                //nestingLevel = 0;
                //isRoot = false;
                IsReducible = true;
            }

            public void AddNode(BasicBlock bb) { basicBlocks[bb] = true; }
            public void AddChildLoop(SimpleLoop loop) { children[loop] = true; }

            public void SetParent(SimpleLoop p)
            {
                parent = p;
                parent.AddChildLoop(this);
            }

            public void SetHeader(BasicBlock bb)
            {
                basicBlocks[bb] = true;
                //header = bb;
            }

            public void SetNestingLevel(int level)
            {
                //nestingLevel = level;
                //if (level == 0) isRoot = true;
            }
        }

        class LSG
        {
            List<SimpleLoop> loops = new List<SimpleLoop>();
            SimpleLoop root;

            public LSG()
            {
                root = CreateNewLoop();
                root.SetNestingLevel(0);
                AddLoop(root);
            }

            public SimpleLoop CreateNewLoop()
            {
                SimpleLoop s = new SimpleLoop();
                loopCounter += 1;
                s.Counter = loopCounter;
                return s;
            }

            public void AddLoop(SimpleLoop loop) { loops.Add(loop); }
            public int GetNumLoops() { return loops.Count; }
        }

        class UnionFindNode
        {
            public UnionFindNode Parent { get; private set; }
            public BasicBlock BB { get; private set; }
            public SimpleLoop Loop { get; set; }
            public int DfsNumber { get; private set; }

            public UnionFindNode()
            {
                BB = null;
                Parent = null;
                Loop = null;
                DfsNumber = 0;
            }

            public void InitNode(BasicBlock _bb, int dfs)
            {
                Parent = this;
                BB = _bb;
                DfsNumber = dfs;
            }

            public UnionFindNode FindSet()
            {
                var nodeList = new List<UnionFindNode>();
                var node = this;

                while (node != node.Parent)
                {
                    Parent = node.Parent;
                    if (Parent != Parent.Parent) { nodeList.Add(node); }
                    node = Parent;
                }

                foreach(var iter in nodeList) { iter.Parent = node.Parent; }
                return node;
            }

            public void UnionParent(UnionFindNode ufn)
            {
                Parent = ufn;
            }
        }

        class HavlakLoopFinder
        {
            CFG cfg;
            LSG lsg;

            const int UNVISITED = -1;
            const int BB_TOP = 0;
            const int BB_NONHEADER = 1;
            const int BB_REDUCIBLE = 2;
            const int BB_SELF = 3;
            const int BB_IRREDUCIBLE = 4;
            const int BB_DEAD = 5;
            const int BB_LAST = 6;
            const int MAXNONBACKPREDS = (32 * 1024);

            public HavlakLoopFinder(CFG cfg, LSG lsg)
            {
                this.cfg = cfg;
                this.lsg = lsg;
            }

            bool IsAncestor(int w, int v, List<int> last)
            {
                return (w <= v) && (v <= last[w]);
            }

            int DSF(BasicBlock currentNode, List<UnionFindNode> nodes, Dictionary<BasicBlock,int> number, List<int> last, int current)
            {
                nodes[current].InitNode(currentNode, current);
                number[currentNode] = current;
                int lastid = current;

                foreach(var target in currentNode.OutEdges)
                {
                    if (number[target] == UNVISITED)
                    {
                        lastid = DSF(target, nodes, number, last, lastid + 1);
                    }
                }

                last[number[currentNode]] = lastid;
                return lastid;
            }

            public int FindLoops()
            {
                BasicBlock startNode = cfg.StartNode;
                if (startNode == null) return 0;

                int size = cfg.GetNumNodes();

                // init
                var non_back_preds = new List<Dictionary<int, bool>>();
                var back_preds = new List<List<int>>();
                var header = new List<int>();
                var types = new List<int>();
                var last = new List<int>();
                var nodes = new List<UnionFindNode>();
                var number = new Dictionary<BasicBlock, int>();

                for (int i=0; i < size; ++i)
                {
                    non_back_preds.Add(new Dictionary<int, bool>());
                    back_preds.Add(new List<int>());
                    header.Add(0);
                    types.Add(0);
                    last.Add(0);
                    nodes.Add(new UnionFindNode());
                }

                // Step a:
                //  - initialize all nodes as unvisited.
                //  - depth-first traversal and numbering.
                //  - unreached BB's are marked as dead.
                foreach(var pair in cfg.BasicBlockMap)
                {
                    number[pair.Value] = UNVISITED;
                }

                DSF(startNode, nodes, number, last, 0);

                // Step b:
                //  - iterate over all nodes.
                //  A backedge comes from a descendant in the DFS tree, and non-backedges
                //  from non-descendants (following Tarjan).
                //  - check incoming edges 'v' and add them to either
                //    - the list of backedges (backPreds) or
                //    - the list of non-backedges (nonBackPreds)
                for (int w = 0; w < size; ++w)
                {
                    header[w] = 0;
                    types[w] = BB_NONHEADER;

                    var nodeW = nodes[w].BB;

                    if (nodeW != null)
                    {
                        foreach (var nodeV in nodeW.InEdges)
                        {
                            var v = number[nodeV];
                            if (v != UNVISITED)
                            {
                                if (IsAncestor(w, v, last)) back_preds[w].Add(v);
                                else non_back_preds[w][v] = true;
                            }
                        }
                    }
                    else types[w] = BB_DEAD;
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
                for (int w = size-1; w >= 0; --w)
                {
                    // this is 'P' in Havlak's paper
                    var nodePool = new List<UnionFindNode>();

                    var nodeW = nodes[w].BB;
                    if (nodeW != null) // dead BB
                    {
                        // Step d:
                        foreach(var v in back_preds[w])
                        {
                            if (v != w) nodePool.Add(nodes[v].FindSet());
                            else types[w] = BB_SELF;
                        }

                        var workList = new List<UnionFindNode>(nodePool);

                        if (nodePool.Count != 0) types[w] = BB_REDUCIBLE;

                        while (workList.Count > 0)
                        {
                            var x = workList[0];
                            workList.RemoveAt(0);

                            // Step e:
                            // Step e represents the main difference from Tarjan's method.
                            // Chasing upwards from the sources of a node w's backedges. If
                            // there is a node y' that is not a descendant of w, w is marked
                            // the header of an irreducible loop, there is another entry
                            // into this loop that avoids w.
                            // The algorithm has degenerated. Break and
                            // return in this case.
                            var nonBackSize = non_back_preds[x.DfsNumber].Count;
                            if (nonBackSize > MAXNONBACKPREDS) return 0;

                            foreach(var iter in non_back_preds[x.DfsNumber])
                            {
                                var y = nodes[iter.Key];
                                var ydash = y.FindSet();

                                if (!IsAncestor(w, ydash.DfsNumber, last))
                                {
                                    types[w] = BB_IRREDUCIBLE;
                                    int dsfnum = ydash.DfsNumber;
                                    non_back_preds[w][dsfnum] = true;
                                }
                                else
                                {
                                    if (ydash.DfsNumber != w && !nodePool.Contains(ydash))
                                    {
                                        workList.Add(ydash);
                                        nodePool.Add(ydash);
                                    }
                                }
                            }
                        }

                        // Collapse/Unionize nodes in a SCC to a single node
                        // For every SCC found, create a loop descriptor and link it in.
                        if ((nodePool.Count > 0) || (types[w] == BB_SELF)) {
                            var loop = lsg.CreateNewLoop();
                            loop.SetHeader(nodeW);
                            loop.IsReducible = (types[w] != BB_IRREDUCIBLE);

                            nodes[w].Loop = loop;

                            foreach(var node in nodePool)
                            {
                                // Add nodes to loop descriptor.
                                header[node.DfsNumber] = w;
                                node.UnionParent(nodes[w]);

                                if (node.Loop != null) node.Loop.SetParent(loop);
                                else loop.AddNode(node.BB);
                            }

                            lsg.AddLoop(loop);
                        }
                    }
                }

                return lsg.GetNumLoops();
            }
        }

        class LoopTesterApp
        {
            CFG cfg = new CFG();

            int FindHavlakLoops(CFG cfg, LSG lsg)
            {
                var h = new HavlakLoopFinder(cfg, lsg);
                return h.FindLoops();
            }

            int FindHavlakLoops(CFG cfg)
            {
                var lsg = new LSG();
                return FindHavlakLoops(cfg, lsg);
            }

            int BuildDiamond(int start)
            {
                int bb0 = start;
                new BasicBlockEdge(cfg, bb0, bb0 + 1);
                new BasicBlockEdge(cfg, bb0, bb0 + 2);
                new BasicBlockEdge(cfg, bb0 + 1, bb0 + 3);
                new BasicBlockEdge(cfg, bb0 + 2, bb0 + 3);
                return bb0 + 3;
            }

            void BuildConnect(int _start, int _end)
            {
                new BasicBlockEdge(cfg, _start, _end);
            }

            int BuildStraight(int start, int n)
            {
                for (int i = 0; i < n; ++i)
                {
                    BuildConnect(start + i, start + i + 1);
                }
                return start + n;
            }

            int BuildBaseLoop(int from_)
            {
                var header   = BuildStraight(from_, 1);
                var diamond1 = BuildDiamond(header);
                var d11      = BuildStraight(diamond1, 1);
                var diamond2 = BuildDiamond(d11);
                var footer   = BuildStraight(diamond2, 1);

                BuildConnect(diamond2, d11);
                BuildConnect(diamond1, header);

                BuildConnect(footer, from_);
                return BuildStraight(footer, 1);
            }

            void Run() {
                Console.WriteLine("Welcome to LoopTesterApp, C# edition");
                Console.WriteLine("Constructing Simple CFG...");

                cfg.CreateNode(0);
                BuildBaseLoop(0);
                cfg.CreateNode(1);
                BuildConnect(0, 2);

                Console.WriteLine("15000 dummy loops");
                for (int i=0; i < 15000; ++i) FindHavlakLoops(cfg);

                Console.WriteLine("Constructing CFG...");
                int n = 2;

                for (int parlooptrees = 0; parlooptrees < 10; ++parlooptrees)
                {
                    cfg.CreateNode(n + 1);
                    BuildConnect(2, n + 1);
                    n++;

                    for (int i = 0; i < 100; ++i)
                    {
                        int top = n;

                        n = BuildStraight(n, 1);
                        for (int j = 0; j < 25; ++j) n = BuildBaseLoop(n);

                        int bottom = BuildStraight(n, 1);
                        BuildConnect(n, top);
                        n = bottom;
                    }

                    BuildConnect(n, 1);
                }

                Console.WriteLine("Performing Loop Recognition\n1 Iteration");

                int loops = FindHavlakLoops(cfg);

                Console.WriteLine("Another 50 iterations...");

                int sum = 0;
                for (int i = 0; i < 50; ++i)
                {
                    Console.Write(".");
                    sum += FindHavlakLoops(cfg);
                }
                Console.WriteLine();
                Console.WriteLine("Found {0} loops (including artificial root node) ({1})", loops, sum);
            }

            static void Main(string[] args)
            {
                var l = new LoopTesterApp();
                l.Run();
            }
        }
    }
}