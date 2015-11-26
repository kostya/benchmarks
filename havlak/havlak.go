package main

import "container/list"
import "fmt"

type BasicBlock struct {
	Name     int
	InEdges  []*BasicBlock
	OutEdges []*BasicBlock
}

func NewBasicBlock(name int) *BasicBlock {
	return &BasicBlock{Name: name}
}

func (bb *BasicBlock) Dump() {
	fmt.Printf("BB#%03d: ", bb.Name)
	if len(bb.InEdges) > 0 {
		fmt.Printf("in : ")
		for _, iter := range bb.InEdges {
			fmt.Printf("BB#%03d ", iter.Name)
		}
	}
	if len(bb.OutEdges) > 0 {
		fmt.Print("out: ")
		for _, iter := range bb.OutEdges {
			fmt.Printf("BB#%03d ", iter.Name)
		}
	}
	fmt.Printf("\n")
}

//-----------------------------------------------------------

type CFG struct {
	Bb        map[int]*BasicBlock
	StartNode *BasicBlock
}

func NewCFG() *CFG {
	return &CFG{Bb: make(map[int]*BasicBlock)}
}

func (cfg *CFG) CreateNode(node int) *BasicBlock {
	if bblock := cfg.Bb[node]; bblock != nil {
		return bblock
	}
	bblock := NewBasicBlock(node)
	cfg.Bb[node] = bblock

	if len(cfg.Bb) == 1 {
		cfg.StartNode = bblock
	}

	return bblock
}

func (cfg *CFG) Dump() {
	for _, n := range cfg.Bb {
		n.Dump()
	}
}

//-----------------------------------------------------------

type BasicBlockEdge struct {
	To   *BasicBlock
	From *BasicBlock
}

func NewBasicBlockEdge(cfg *CFG, from int, to int) *BasicBlockEdge {
	self := new(BasicBlockEdge)
	self.To = cfg.CreateNode(to)
	self.From = cfg.CreateNode(from)

	self.From.OutEdges = append(self.From.OutEdges,self.To)
	self.To.InEdges = append(self.To.InEdges, self.From)

	return self
}

//======================================================
// Scaffold Code
//======================================================

// Basic representation of loops, a loop has an entry point,
// one or more exit edges, a set of basic blocks, and potentially
// an outer loop - a "parent" loop.
//
// Furthermore, it can have any set of properties, e.g.,
// it can be an irreducible loop, have control flow, be
// a candidate for transformations, and what not.
//
type SimpleLoop struct {
	// No set, use map to bool
	basicBlocks map[*BasicBlock]bool
	children    map[*SimpleLoop]bool
	Parent      *SimpleLoop
	header      *BasicBlock

	IsRoot       bool
	IsReducible  bool
	Counter      int
	NestingLevel int
	DepthLevel   int
}

func (loop *SimpleLoop) AddNode(bb *BasicBlock) {
	loop.basicBlocks[bb] = true
}

func (loop *SimpleLoop) AddChildLoop(child *SimpleLoop) {
	loop.children[child] = true
}

func (loop *SimpleLoop) Dump(indent int) {
	for i := 0; i < indent; i++ {
		fmt.Printf("  ")
	}

	// No ? operator ?
	fmt.Printf("loop-%d nest: %d depth %d ",
		loop.Counter, loop.NestingLevel, loop.DepthLevel)
	if !loop.IsReducible {
		fmt.Printf("(Irreducible) ")
	}

	// must have > 0
	if len(loop.children) > 0 {
		fmt.Printf("Children: ")
		for ll, _ := range loop.children {
			fmt.Printf("loop-%d", ll.Counter)
		}
	}
	if len(loop.basicBlocks) > 0 {
		fmt.Printf("(")
		for bb, _ := range loop.basicBlocks {
			fmt.Printf("BB#%03d ", bb.Name)
			if loop.header == bb {
				fmt.Printf("*")
			}
		}
		fmt.Printf("\b)")
	}
	fmt.Printf("\n")
}

func (loop *SimpleLoop) SetHeader(bb *BasicBlock) {
	loop.AddNode(bb)
	loop.header = bb
}

//------------------------------------
// Helper (No templates or such)
//
func max(x, y int) int {
	if x > y {
		return x
	}
	return y
}

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
var loopCounter = 0

type LSG struct {
	Root  *SimpleLoop
	Loops []*SimpleLoop
}

func NewLSG() *LSG {
	lsg := new(LSG)
	root := lsg.NewLoop()
	root.NestingLevel = 0

	return lsg
}

func (lsg *LSG) NewLoop() *SimpleLoop {
	loop := new(SimpleLoop)
	loop.basicBlocks = make(map[*BasicBlock]bool)
	loop.children = make(map[*SimpleLoop]bool)
	loop.Parent = nil
	loop.header = nil

	loop.Counter = loopCounter
	loopCounter++
	return loop
}

func (lsg *LSG) Dump() {
	lsg.dump(lsg.Root, 0)
}

func (lsg *LSG) dump(loop *SimpleLoop, indent int) {
	loop.Dump(indent)

	for ll, _ := range loop.children {
		lsg.dump(ll, indent+1)
	}
}

func (lsg *LSG) CalculateNestingLevel() {
	for _, ll := range lsg.Loops {
		if ll.IsRoot {
			continue
		}
		if ll.Parent == nil {
			ll.Parent = lsg.Root
			ll.Parent.AddChildLoop(ll)
		}
	}
	lsg.calculateNestingLevel(lsg.Root, 0)
}

func (lsg *LSG) calculateNestingLevel(loop *SimpleLoop, depth int) {
	loop.DepthLevel = depth
	for ll, _ := range loop.children {
		lsg.calculateNestingLevel(ll, depth+1)

		ll.NestingLevel = max(loop.NestingLevel, ll.NestingLevel+1)
	}
}

// Basic Blocks and Loops are being classified as regular, irreducible,
// and so on. This enum contains a symbolic name for all these classifications
//
const (
	_             = iota // Go has an interesting ioate concept
	bbTop                // uninitialized
	bbNonHeader          // a regular BB
	bbReducible          // reducible loop
	bbSelf               // single BB loop
	bbIrreducible        // irreducible loop
	bbDead               // a dead BB
	bbLast               // sentinel
)

// UnionFindNode is used in the Union/Find algorithm to collapse
// complete loops into a single node. These nodes and the
// corresponding functionality are implemented with this class
//
type UnionFindNode struct {
	Parent    *UnionFindNode
	Bb        *BasicBlock
	Loop      *SimpleLoop
	DfsNumber int
}

// Init explicitly initializes UnionFind nodes.
//
func (u *UnionFindNode) Init(bb *BasicBlock, dfsNumber int) {
	u.Parent = u
	u.Bb = bb
	u.DfsNumber = dfsNumber
	u.Loop = nil
}

// FindSet implements the Find part of the Union/Find Algorithm
//
// Implemented with Path Compression (inner loops are only
// visited and collapsed once, however, deep nests would still
// result in significant traversals).
//
func (u *UnionFindNode) FindSet() *UnionFindNode {
	nodeList := []*UnionFindNode{}
	node := u

	var p *UnionFindNode
	for ; node != node.Parent; node = node.Parent {
		p = node.Parent
		if p != p.Parent {
			nodeList = append(nodeList, node)
		}

	}

	// Path Compression, all nodes' parents point to the 1st level parent.
	for _, ll := range nodeList {
		ll.Parent = node.Parent
	}

	return node
}

// Union relies on path compression.
//
func (u *UnionFindNode) Union(B *UnionFindNode) {
	u.Parent = B
}

// Constants
//
// Marker for uninitialized nodes.
const unvisited = -1

// Safeguard against pathological algorithm behavior.
const maxNonBackPreds = 32 * 1024

// IsAncestor
//
// As described in the paper, determine whether a node 'w' is a
// "true" ancestor for node 'v'.
//
// Dominance can be tested quickly using a pre-order trick
// for depth-first spanning trees. This is why DFS is the first
// thing we run below.
//
// Go comment: Parameters can be written as w,v int, inlike in C, where
//   each parameter needs its own type.
//
func isAncestor(w, v int, last []int) bool {
	return ((w <= v) && (v <= last[w]))
}

// listContainsNode
//
// Check whether a list contains a specific element.
//
// Go comment: moving the assignment of el into the if
//    provided for improved scoping!
//
func listContainsNode(l []*UnionFindNode, u *UnionFindNode) bool {
	for _, el := range l {
		if el == u {
			return true
		}
	}
	return false
}

// DFS - Depth-First-Search and node numbering.
//
func DFS(currentNode *BasicBlock, nodes []*UnionFindNode, number map[*BasicBlock]int, last []int, current int) int {
	nodes[current].Init(currentNode, current)
	number[currentNode] = current

	lastid := current
	for _, ll := range currentNode.OutEdges {
		if number[ll] == unvisited {
			lastid = DFS(ll, nodes, number, last, lastid+1)
		}
	}
	last[number[currentNode]] = lastid
	return lastid
}

// FindLoops
//
// Find loops and build loop forest using Havlak's algorithm, which
// is derived from Tarjan. Variable names and step numbering has
// been chosen to be identical to the nomenclature in Havlak's
// paper (which, in turn, is similar to the one used by Tarjan).
//
func FindLoops(cfgraph *CFG, lsgraph *LSG) {
	if cfgraph.StartNode == nil {
		return
	}

	size := len(cfgraph.Bb)

	nonBackPreds := make([]map[int]bool, size)
	backPreds := make([]list.List, size)

	number := make(map[*BasicBlock]int)
	header := make([]int, size, size)
	types := make([]int, size, size)
	last := make([]int, size, size)
	nodes := make([]*UnionFindNode, size, size)

	for i := 0; i < size; i++ {
		nodes[i] = new(UnionFindNode)
	}

	// Step a:
	//   - initialize all nodes as unvisited.
	//   - depth-first traversal and numbering.
	//   - unreached BB's are marked as dead.
	//
	for i, bb := range cfgraph.Bb {
		number[bb] = unvisited
		nonBackPreds[i] = make(map[int]bool)
	}

	DFS(cfgraph.StartNode, nodes, number, last, 0)

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
	for w := 0; w < size; w++ {
		header[w] = 0
		types[w] = bbNonHeader

		nodeW := nodes[w].Bb
		if nodeW == nil {
			types[w] = bbDead
			continue // dead BB
		}


		for _, nodeV := range nodeW.InEdges {

			v := number[nodeV]
			if v == unvisited {
				continue // dead node
			}

			if isAncestor(w, v, last) {
				backPreds[w].PushBack(v)
			} else {
				nonBackPreds[w][v] = true
			}
		}
	}

	// Start node is root of all other loops.
	header[0] = 0

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
	for w := size - 1; w >= 0; w-- {
		// this is 'P' in Havlak's paper
		nodePool := []*UnionFindNode{}

		nodeW := nodes[w].Bb
		if nodeW == nil {
			continue // dead BB
		}

		// Step d:
		for ll := backPreds[w].Front(); ll != nil; ll = ll.Next() {
			v := ll.Value.(int)
			if v != w {
				nodePool = append(nodePool, nodes[v].FindSet())
			} else {
				types[w] = bbSelf
			}
		}

		// Copy nodePool to workList.
		//
		workList := []*UnionFindNode{}
		for _, v := range nodePool {
			// workaround for gccgo problem, suggested by Ian
			workList = append(workList, v)
		}

		if len(nodePool) != 0 {
			types[w] = bbReducible
		}

		// work the list...
		//
		for len(workList) > 0 {
			x := workList[0]
			workList = workList[1:]

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
			nonBackSize := len(nonBackPreds[x.DfsNumber])
			if nonBackSize > maxNonBackPreds {
				return
			}

			for iter := range nonBackPreds[x.DfsNumber] {
				y := nodes[iter]
				ydash := y.FindSet()

				if !isAncestor(w, ydash.DfsNumber, last) {
					types[w] = bbIrreducible
					nonBackPreds[w][ydash.DfsNumber] = true
				} else {
					if ydash.DfsNumber != w {
						if !listContainsNode(nodePool, ydash) {
							workList = append(workList, ydash)
							nodePool = append(workList, ydash)
						}
					}
				}
			}
		}

		// Collapse/Unionize nodes in a SCC to a single node
		// For every SCC found, create a loop descriptor and link it in.
		//
		if (len(nodePool) > 0) || (types[w] == bbSelf) {
			loop := lsgraph.NewLoop()

			loop.SetHeader(nodeW)
			loop.IsReducible = types[w] != bbIrreducible

			// At this point, one can set attributes to the loop, such as:
			//
			// the bottom node:
			//    iter  = backPreds[w].begin();
			//    loop bottom is: nodes[iter].node);
			//
			// the number of backedges:
			//    backPreds[w].size()
			//
			// whether this loop is reducible:
			//    type[w] != BasicBlockClass.bbIrreducible
			//
			nodes[w].Loop = loop

			for _, node := range nodePool {

				// Add nodes to loop descriptor.
				header[node.DfsNumber] = w
				node.Union(nodes[w])

				// Nested loops are not added, but linked together.
				if node.Loop != nil {
					node.Loop.Parent = loop
					node.Loop.Parent.AddChildLoop(node.Loop)
				} else {
					loop.AddNode(node.Bb)
				}
			}

			lsgraph.Loops = append(lsgraph.Loops, loop)
		} // nodePool.size
	} // Step c

}

// External entry point.
func FindHavlakLoops(cfgraph *CFG, lsgraph *LSG) int {
	FindLoops(cfgraph, lsgraph)
	return len(lsgraph.Loops)
}

//======================================================
// Testing Code
//======================================================

func buildDiamond(cfgraph *CFG, start int) int {
	bb0 := start
	NewBasicBlockEdge(cfgraph, bb0, bb0+1)
	NewBasicBlockEdge(cfgraph, bb0, bb0+2)
	NewBasicBlockEdge(cfgraph, bb0+1, bb0+3)
	NewBasicBlockEdge(cfgraph, bb0+2, bb0+3)

	return bb0 + 3
}

func buildConnect(cfgraph *CFG, start int, end int) {
	NewBasicBlockEdge(cfgraph, start, end)
}

func buildStraight(cfgraph *CFG, start int, n int) int {
	for i := 0; i < n; i++ {
		buildConnect(cfgraph, start+i, start+i+1)
	}
	return start + n
}

func buildBaseLoop(cfgraph *CFG, from int) int {
	header := buildStraight(cfgraph, from, 1)
	diamond1 := buildDiamond(cfgraph, header)
	d11 := buildStraight(cfgraph, diamond1, 1)
	diamond2 := buildDiamond(cfgraph, d11)
	footer := buildStraight(cfgraph, diamond2, 1)
	buildConnect(cfgraph, diamond2, d11)
	buildConnect(cfgraph, diamond1, header)

	buildConnect(cfgraph, footer, from)
	footer = buildStraight(cfgraph, footer, 1)
	return footer
}

func main() {
	fmt.Printf("Welcome to LoopTesterApp, Go edition\n")

	lsgraph := NewLSG()
	cfgraph := NewCFG()

	fmt.Printf("Constructing Simple CFG...\n")

	cfgraph.CreateNode(0) // top
	buildBaseLoop(cfgraph, 0)
	cfgraph.CreateNode(1) // bottom
	NewBasicBlockEdge(cfgraph, 0, 2)

	fmt.Printf("15000 dummy loops\n")
	for dummyloop := 0; dummyloop < 15000; dummyloop++ {
		FindHavlakLoops(cfgraph, NewLSG())
	}

	fmt.Printf("Constructing CFG...\n")
	n := 2

	for parlooptrees := 0; parlooptrees < 10; parlooptrees++ {
		cfgraph.CreateNode(n + 1)
		buildConnect(cfgraph, 2, n+1)
		n = n + 1

		for i := 0; i < 100; i++ {
			top := n
			n = buildStraight(cfgraph, n, 1)
			for j := 0; j < 25; j++ {
				n = buildBaseLoop(cfgraph, n)
			}
			bottom := buildStraight(cfgraph, n, 1)
			buildConnect(cfgraph, n, top)
			n = bottom
		}
		buildConnect(cfgraph, n, 1)
	}

	fmt.Printf("Performing Loop Recognition\n1 Iteration\n")
	FindHavlakLoops(cfgraph, lsgraph)

	fmt.Printf("Another 50 iterations...\n")
	s := 0
	for i := 0; i < 50; i++ {
		fmt.Printf(".")
		s += FindHavlakLoops(cfgraph, NewLSG())
	}

	fmt.Printf("\nFound %d loops (including artificial root node) (%d)\n", len(lsgraph.Loops), s)
}
