package main

import (
	"benchmark"
	"sort"
	"strconv"
)

// setPrimes sets the prime numbers in the sieve using the sieve of Atkin.
func setPrimes(sieve []bool) {
	// Start with the knowledge that 2 and 3 are the first primes
	copy(sieve, []bool{false, false, true, true})

	limit := len(sieve) - 1

	for x := 1; x*x < limit; x++ {
		for y := 1; y*y < limit; y++ {
			// step 1: first quadratic using m = 12 and r in R1 = {r : 1, 5}
			n := (4 * x * x) + (y * y)
			if n <= limit && (n%12 == 1 || n%12 == 5) {
				sieve[n] = !sieve[n]
			}

			// step 2: second quadratic using m = 12 and r in R2 = {r : 7}
			n = (3 * x * x) + (y * y)
			if n <= limit && n%12 == 7 {
				sieve[n] = !sieve[n]
			}

			// step 3: third quadratic using m = 12 and r in R3 = {r : 11}
			n = (3 * x * x) - (y * y)
			if x > y && n <= limit && n%12 == 11 {
				sieve[n] = !sieve[n]
			}
		}
	}

	// step 4: eliminate squares of primes (and their multiples)
	for n := 5; n*n < limit; n++ {
		if sieve[n] {
			for k := n * n; k < limit; k += n * n {
				sieve[k] = false
			}
		}
	}
}

// trieNode is a trie for bytes
type trieNode struct {
	terminal bool
	children map[byte]*trieNode
}

// values returns the values of all terminal nodes in the trie.
func (t *trieNode) values(prefix string) []string {
	var result []string
	if t.terminal {
		result = append(result, prefix)
	}
	for i, child := range t.children {
		if child == nil {
			continue
		}
		result = append(result, child.values(prefix+string(byte(i)))...)
	}
	return result
}

func (t *trieNode) findNode(val string) *trieNode {
	node := t
	for i := 0; i < len(val); i++ {
		v := []byte(val)[i]
		node = node.children[v]
		if node == nil {
			return nil
		}
	}
	return node
}

// generateTrie generates a trie from the sieve.
func generateTrie(sieve []bool) *trieNode {
	if len(sieve) == 0 {
		return &trieNode{}
	}
	// Find all nodes that need to be created by getting all prefixes of all primes.
	// A number is a prefix of itself, so all primes are included in prefixes.
	prefixes := make([]bool, len(sieve))
	nodeCount := 0
	for i := 0; i < len(sieve); i++ {
		if !sieve[i] {
			continue
		}
		prefix := i
		for !prefixes[prefix] {
			prefixes[prefix] = true
			nodeCount++
			prefix /= 10
		}
	}

	// allocate all the nodes at once
	nodes := make([]trieNode, nodeCount)

	populateChildren(0, 0, nodes, sieve, prefixes)
	return &nodes[0]
}

// populateChildren populates the children of the node at nodeIdx with values from the sieve.
func populateChildren(nodeIdx, val int, nodes []trieNode, sieve, prefixes []bool) int {
	if nodeIdx >= len(sieve) {
		return nodeIdx
	}
	// Set positions for 0 and 9 digits
	firstChild := val * 10
	lastChild := min(firstChild+9, len(sieve)-1)
	// root node should not have a 0 child
	if nodeIdx == 0 {
		firstChild++
	}

	// Count the children so we can create a correctly sized map
	childCount := 0
	for childVal := firstChild; childVal <= lastChild; childVal++ {
		if prefixes[childVal] {
			childCount++
		}
	}

	// Nothing to do if there are no children
	if childCount == 0 {
		return nodeIdx
	}
	node := &nodes[nodeIdx]
	node.children = make(map[byte]*trieNode, childCount)

	for childVal := firstChild; childVal <= lastChild; childVal++ {
		if !prefixes[childVal] {
			continue
		}
		// The child's node will be the next node in the slice
		nodeIdx++
		child := &nodes[nodeIdx]
		child.terminal = sieve[childVal]
		// Convert the child's value to the digit that it represents '0' to '9'
		byteVal := byte(childVal%10) + '0'
		node.children[byteVal] = child
		// recurse to populate grand babies
		nodeIdx = populateChildren(nodeIdx, childVal, nodes, sieve, prefixes)
	}
	return nodeIdx
}

// findPrimes returns a sorted slice of all primes between 0 and upperBound that have the given prefix.
func findPrimes(upperBound, prefix int) []int {
	if upperBound < 2 {
		return nil
	}
	sieve := make([]bool, upperBound+1)
	setPrimes(sieve)

	head := generateTrie(sieve)
	prefixString := strconv.Itoa(prefix)
	head = head.findNode(prefixString)
	var result []int
	for _, val := range head.values("") {
		n, err := strconv.Atoi(prefixString + val)
		if err != nil {
			panic(err)
		}
		result = append(result, n)
	}
	sort.Ints(result)
	return result
}

// main function runs the benchmark for the findPrimes function.
func main() {
	err := benchmark.Run("", findPrimes)
	if err != nil {
		panic(err)
	}
}
