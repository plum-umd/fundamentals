import java.util.*;
import tester.*;

class Graph {
    Hashtable<String,ArrayList<Edge>> g;
    Graph() {
	this.g = new Hashtable<String,ArrayList<Edge>>();
    }

    // EFFECT: Add edge between node1 and node2 in this graph.
    void addEdge(String node1, String node2) {
	if (!this.g.containsKey(node1)) {
	    this.g.put(node1, new ArrayList<Edge>());
	}
	if (!this.g.containsKey(node2)) {
	    this.g.put(node2, new ArrayList<Edge>());
	}

	this.g.get(node1).add(new Edge(node1, node2));
	this.g.get(node2).add(new Edge(node2, node1));
    }

    // Get the neighbors of the given node in this graph.
    ArrayList<Edge> getNeighbors(String node) {
	return this.g.get(node);
    }

    // Does this graph contain the given node?
    Boolean containsNode(String node) {
	return this.g.containsKey(node);
    }

    // Is there an edge between node1 and node2?
    Boolean edgeBetween(String node1, String node2) {
	if (!this.containsNode(node1)) { return false; }
	if (!this.containsNode(node2)) { return false; }
	for (Edge e : this.getNeighbors(node1))
	    if (e.to.equals(node2))
		return true;
	return false;
    }
}

class Edge {
    String from;
    String to;
    Edge(String from, String to) {
	this.from = from;
	this.to = to;
    }
}

// Represents all the nodes we've already visited.
class Seen {
    ArrayList<String> seen;
    Seen() {
	this.seen = new ArrayList<String>();
    }

    // EFFECT: add given node to this seen.
    void add(String node) {
	this.seen.add(node);
    }

    // Has the given node been seen already?
    Boolean hasSeen(String node) {
	for (String n : seen) {
	    if (n.equals(node)) {
		return true;
	    }
	}
	return false;
    }
}

class Algorithms {

    // Is there a path from start to end in g?
    Boolean pathTo(Graph g, String start, String end) {
	Seen seen = new Seen();
	return this.pathToSeen(g, start, end, new Seen());
    }

    Boolean pathToAlt(Graph g, String start, String end) {
	Seen seen = new Seen();
	ToDo todo = new ....;
	todo.add(new Edge("", start));

	while(!todo.isEmpty()) {
	    Edge edge = todo.remove();
	    seen.add(edge.to);
	    if (edge.to.equals(end))
		return true;
	    for (Edge e : g.getNeighbors(edge.to)) {
		if (!seen.hasSeen(e.from))
		    todo.add(e);
	    }
	}
	return false;
    }

    Boolean pathToSeen(Graph g, String start, String end, Seen seen) {
	if (start.equals(end)) {
	    return true;
	}
	seen.add(start);
	for (Edge e : g.getNeighbors(start)) {
	    if (!seen.hasSeen(e.to)) {
		if (this.pathToSeen(g, e.to, end, seen)) {
		    return true;
		}
	    }
	}
	return false;
    }
}

class Examples {

    void testMBTA(Tester t) {
	Graph MBTA = new Graph();
	Algorithms algo = new Algorithms();
	MBTA.addEdge("Ruggles", "Mass Ave");
	MBTA.addEdge("Mass Ave", "BB");
	MBTA.addEdge("Porter", "Harvard");

	t.checkExpect(MBTA.containsNode("Ruggles"), true);
	t.checkExpect(MBTA.containsNode("Mass Ave"), true);
	t.checkExpect(MBTA.containsNode("BB"), true);
	t.checkExpect(MBTA.containsNode("DC"), false);
	t.checkExpect(MBTA.edgeBetween("Ruggles", "Mass Ave"), true);
	t.checkExpect(MBTA.edgeBetween("Mass Ave", "BB"), true);
	t.checkExpect(MBTA.edgeBetween("Ruggles", "BB"), false);
	t.checkExpect(MBTA.edgeBetween("Ruggles", "Park"), false);

	t.checkExpect(algo.pathTo(MBTA, "Ruggles", "Ruggles"), true);
	t.checkExpect(algo.pathTo(MBTA, "Ruggles", "Mass Ave"), true);
	t.checkExpect(algo.pathTo(MBTA, "Ruggles", "BB"), true);
	t.checkExpect(algo.pathTo(MBTA, "Ruggles", "Harvard"), false);
    }

}


