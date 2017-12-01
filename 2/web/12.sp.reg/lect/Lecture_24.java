import java.util.*;
import tester.*;

/** Represents a Graph */
class Graph {
    HashMap<String, ArrayList<Neighbor>> graph
	= new HashMap<String, ArrayList<Neighbor>>();

    /** Get the Neighbors of the given Node */
    ArrayList<Neighbor> getNeighbors(String node){
        if (!this.graph.containsKey(node)) {
            throw new RuntimeException("Not there!!");
	} else {
	    return this.graph.get(node);
	}
    }

    /** Add an Edge */
    void addEdge(String from, String to, Integer dist){
        if(!this.graph.containsKey(from))
            this.graph.put(from, new ArrayList<Neighbor>());
        if(!this.graph.containsKey(to))
            this.graph.put(to, new ArrayList<Neighbor>());

        this.graph.get(from).add(new Neighbor(to, dist));
        this.graph.get(to).add(new Neighbor(from, dist));
    }
}

/** Represents a Neighbor with a Distance */
class Neighbor {
    String name;
    Integer distance;

    Neighbor(String name, double Distance){
        this.name = name;
        this.distance = distance;
    }
}

/** Represent an Edge in a Path */
class Edge {
    String src;
    String targ;
    Integer distance;

    Edge(String src, String targ, Integer distance) {
        this.src = src;
        this.targ = targ;
        this.distance = distance;
    }
}

/** Represents a Path in a Graph */
class Path {
    ArrayList<Edge> path = new ArrayList<Edge>();

    /** EFFECT: Add a new Edge to this Path */
    void add(Edge e){
        this.path.add(e);
    }

    /** Does this Path connect to the Target */
    Boolean containsTarget(String targ){
        for(Edge e : path) {
            if(e.targ.equals(targ)) {
                return true;
            }
        }
        return false;
    }

    /** Get Edge that matches Targ */
    Edge get(String targ){
        for(Edge e : path){
            if(e.targ.equals(targ)) {
                return e;
            }
        }
        throw new RuntimeException("No There!!");
    }
}

/** Represents a ToDo Visit List */
interface ToDo {

    /** Add an Edge to this ToDo list */
    void add(Edge s);

    /** Remove (and Return) an Element from this ToDo list */
    Edge remove();

    /** Does this ToDo List already contain an Edge with the
     *    given destination? */
    Boolean containsDest(String s);

    /** Is this ToDo list empty? */
    Boolean isEmpty();
}


class Algorithms {

    /** Is there a Path from start to end? */
    Boolean pathTo(Graph g, String start, String end, ToDo todo){
        // Initialize
        Path path = new Path();
        todo.add(new Edge("", start, 0));

        // Until we run out, or reach the destination
        while(!todo.isEmpty()){
	    Edge edge = todo.remove();
            String curr = edge.targ;

            path.add(edge);

            // Done...
            if(curr.equals(end))
                return true;

            // For Each Edge...
            for(Neighbor n : g.getNeighbors(curr)){
                if(!path.containsTarget(n.name)){
		    Edge explr = new Edge(curr, n.name,
					  edge.distance+n.distance);
		    todo.add(explr);
                }
            }
	    return true;
        }
        return false;
    }
}

/** Implements the ToDo in LIFO Order */
class StackToDo implements ToDo{
    ArrayList<Edge> edges = new ArrayList<Edge>();

    /** Add the given Edge to this ToDo */
    public void add(Edge e){
        if(!this.containsDest(e.targ))
            edges.add(e);
    }
    /** Does this ToDo contain an Edge with the given destintation? */
    public Boolean containsDest(String s) {
        for(Edge e : this.edges){
            if(e.targ.equals(s))
                return true;
        }
        return false;
    }
    /** Is this ToDo Empty? */
    public Boolean isEmpty() {
        return edges.isEmpty();
    }
    /** Remove/return the next ToDo Edge */
    public Edge remove() {
        return this.edges.remove(this.edges.size()-1);
    }
}


/** Examples/Tests */
class LectureExamples{

    void testIt(Tester t){
        Graph g = new Graph();

        Algorithms algo = new Algorithms();

        g.addEdge("A", "C", 3);
        g.addEdge("A", "B", 2);
        g.addEdge("A", "D", 4);
        g.addEdge("B", "D", 3);
        g.addEdge("C", "E", 5);
        g.addEdge("C", "D", 1);
        g.addEdge("D", "E", 6);

        t.checkExpect(algo.pathTo(g, "A", "E", new StackToDo()), true);
	// t.checkExpect(algo.pathTo(g, "A", "D", new StackToDo()), true);
        // t.checkExpect(algo.pathTo(g, "A", "R", new StackToDo()), false);
    }
}
