class First {
    public static void main (String[] args) {
	System.out.println( new Coord(3, 4).move(1, 2).x );
    }
}

class Coord {
    Integer x;
    Integer y;

    Coord (Integer x, Integer y) {
	this.x = x;
	this.y = y;
    }

    Coord move (Integer dx, Integer dy) {
	return new Coord (this.x + dx, this.y + dy);
    }    
}


    
    
