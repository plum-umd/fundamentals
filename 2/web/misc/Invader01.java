/**
 * @opt attributes
 * @opt types
 * @opt inferrel
 * @hidden
 */
class UMLOptions {}

class World {
    Swarm swarm;
    Laser laser;
    Bullet maybeBullet;
    Number ticks;
}

class Laser {
    Posn p;
}

class Posn {
    Number x;
    Number y;
}

interface Bullet {
    public Bullet next();
    public Scene drawOn(Scene s);
    public Bullet spray(Invaders is);
    public Swarm mowDown(Swarm s);
    public Bullet fire(Laser l);
}


class NoneBullet implements Bullet {}
class SomeBullet implements Bullet {
    Posn p;
}

interface Invader {
    public Invader next();
    public Invader kill();
    public Number leftX();
    public Number rightX();
    public Number bottomY();
    public Number x();
    public Number y();
    public Boolean isDead();
}

class DeadInvader implements Invader {}
class LiveInvader implements Invader {
    Posn p;
}

interface Swarm {
    public Scene drawOn(Scene s);
    public Swarm next();
    public Number leftX();
    public Number rightX();
    public Swarm shotAt(Bullet b);
    public Boolean isDead();
    public Boolean isPastBotton();
}

class LeftSwarm implements Swarm {
    Posn p;
    Invaders invaders;
}
class RightSwarm implements Swarm {
    Posn p;
    Invaders invaders;
}

interface Invaders {
    public Boolean allDead();
    public Invaders shotAt(Bullet b);
    public Boolean isPastBottom();
}

class EmptyInvaders implements Invaders {}
class ConsInvaders implements Invaders {
    Invader first;
    Invaders rest;
}

/**	
 * @hidden		    
 */
class Number {}

/**
 * @hidden
 *
 */
class Scene {}