class Shark {
    Integer yCoord;
    Integer health;
    Shark(Integer yCoord, Integer health) {
	this.yCoord = yCoord;
	this.health = health;
    }
}

class Examples {
    Shark s = new Shark(3, 4);
    School s2 = new EmptySchool();
}

class Ocean {
    Shark shark;
    School school;
    Ocean(Shark shark, School school) {
	this.shark = shark;
	this.school = school;
    }
}

class Fish { ... }


interface School {}

class ConsSchool implements School {
    Fish fish;
    School rest;
    ConsSchool(Fish fish, School rest) {
	this.fish = fish;
	this.rest = rest;
    }
}

class EmptySchool implements School {
    EmptySchool() {}
}





