package edu.umd.cmsc132A;

// QUIZ: Write down the SIGNATURES for methods
// that need to be added to the UFO interface
// to implement sameUFO following the double dispatch
// pattern.

// o.sameUFO(u)

interface UFO {
    // Is this the same UFO as o?
    Boolean sameUFO(UFO o);

}
