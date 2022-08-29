/*
Objective: Graph representation of the system of planets
Author: Nathan Chapman
Date: 8/7/2022
*/

import java.util.ArrayList;

public class PlanetSystem {
// CLASS VARIABLES
  public ArrayList<Planet> planets;
  public ArrayList<ArrayList<String> > network;
  // values of strings can be: 'u' for undiscovered, "d" for discovered, 'c1/2/3' for colonized at levels 1,2, or 3

// METHODS
  // create an empty planetary system
  public PlanetSystem () {
    // initialize system with 0 planets
    planets = new ArrayList<Planet>(0);
    // initialize system with a null network adjacency matrix
    network = new ArrayList<ArrayList<String> >(0);
    network.add(new ArrayList<String>(0));
  }

  // add a new row and column to the network adjacency matrix
  private void extendNetwork () {

    if (network.get(0).isEmpty()) {
      network.add(new ArrayList<String>(1));
    }
    else {
      network.forEach((row) -> row.add("")); // tack on another element at the end of each  row
      network.add(new ArrayList<String>(network.size() + 1));    // add another row
      network.get(network.size() - 1).set(network.size() - 1, ""); // initialize the last element of the new row
    }
  }

  // when a
  public void addPlanet (Planet p) {
    planets.add(p);
    extendNetwork();
    network.
  }

  // TODO: add  planet connecting
  public void addConnection (Planet p1, Planet p2) {
    p1.connect(p2);

  }

  // trade between planets
  public void trade(Planet p){

  }

  // public static String toString () {
  //   // TODO: print board
  //   return "";
  // }

  public static void main(String[] args) {
    PlanetSystem empire = new PlanetSystem();
    System.out.println(empire.planets);
  }
}
