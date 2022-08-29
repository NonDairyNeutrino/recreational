/*
Objective: Graph representation of the system of planets
Author: Nathan Chapman
Date: 8/7/2022
*/

import java.util.ArrayList;

public class PlanetSystem {
// CLASS VARIABLES
  public ArrayList<Planet> planets;
  private ArrayList<ArrayList<String> > network;

// METHODS
  // create an empty planetary system
  public PlanetSystem () {
    // initialize system with 0 planets
    planets = new ArrayList<Planet>(0);
    // initialize system with a null network adjacency matrix
    network   = new ArrayList<ArrayList<String> >(0);
    for (int k = 0; k < network.size(); k++) {
      network.set(k, new ArrayList<String>());
    }
  }

  // add a new row and column to the network adjacency matrix
  private void extendNetwork () {
    network.forEach((row) -> row.add("")); // tack on another element at the end of each  row
    network.add(new ArrayList<String>(network.size() + 1));    // add another row
    network.get(network.size() - 1).set(network.size() - 1, ""); // initialize the last element of the new row
  }

  // when a
  public void addPlanet (Planet p) {
    planets.add(p);
    extendNetwork();
  }

  // TODO: add  planet connecting
  public void addConnection (Planet p1, Planet p2) {
    p1.connect(p2);

  }

  // trade between planets
  public void trade(Planet p){

  }

  public static void main(String[] args) {
    PlanetSystem empire = new PlanetSystem();
    System.out.println(empire.planets);
  }
}
