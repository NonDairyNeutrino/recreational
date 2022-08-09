/*
Objective: Graph representation of the system of planets
Author: Nathan Chapman
Date: 8/7/2022
*/

import java.util.ArrayList;

public class PlanetSystem {
// CLASS VARIABLES
  public  static ArrayList<Planet> planets;
  private static ArrayList<ArrayList<String> > network;

// METHODS
  // create an empty planetary system
  public PlanetSystem () {
    // initialize system with 0 planets
    planets = new ArrayList<Planet>();
    // initialize system with a null network adjacency matrix
    network   = new ArrayList<ArrayList<String> >(0);
    for (int k = 0; k < network.size(); k++) {
      network.set(k, new ArrayList<String>(0));
    }
  }

  private static void extendGraph () {
    network.forEach((row) -> row.add("")); // tack on another element at the end of each  row
    network.add(new ArrayList<String>(network.size() + 1));    // add another row
    network.get(network.size() - 1).set(network.size() - 1, ""); // initialize the last element of the new row
  }

  private static void updateGraph (Planet p) {
    extendGraph();

  }

  public static void addPlanet (Planet p) {
    planets.add(p);
    extendGraph();

    // TODO: add network adjacency matrix
  }
}
