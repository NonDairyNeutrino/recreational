/*
Objective: Graph representation of the system of planets
Author: Nathan Chapman
Date: 8/7/2022
*/

import java.util.ArrayList;

public class PlanetSystem {
// CLASS VARIABLES
  public  static ArrayList<Planet> planets;
  private static ArrayList<ArrayList<String> > graph;

// METHODS
  // create an empty planetary system
  public PlanetSystem () {
    // initialize system with 0 planets
    planets = new ArrayList<Planet>();
    // initialize system with a null graph adjacency matrix
    graph   = new ArrayList<ArrayList<String> >(0);
    for (int k = 0; k < graph.size(); k++) {
      graph.set(k, new ArrayList<String>(0));
    }
  }

  private static void extendGraph () {
    graph.forEach((row) -> row.add("")); // tack on another element at the end of each  row
    graph.add(new ArrayList<String>(graph.size() + 1));    // add another row
    graph.get(graph.size() - 1).set(graph.size() - 1, ""); // initialize the last element of the new row
  }

  public static void addPlanet (Planet p) {
    planets.add(p);
    extendGraph();

    // TODO: add graph adjacency matrix
  }
}
