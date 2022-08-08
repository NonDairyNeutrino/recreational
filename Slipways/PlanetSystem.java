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
  public PlanetSystem () {
    planets = new ArrayList<Planet>();
    graph   = new ArrayList<ArrayList<String> >(planets.size());
    for (int k = 0; k < graph.size(); k++) {
      graph.set(k, new ArrayList<String>());
    }
  }

  public static void addPlanet (Planet p) {
    planets.add(p);
    
  }
}
