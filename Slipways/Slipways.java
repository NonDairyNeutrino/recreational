/*
Objective: This program serves to execute the main gameplay loop of the text-based implementation of the Slipways game.
Author: Nathan Chapman
Date: 8/26/2022
*/

/*
Outline
-------
1) Print initial prompt and introduction to the game ex. "Welcome to (text-based) Slipways where you can build your interplanetary trade empire!"

2) Generate random planets via a seed.  Seed can be given or random.

2) TODO: Get user inputted actions e.g. discover, colonize, connect, trade.

3) TODO: Implement the action on the system via the backend

4) TODO: Return board state

5) goto step 2 and loop!
*/

import java.util.Random;

public class Slipways {

  private static Random rng;

  //private static String helpText = ""

  //private static String controlsText = "";

  private static void welcome () {
    System.out.println("Welcome to (text-based) Slipways; where you can build your interplanetary trade empire!");
    System.out.println("To begin you can either specify a seed for planet generation, or you can use a randomly generated one.");
  }

  private static Random getRNG () {
    String seedText = "Please enter a seed for planet generation.  If you want a random seed, simply press 'Enter'";
    System.out.println(seedText);    // Display seed prompt
    int seed = SlipwaysUI.getSeed(); // get seed
    return new Random(seed);          // get planet rng
  }

  private static void initPlanetSystem () {
    PlanetSystem systemFull = new PlanetSystem(); // create an empty space
    System.out.println(systemFull.network);
    for (int k = 0; k < 10; k++) {
      systemFull.addPlanet(new Planet(rng.nextInt())); // fill the space with planets, each with random ids
    }
  }

  public static void main(String[] args) {
    welcome();
    rng = getRNG();
    initPlanetSystem();
  }
}
