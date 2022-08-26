/*
Objective: This program serves to execute the main gameplay loop of the text-based implementation of the Slipways game.
Author: Nathan Chapman
Date: 8/26/2022
*/

/*
Outline
-------
1) TODO: Print initial prompt and introduction to the game ex. "Welcome to (text-based) Slipways where you can build your interplanetary trade empire!"

2) TODO: Generate random planets via a seed.  Seed can be given or random.

2) TODO: Get user inputted actions e.g. discover, colonize, connect, trade.

3) TODO: Implement the action on the system via the backend

4) TODO: Return board state

5) goto step 2 and loop!
*/

public class Slipways {

  private static String initPrompt = "Welcome to (text-based) Slipways; where you can build your interplanetary trade empire!/nTo begin you can either specify a seed for planet generation, or you can use a randomly generated one.";

  private static String helpText = "";

  private static String controlsText = "";

  private static String seedText = "Please enter a seed for planet generation.  If you want a random seed, simply press 'Enter'";

  public static void main(String[] args) {
    System.out.println(initPrompt); // Display initial welcome and instructions.

    System.out.println(seedText); // Display seed prompt

    // TODO: get seed and build planet system
  }
}
