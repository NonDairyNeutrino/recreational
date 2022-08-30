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

  public static void main(String[] args) {
    SlipwaysUI.welcome();
    rng = SlipwaysUI.getRNG();
    System.out.println(rng.nextInt());
  }
}
