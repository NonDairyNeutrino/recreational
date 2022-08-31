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

    String input;
    SlipwaysUI.actionPrompt();
    while (!(input = SlipwaysUI.sc.nextLine()).equals("quit") && !input.equals("q")) {
      // input can be one of "probe", "discover", "connect", etc.
      switch (input) {
        // display
        case "b":
        case "board":
        case "s":
        case "state":
          System.out.println("BOARD");
          break;
        case "actions":
        case "controls":
          SlipwaysUI.showActions();
          break;
        // actions
        case "p":
        case "probe":
          System.out.println("probe out");
          break;
        case "d":
        case "discover":
          System.out.println("discover out");
          break;
        case "c":
        case "connect":
          System.out.println("connect out");
          break;
        //
        case "clear":
          System.out.print("\033\143");
          break;
      }
      // TODO: show board state
      SlipwaysUI.actionPrompt();
    }
  }
}
