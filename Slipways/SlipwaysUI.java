/*
Objective: Provide the front end for user interaction
Author: Nathan Chapman
Date: 8/26/2022
*/

import java.util.Scanner;
import java.util.Random;

public class SlipwaysUI {
  public static Scanner sc = new Scanner(System.in);
  private static Random rng = new Random();

  //private static String helpText = ""

  //private static String controlsText = "";

  public static void welcome () {
    System.out.println("Welcome to (text-based) Slipways; where you can build your interplanetary trade empire!");
    System.out.println("To begin you can either specify a seed for planet generation, or you can use a randomly generated one.");
  }

  // get the user input seed or generatea a random one
  public static int getSeed () {
    String seedString;
    // if they input an empty line, return a random integer
    if ((seedString = sc.nextLine()).equals("")) {
      return rng.nextInt();
    }
    // if not, return the integer value of the string they entered
    else {
      return Integer.valueOf(seedString);
    }
  }

  public static Random getRNG () {
    String seedText = "Please enter an integer seed for planet generation.  If you want a random seed, simply press 'Enter'";
    System.out.println(seedText);    // Display seed prompt
    int seed = getSeed(); // get seed
    return new Random(seed);          // get planet rng
  }

}
