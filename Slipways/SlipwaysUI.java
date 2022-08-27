/*
Objective: Provide the front end for user interaction
Author: Nathan Chapman
Date: 8/26/2022
*/

import java.util.Scanner;
import java.util.Random;

public class SlipwaysUI {
  private static Scanner sc = new Scanner(System.in);
  private static Random rng = new Random();

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

}
