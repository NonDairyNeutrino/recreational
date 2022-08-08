import java.util.ArrayList;
import java.util.Collections;

public class Planet {
  // Instrinsic properties
  private boolean discovered = false;
  private boolean colonized = false;
  public String name;
  public ArrayList<String> needs = new ArrayList<String>();
  public ArrayList<String> gives = new ArrayList<String>();
  // Extrinsic properties
  public ArrayList<Planet> connections = new ArrayList<Planet>();

  public Planet(ArrayList<String> needs, ArrayList<String> gives) {
    this.needs = needs;
    this.gives = gives;
    PlanetSystem.addPlanet(this);
  }

  public void discover () {
    this.discovered = true;
  }

  public void colonize() {
    this.colonized = true;
  }

  public void connect(Planet p) {
    //PlanetSystem

    /*this.connections.add(p);
    p.connections.add(this);*/
  }

  public boolean isConnected(Planet p) {
    return this.connections.contains(p);
  }

  public void trade(Planet p){
    boolean tradeTo, tradeFrom;
    if(
      // are they connected?
      this.connections.contains(p) && (
        // can this give?
        (tradeTo = !Collections.disjoint(this.gives, p.needs)) ||
        // can this receive?
        !Collections.disjoint(this.needs, p.gives)
      )
    ) {
      if (tradeTo) {

      }
      else {

      }
    }
  }

}
