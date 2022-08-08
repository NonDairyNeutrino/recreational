import java.util.ArrayList;

public class Planet {
  // Instrinsic properties
  public boolean colonized = false;
  public String name;
  public ArrayList<String> needs = new ArrayList<String>();
  public ArrayList<String> gives = new ArrayList<String>();
  // Extrinsic properties
  public ArrayList<Planet> connections = new ArrayList<Planet>();

  public Planet(ArrayList<String> needs, ArrayList<String> gives) {
    this.needs = needs;
    this.gives = gives;
    PlanetSystem.planets.add(this);
  }

  public void colonize() {
    this.colonized = true;
  }

  public void connect(Planet p) {
    this.connections.add(p);
    p.connections.add(this);
  }

  public boolean isConnected(Planet p) {
    return this.connections.contains(p);
  }

  public void trade(Planet p){
    if(this.connections.contains(p)) {
      
    }
  }

}
