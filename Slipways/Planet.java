import java.util.ArrayList;
import java.util.Collections;

public class Planet {
  // Instrinsic properties
  public String name;
  private int id;
  public boolean discovered = false;
  public boolean colonized  = false;
  public ArrayList<String> needs = new ArrayList<String>();
  public ArrayList<String> gives = new ArrayList<String>();
  // Extrinsic properties
  public ArrayList<Planet> connections = new ArrayList<Planet>();

  public Planet(int id) {
    this.id = id;
  }

  public void discover () {
    this.discovered = true;
    this.needs = needs;
    this.gives = gives;
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

  public static void main(String[] args) {
    System.out.println("test");
  }

}
