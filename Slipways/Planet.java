public class Planet {
  public boolean colonized = false;
  public String name;
  public ArrayList<String> needs = new ArrayList<String>;
  public ArrayList<String> gives = new ArrayList<String>;
  public ArrayList<String> connections = new ArrayList<String>;
  public static ArrayList<String> planet_list = new ArrayList<String>;

  public Planet(ArrayList<String> needs, ArrayList<String> gives) {
    this.needs = needs;
    this.gives = gives;
    planet_list.add(this)
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
