using UnityEngine;


namespace Infovis {

  public class Element {

    public float  size  = 0          ;
    public Color  color = Color.white;
    public string text  = ""         ;

    public override string ToString() {
      return "size = " + size + ", color = " + color + ", text = " + text;
    }

  }

  public class Points : Element {

    public Vector3[] locations = {};

    public override string ToString() {
      string line = "Points: " + base.ToString() + ", locations =";
      foreach(Vector3 x in locations)
        line = line + " " + x;
      return line;
    }

  }
    
  public class Polyline : Element {

    public Vector3[] locations = {};

    public override string ToString() {
      string line = "Polyline: " + base.ToString() + ", locations =";
      foreach(Vector3 x in locations)
        line = line + " " + x;
      return line;
    }

  }
    
  public class Polygon : Element {

    public Vector3[] locations = {};

    public override string ToString() {
      string line = "Polygon: " + base.ToString() + ", locations =";
      foreach(Vector3 x in locations)
        line = line + " " + x;
      return line;
    }

  }
    
  public class Label : Element {

    public Vector3 origin     = Vector3.zero ;
    public Vector3 horizontal = Vector3.right;
    public Vector3 vertical   = Vector3.up   ;

    public override string ToString() {
      return "Label: " + base.ToString() + ", origin = " + origin + ", horizontal = " + horizontal + ", vertical = " + vertical;
    }

  }
    
}
