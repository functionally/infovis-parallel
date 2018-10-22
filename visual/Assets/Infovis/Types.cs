using Infovis.Protobuf;
using UnityEngine;

using Debug = UnityEngine.Debug;


namespace Infovis {

  public class Element {

    public GameObject obj   = null       ;
    public float      size  = 0          ;
    public Color      color = Color.white;
    public string     text  = ""         ;

    public Element(GameObject root, GameObject o, string identifier) {
      obj = o;
      obj.name = identifier;
      obj.tag = "infovis";
      obj.transform.parent = root.transform;
    }

    public virtual void Update(Geometry geometry) {

      if (Parsing.DirtySize(geometry))
        size = (float) geometry.Size;

      if (Parsing.DirtyColor(geometry)) {
        float r = (geometry.Colr & 0xFF000000) / (float) 0xFF000000;
        float g = (geometry.Colr & 0x00FF0000) / (float) 0x00FF0000;
        float b = (geometry.Colr & 0x0000FF00) / (float) 0x0000FF00;
        float a = (geometry.Colr & 0x000000FF) / (float) 0x000000FF;
        color = new Color(r, g, b, a);
        obj.GetComponent<Renderer>().material.color = color;
      }

      if (Parsing.DirtyText(geometry))
        text = geometry.Text;

    }

    public virtual void Dump(string prefix) {
      Debug.Log(prefix + "Element");
      Debug.Log(prefix + "  size = " + size);
      Debug.Log(prefix + "  color = " + color);
      Debug.Log(prefix + "  text = " + text);
    }

    protected static Object LoadCone() {
      if (cone == null)
        cone = Resources.Load("cone");
      return cone;
    }

    private static Object cone = null;

  }

  public class Point : Element {

    public Point(GameObject root, string identifier)
      : base(root, GameObject.CreatePrimitive(PrimitiveType.Sphere), identifier) {
    }

    public Vector3 location = Vector3.zero;

    public override void Update(Geometry geometry) {

      base.Update(geometry);

      if (Parsing.DirtySize(geometry)) {
        float radius = size / 2;
        obj.transform.localScale = new Vector3(radius, radius, radius);
      }

      if (Parsing.DirtyLocation(geometry)) {

        Debug.Assert(geometry.Posx.Count == 1);
        Debug.Assert(geometry.Posy.Count == 1);
        Debug.Assert(geometry.Posz.Count == 1);

        location = new Vector3((float) geometry.Posx[0], (float) geometry.Posy[0], (float) geometry.Posz[0]);

        obj.transform.position = location;

      }

    }

    public override void Dump(string prefix) {
      Debug.Log(prefix + "Point");
      base.Dump(prefix + "  ");
      Debug.Log(prefix + " location = " + location);
    }

  }
    
  public class Line : Element {

    public Vector3[] locations = {};

    public Line(GameObject root, string identifier)
      : base(root, GameObject.CreatePrimitive(PrimitiveType.Cylinder), identifier) {
    }

    public override void Update(Geometry geometry) {

      base.Update(geometry);

      if (Parsing.DirtyLocation(geometry)) {

        Debug.Assert(geometry.Posx.Count == 2);
        Debug.Assert(geometry.Posy.Count == 2);
        Debug.Assert(geometry.Posz.Count == 2);

        locations = new Vector3[]{
          new Vector3((float) geometry.Posx[0], (float) geometry.Posy[0], (float) geometry.Posz[0]),
          new Vector3((float) geometry.Posx[1], (float) geometry.Posy[1], (float) geometry.Posz[1])
        };

        Vector3 midpoint = (locations[0] + locations[1]) / 2;
        Vector3 displacement = locations[1] - locations[0];
        obj.transform.position = midpoint;
        obj.transform.rotation = Quaternion.LookRotation(displacement);
        obj.transform.Rotate(90, 0, 0);

        if (geometry.Glyp == 1) {
          GameObject glyph = (GameObject) Object.Instantiate(LoadCone(), obj.transform, false);
          foreach (MeshRenderer renderer in glyph.GetComponentsInChildren<MeshRenderer>())
            renderer.material.color = color;
        }

      }

      if (Parsing.DirtySize(geometry) || Parsing.DirtyLocation(geometry)) {
        float radius = size / 2;
        Vector3 displacement = locations[1] - locations[0];
        obj.transform.localScale = new Vector3(radius, displacement.magnitude / 2, radius);
      }

    }

    public override void Dump(string prefix) {
      Debug.Log(prefix + "Line");
      base.Dump(prefix + "  ");
      Debug.Log(prefix + " locations = ");
      foreach(Vector3 xyz in locations)
        Debug.Log(prefix + "  " + xyz);
    }

  }
    
  public class Rectangle : Element {

    public Vector3[] locations = {};

    public Rectangle(GameObject root, string identifier)
      : base(root, GameObject.CreatePrimitive(PrimitiveType.Plane), identifier) {
    }

    public override void Update(Geometry geometry) {

      base.Update(geometry);

      if (Parsing.DirtyLocation(geometry))
        locations = Parsing.MakeLocations(geometry);

    }

    public override void Dump(string prefix) {
      Debug.Log(prefix + "Rectangle");
      base.Dump(prefix + "  ");
      Debug.Log(prefix + " locations = ");
      foreach(Vector3 xyz in locations)
        Debug.Log(prefix + "  " + xyz);
    }

  }
    
  public class Label : Element {

    public Vector3 origin     = Vector3.zero ;
    public Vector3 horizontal = Vector3.right;
    public Vector3 vertical   = Vector3.up   ;

    public Label(GameObject root, string identifier)
      : base(root, new GameObject(), identifier) {
    }

    public override void Update(Geometry geometry) {

      base.Update(geometry);

      if (Parsing.DirtyLocation(geometry)) {
        Debug.Assert(geometry.Posx.Count == 3);
        Debug.Assert(geometry.Posy.Count == 3);
        Debug.Assert(geometry.Posz.Count == 3);
        origin     = new Vector3((float) geometry.Posx[0], (float) geometry.Posy[0], (float) geometry.Posz[0]);
        horizontal = new Vector3((float) geometry.Posx[1], (float) geometry.Posy[1], (float) geometry.Posz[1]);
        vertical   = new Vector3((float) geometry.Posx[2], (float) geometry.Posy[2], (float) geometry.Posz[2]);
      }

    }

    public override void Dump(string prefix) {
      Debug.Log(prefix + "Label");
      base.Dump(prefix + "  ");
      Debug.Log(prefix + "  origin = " + origin);
      Debug.Log(prefix + "  horizontal = " + horizontal);
      Debug.Log(prefix + "  vertical = " + vertical);
    }

  }

  static class Parsing {

    public static bool DirtyLocation(Geometry geometry) {
      return (geometry.Mask & 1 << 0) != 0;
    }

    public static bool DirtySize(Geometry geometry) {
      return (geometry.Mask & 1 << 1) != 0;
    }

    public static bool DirtyColor(Geometry geometry) {
      return (geometry.Mask & 1 << 2) != 0;
    }

    public static bool DirtyText(Geometry geometry) {
      return (geometry.Mask & 1 << 3) != 0;
    }

    public static Vector3[] MakeLocations(Geometry geometry) {

      int n = geometry.Posx.Count;
     Debug.Assert(geometry.Posy.Count == n);
     Debug.Assert(geometry.Posz.Count == n);

      Vector3[] locations = new Vector3[n];
      for (int i = 0; i < locations.Length; ++i)
        locations[i] = new Vector3((float) geometry.Posx[i], (float) geometry.Posy[i], (float) geometry.Posz[i]);

      return locations;

    }

  }
    
}
