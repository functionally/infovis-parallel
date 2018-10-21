using Infovis.Protobuf;
using System;
using UnityEngine;


namespace Infovis {

  public class Element {

    public float  size  = 0          ;
    public Color  color = Color.white;
    public string text  = ""         ;

    public virtual void Update(Geometry geometry) {
      if (Parsing.DirtySize(geometry))
        size = (float) geometry.Size;
      if (Parsing.DirtyColor(geometry)) {
        float r = (geometry.Colr & 0xFF000000) / (float) 0xFF000000;
        float g = (geometry.Colr & 0x00FF0000) / (float) 0x00FF0000;
        float b = (geometry.Colr & 0x0000FF00) / (float) 0x0000FF00;
        float a = (geometry.Colr & 0x000000FF) / (float) 0x000000FF;
        color = new Color(r, g, b, a);
      }
      if (Parsing.DirtyText(geometry))
        text = geometry.Text;
    }

    public virtual void Dump(string prefix) {
      Console.WriteLine(prefix + "Element");
      Console.WriteLine(prefix + "  size = " + size);
      Console.WriteLine(prefix + "  color = " + color);
      Console.WriteLine(prefix + "  text = " + text);
    }

  }

  public class Points : Element {

    public Vector3[] locations = {};

    public override void Update(Geometry geometry) {
      base.Update(geometry);
      if (Parsing.DirtyLocation(geometry))
        locations = Parsing.MakeLocations(geometry);
    }

    public override void Dump(string prefix) {
      Console.WriteLine(prefix + "Points");
      base.Dump(prefix + "  ");
      Console.WriteLine(prefix + " locations = ");
      foreach(Vector3 xyz in locations)
        Console.WriteLine(prefix + "  " + xyz);
    }

  }
    
  public class Polyline : Element {

    public Vector3[] locations = {};

    public override void Update(Geometry geometry) {
      base.Update(geometry);
      if (Parsing.DirtyLocation(geometry))
        locations = Parsing.MakeLocations(geometry);
    }

    public override void Dump(string prefix) {
      Console.WriteLine(prefix + "Polyline");
      base.Dump(prefix + "  ");
      Console.WriteLine(prefix + " locations = ");
      foreach(Vector3 xyz in locations)
        Console.WriteLine(prefix + "  " + xyz);
    }

  }
    
  public class Polygon : Element {

    public Vector3[] locations = {};

    public override void Update(Geometry geometry) {
      base.Update(geometry);
      if (Parsing.DirtyLocation(geometry))
        locations = Parsing.MakeLocations(geometry);
    }

    public override void Dump(string prefix) {
      Console.WriteLine(prefix + "Polygon");
      base.Dump(prefix + "  ");
      Console.WriteLine(prefix + " locations = ");
      foreach(Vector3 xyz in locations)
        Console.WriteLine(prefix + "  " + xyz);
    }

  }
    
  public class Label : Element {

    public Vector3 origin     = Vector3.zero ;
    public Vector3 horizontal = Vector3.right;
    public Vector3 vertical   = Vector3.up   ;

    public override void Update(Geometry geometry) {
      base.Update(geometry);
      if (Parsing.DirtyLocation(geometry)) {
//      Debug.Assert(geometry.Posx.Count == 3);
//      Debug.Assert(geometry.Posy.Count == 3);
//      Debug.Assert(geometry.Posz.Count == 3);
        origin     = new Vector3((float) geometry.Posx[0], (float) geometry.Posy[0], (float) geometry.Posz[0]);
        horizontal = new Vector3((float) geometry.Posx[1], (float) geometry.Posy[1], (float) geometry.Posz[1]);
        vertical   = new Vector3((float) geometry.Posx[2], (float) geometry.Posy[2], (float) geometry.Posz[2]);
      }
    }

    public override void Dump(string prefix) {
      Console.WriteLine(prefix + "Label");
      base.Dump(prefix + "  ");
      Console.WriteLine(prefix + "  origin = " + origin);
      Console.WriteLine(prefix + "  horizontal = " + horizontal);
      Console.WriteLine(prefix + "  vertical = " + vertical);
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
//    Debug.Assert(geometry.Posy.Count == n);
//    Debug.Assert(geometry.Posz.Count == n);

      Vector3[] locations = new Vector3[n];
      for (int i = 0; i < locations.Length; ++i)
        locations[i] = new Vector3((float) geometry.Posx[i], (float) geometry.Posy[i], (float) geometry.Posz[i]);

      return locations;

    }

  }
    
}
