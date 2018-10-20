using Infovis.Protobuf;
using System;
using UnityEngine;


namespace Infovis {

  public static class Parsing {

    public static Tuple<long, Element> MakeElement(Geometry geometry) {

      Element result;
      switch (geometry.Type) {
        case 1:
          result = Update(new Points(), geometry);
          break;
        case 2:
          result = Update(new Polyline(), geometry);
          break;
        case 3:
          result = Update(new Polygon(), geometry);
          break;
        case 4:
          result = Update(new Label(), geometry);
          break;
        default:
          result = null;
          break;
      }

      return Tuple.Create(geometry.Iden, result);

    }

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

    private static void UpdateElement(Element element, Geometry geometry) {

      if (DirtySize(geometry))
        element.size = (float) geometry.Size;

      if (DirtyColor(geometry)) {
        float r = (geometry.Colr & 0xFF000000) / (float) 0xFF000000;
        float g = (geometry.Colr & 0x00FF0000) / (float) 0x00FF0000;
        float b = (geometry.Colr & 0x0000FF00) / (float) 0x0000FF00;
        float a = (geometry.Colr & 0x000000FF) / (float) 0x000000FF;
        element.color = new Color(r, g, b, a);
      }

      if (DirtyText(geometry))
        element.text = geometry.Text;

    }

    private static Vector3[] MakeLocations(Geometry geometry) {

      int n = geometry.Posx.Count;
//    Debug.Assert(geometry.Posy.Count == n);
//    Debug.Assert(geometry.Posz.Count == n);

      Vector3[] locations = new Vector3[n];
      for (int i = 0; i < locations.Length; ++i)
        locations[i] = new Vector3((float) geometry.Posx[i], (float) geometry.Posy[i], (float) geometry.Posz[i]);

      return locations;

    }

    public static Points Update(Points element, Geometry geometry) {

      UpdateElement(element, geometry);

      if (DirtyLocation(geometry))
        element.locations = MakeLocations(geometry);

      return element;

    }

    public static Polyline Update(Polyline element, Geometry geometry) {

      UpdateElement(element, geometry);

      if (DirtyLocation(geometry))
        element.locations = MakeLocations(geometry);

      return element;

    }

    public static Polygon Update(Polygon element, Geometry geometry) {

      UpdateElement(element, geometry);

      if (DirtyLocation(geometry))
        element.locations = MakeLocations(geometry);

      return element;

    }

    public static Label Update(Label element, Geometry geometry) {

      UpdateElement(element, geometry);

      if (DirtyLocation(geometry)) {
//      Debug.Assert(geometry.Posx.Count == 3);
//      Debug.Assert(geometry.Posy.Count == 3);
//      Debug.Assert(geometry.Posz.Count == 3);
        element.origin     = new Vector3((float) geometry.Posx[0], (float) geometry.Posy[0], (float) geometry.Posz[0]);
        element.horizontal = new Vector3((float) geometry.Posx[1], (float) geometry.Posy[1], (float) geometry.Posz[1]);
        element.vertical   = new Vector3((float) geometry.Posx[2], (float) geometry.Posy[2], (float) geometry.Posz[2]);
      }

      return element;

    }

  }

}
