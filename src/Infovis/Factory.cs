using Infovis.Protobuf;
using System;
using UnityEngine;


namespace Infovis {

  public static class Factory {

    public static Element CreateElement(Geometry geometry) {
      Element result;
      switch (geometry.Type) {
        case 1:
          result = new Points();
          break;
        case 2:
          result = new Polyline();
          break;
        case 3:
          result = new Polygon();
          break;
        case 4:
          result = new Label();
          break;
        default:
          result = null;
          break;
      }
      result.Update(geometry);
      return result;

    }

    public static Element UpdateElement(Element element, Geometry geometry) {
      element.Update(geometry);
      return element;
    }

  }

}
