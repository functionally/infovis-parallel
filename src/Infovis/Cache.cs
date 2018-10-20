using Infovis.Protobuf;
using System;
using System.Collections.Generic;


namespace Infovis {

  public class Cache {

    public Cache() {
    }

    public void Update(Request request) {

      if (request.Reset)
        elements.Clear();

      foreach (Geometry geometry in request.Upsert) {
        long identifier = geometry.Iden;
        Element element = null;
        if (elements.TryGetValue(identifier, out element))
          elements[identifier] = Factory.UpdateElement(element, geometry);
        else
          elements[identifier] = Factory.CreateElement(geometry);
      }

      foreach (long identifier in request.Delete)
        elements.Remove(identifier);

    }

    public void Dump() {
      Dump("");
    }

    public void Dump(string prefix) {
      foreach (long identifier in elements.Keys) {
        Console.WriteLine(prefix + identifier);
        elements[identifier].Dump(prefix + "  ");
      }
    }

    private Dictionary<long, Element> elements = new Dictionary<long, Element>();

  }

}
