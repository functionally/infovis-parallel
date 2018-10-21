using Infovis.Protobuf;
using System;
using System.Collections.Generic;


namespace Infovis {

  public class Cache {

    public static void Process(Request request) {
      lock (pending)
        pending.Enqueue(request);
    }

    public static void Refresh() {
      while (pending.Count > 0)
        lock (pending) {
          Request request = pending.Dequeue();          
          Console.WriteLine(request);
          cache.Update(request);
          cache.Dump("  ");
        }
    }

    private static Queue<Request> pending = new Queue<Request>();

    private static Cache cache = new Cache();

    private void Update(Request request) {

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

    private void Dump() {
      Dump("");
    }

    private void Dump(string prefix) {
      foreach (long identifier in elements.Keys) {
        Console.WriteLine(prefix + identifier);
        elements[identifier].Dump(prefix + "  ");
      }
    }

    private Dictionary<long, Element> elements = new Dictionary<long, Element>();

  }

}
