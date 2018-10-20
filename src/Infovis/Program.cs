using Google.Protobuf;
using Infovis.Protobuf;
using System;
using System.IO;
using UnityEngine;


namespace Infovis {

  class Program {

    static void Main(string[] arguments) {

      foreach (string filename in arguments) {

        byte[] bytes = File.ReadAllBytes(filename);

        Request request = Request.Parser.ParseFrom(bytes);

        Console.Write(request.Reset);

        foreach (Geometry geometry in request.Upsert) {

          Console.WriteLine(geometry);
  
          Tuple<long, Element> element = Parsing.MakeElement(geometry);
          Console.WriteLine(element.Item1);
          Console.WriteLine(element.Item2);
        }

        foreach (int identifier in request.Delete)
          Console.WriteLine(identifier);

      }

    }

  }

}
