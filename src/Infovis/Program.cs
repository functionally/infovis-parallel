using Google.Protobuf;
using Infovis.Protobuf;
using System;
using System.IO;
using UnityEngine;


namespace Infovis {

  class Program {

    static void Main(string[] arguments) {

      Cache cache = new Cache();

      foreach (string filename in arguments) {
        Console.WriteLine(filename);
        Request request = Request.Parser.ParseFrom(File.ReadAllBytes(filename));
//      Console.WriteLine(request);
        cache.Update(request);
        cache.Dump("  ");
      }
 
    }

  }

}
