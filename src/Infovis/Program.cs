using Google.Protobuf;
using Infovis.Protobuf;
using System;
using System.IO;
using UnityEngine;
using WebSocketSharp;
using WebSocketSharp.Server;


namespace Infovis {

  class Program {

    static void Main(string[] arguments) {

      Cache cache = new Cache();
      
      WebSocketServer server = new WebSocketServer("ws://127.0.0.1:8080");
      server.AddWebSocketService<GeometrySocket>("/Infovis");

      server.Start();

      if (server.IsListening) {
        Console.WriteLine("Listening on port {0}, and providing WebSocket services:", server.Port);
        foreach (var path in server.WebSocketServices.Paths)
          Console.WriteLine("  {0}", path);
      }
      Console.WriteLine("Press Enter key to stop the server. . . .");
      Console.ReadLine();

      server.Stop(); 

    }

    static void OldMain(string[] arguments) {

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
