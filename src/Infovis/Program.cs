using System;
using System.Threading;
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
      while (true) {
        Cache.Refresh();
        Thread.Sleep(10);
      }

      server.Stop(); 

    }

  }

}
