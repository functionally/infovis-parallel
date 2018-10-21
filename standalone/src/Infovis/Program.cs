using System;
using System.Threading;
using WebSocketSharp;
using WebSocketSharp.Server;


namespace Infovis {

  class Program {

    static void Main(string[] arguments) {

      WebSocketServer server = new WebSocketServer("ws://127.0.0.1:8080");
      server.Log.Level = LogLevel.Trace;
      server.AddWebSocketService<GeometrySocket>("/Infovis", () => new GeometrySocket() {IgnoreExtensions = true});

      server.Start();

      if (server.IsListening) {

        while (true) {
          State.Refresh();
          Thread.Sleep(10);
        }

        server.Stop(); 

      }

    }

  }

}
