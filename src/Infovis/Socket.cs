using Infovis.Protobuf;
using System;
using WebSocketSharp;
using WebSocketSharp.Server;


namespace Infovis {

  public class GeometrySocket : WebSocketBehavior {

    Cache cache = new Cache();

    protected override void OnMessage(MessageEventArgs evt) {
//    Console.WriteLine(evt.Data);
      Request request = Request.Parser.ParseFrom(evt.RawData);
      Console.WriteLine(request);
      lock (cache) {
        cache.Update(request);
        cache.Dump("  ");
      }
    }

  }

}
