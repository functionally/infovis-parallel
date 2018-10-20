using Google.Protobuf;
using Infovis.Protobuf;
using System;
using System.IO;


namespace Infovis
{

  class Program
  {

    static void Main(string[] args)
    {
      Response x = new Response {
        Log = "a message"
      };
      Console.WriteLine(x);
 
      byte[] serialized;
      if (true) {
        serialized = x.ToByteArray();
      } else {
        using (MemoryStream stream = new MemoryStream()) {
          x.WriteTo(stream);
          serialized = stream.ToArray();
        }
      }
      Response y = Response.Parser.ParseFrom(serialized);

      Console.WriteLine(y);
    }

  }

}
