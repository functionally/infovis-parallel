using UnityEngine;
using WebSocketSharp.Server;

using Debug = UnityEngine.Debug;


namespace Infovis {

  public class Program : MonoBehaviour {

    private const string serviceAddress = "ws://0.0.0.0:8080";

    private const string serviceName = "/Infovis";

    private WebSocketServer server = null;

    void Start() {

      server = new WebSocketServer(serviceAddress);
      server.AddWebSocketService<GeometrySocket>(serviceName, () => new GeometrySocket() {IgnoreExtensions = true});
      Debug.Log("Starting service on " + serviceAddress + serviceName);

      server.Start();

    }

    void Update() {

      State.Refresh();

    }

  }

}
