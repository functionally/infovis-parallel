using System.Collections;
using UnityEngine;
using WebSocketSharp.Server;

using Debug = UnityEngine.Debug;
using Text = UnityEngine.UI.Text;


namespace Infovis {

  public class Program : MonoBehaviour {

    private const string serviceAddress = "ws://0.0.0.0:8080";

    private const string serviceName = "/Infovis";

    private WebSocketServer server = null;

    public Text infoText;

    void Start() {

      server = new WebSocketServer(serviceAddress);
      server.AddWebSocketService<GeometrySocket>(serviceName, () => new GeometrySocket() {IgnoreExtensions = true});
      Debug.Log("Starting service on " + serviceAddress + serviceName);

      server.Start();

      GameObject camera = GameObject.Find("OVRCameraRig");
      home = camera.transform.position;

      StartCoroutine(ShowMessage("Infovis Parallel\nws://" + Network.player.ipAddress + ":8080"));

    }

    public IEnumerator ShowMessage(string text) {
      infoText.text = text;
      infoText.enabled = true;
      yield return new WaitForSeconds(10);
      infoText.enabled = false;
    }

    void Update() {

      State.Refresh();

      GameObject camera = GameObject.Find("OVRCameraRig");
      OVRInput.Controller controller = OVRInput.GetActiveController();

      if (OVRInput.GetDown(OVRInput.Button.Two, controller)) {

        camera.transform.position = home;

        StartCoroutine(ShowMessage("Infovis Parallel\nws://" + Network.player.ipAddress + ":8080"));

      } else {

        Vector2 joystick = OVRInput.Get(OVRInput.Axis2D.PrimaryTouchpad, controller);
  
        if (OVRInput.GetDown(OVRInput.Button.One, controller))
          upwards = - upwards;
        float shift = OVRInput.Get(OVRInput.Button.One, controller) ? upwards : 0;
  
        camera.transform.position = camera.transform.position - 0.008f * (new Vector3(joystick[0], shift, joystick[1]));

      }

    }

    private Vector3 home = Vector3.zero;

    private float upwards = -0.5f;

  }

}
