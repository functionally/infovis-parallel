using System.Collections;
using UnityEngine;

using Debug = UnityEngine.Debug;
using Text = UnityEngine.UI.Text;


namespace Infovis {

  public class InfoScreen : MonoBehaviour {

    public Text infoText;

    public GameObject selectionVisualizer;

    void Start() {

      StartCoroutine(ShowMessage("Infovis Parallel\nws://" + Network.player.ipAddress + ":8080", 5f));

    }

    void Update() {

      OVRInput.Controller controller = OVRInput.Controller.Gamepad;

      if (OVRInput.GetDown(OVRInput.Button.Four, controller)) {
        StartCoroutine(ShowMessage("BUTTON FOUR", 2));
      }

    }

    public IEnumerator ShowMessage(string text, float duration) {
      infoText.text = text;
      infoText.enabled = true;
      yield return new WaitForSeconds(duration);
      infoText.enabled = false;
    }

  }

}
