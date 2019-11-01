
'use strict';


const Configuration = require("./configuration")
const Connection    = require("./connection"   )
const Visualizer    = require("./visualizer"   )


const DEBUG = false


const requestQueue = []

const keyQueue = []

let isVisualizing = false

let theContext = null


function echoHandler(connection, request) {
  if (DEBUG) console.debug("echoHandler: request =", request)
  requestQueue.unshift(request)
}


function keyHandler(event) {
  if (!isVisualizing || event.defaultPrevented)
    return
  keyQueue.unshift({
    key  : event.key || event.keyCode
  , shift: event.shiftKey
  })
}


function startup() {

  document.addEventListener("keydown", keyHandler)

  theContext = uiCanvas.getContext("webgl2")
  const gl = theContext

  gl.canvas.width  = window.innerWidth
  gl.canvas.height = window.innerHeight

  Visualizer.setupCanvas(gl)

  Configuration.compute()
  Connection.updateButtons()

}


function startVisualizing() {

  isVisualizing = true

  const configuration = Configuration.update()
  Connection.reconnect(configuration, echoHandler)
  Visualizer.visualizeBuffers(theContext, configuration, requestQueue, keyQueue)

}


function stopVisualizing() {
  Connection.unconnect()
  isVisualizing = false
}


module.exports = {
  Configuration    : Configuration
, startVisualizing : startVisualizing
, startup          : startup
, stopVisualizing  : stopVisualizing
}
