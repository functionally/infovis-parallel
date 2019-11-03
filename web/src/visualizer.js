
'use strict';

const Keyboard = require("./keyboard")
const Rendering = {
  Frames     : require("./rendering/frames"    )
, Linear     : require("./rendering/linear"    )
, Program    : require("./rendering/program"   )
, Projection : require("./rendering/projection")
, Selector   : require("./rendering/selector"  )
, Text       : require("./rendering/text"      )
}


require("./gl-matrix")


const DEBUG = false


const forgetKeys = true


const quat = glMatrix.quat
const vec3 = glMatrix.vec3


const zero = vec3.fromValues(0, 0, 0)


function setupCanvas(gl, useBlending = !DEBUG, useCulling = !DEBUG) {

  if (DEBUG) console.debug("setupCanvas")

  gl.viewport(0, 0, gl.canvas.width, gl.canvas.height)

  gl.clearColor(0., 0., 0., 1.)
  gl.clearDepth(1.0)

  gl.enable(gl.DEPTH_TEST)
  gl.depthFunc(gl.LEQUAL)

  if (useBlending) {
    gl.enable(gl.BLEND)
    gl.blendFunc(gl.SRC_ALPHA, gl.ONE_MINUS_SRC_ALPHA)
  }

  if (useCulling) {
    gl.enable(gl.CULL_FACE)
    gl.cullFace(gl.BACK)
  }

  gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
}


let theManager  = null
let theSelector = null


function ensureManager(gl) {
  if (theManager == null)
    theManager = Rendering.Frames.createManager(gl)
  if (theSelector == null)
    theSelector = Rendering.Selector.create(gl, theManager.program)
  Rendering.Frames.destroyManager(gl, theManager)
}


function initializeGraphics(gl, initialViewer, initialTool) {
  ensureManager(gl)
  return {
    manager  : theManager
  , selector : theSelector
  , pov      : initialViewer
  , tool     : initialTool
  , offset   : {position: zero, rotation: Rendering.Linear.fromEulerd(zero)}
  , message  : ""
  }
}


function makeLocation(positionRotation) {
  const result = new proto.Infovis.Location()
  result.setPosx(positionRotation.position[0])
  result.setPosy(positionRotation.position[1])
  result.setPosz(positionRotation.position[2])
  result.setRotx(positionRotation.rotation[0])
  result.setRoty(positionRotation.rotation[1])
  result.setRotz(positionRotation.rotation[2])
  result.setRotw(positionRotation.rotation[3])
  return result
}


let isRunning = false


function visualizeBuffers(gl, configuration, requestQueue, keyQueue, respond) {

  isRunning = true

  const graphics = initializeGraphics(
    gl
  , {
      position: configuration.initial.view.position
    , rotation: Rendering.Linear.fromEulerd(configuration.initial.view.orientation)
    }
  , {
      position: configuration.initial.tool.position
    , rotation: Rendering.Linear.fromEulerd(configuration.initial.tool.orientation)
    }
  )

  Rendering.Text.ensureShaders(gl)

  function animation(timestamp) {

    if (!isRunning) {
      keyQueue.length = 0
      requestQueue.length = 0
      return
    }

    const dirtyGraphics = keyQueue.length > 0 || requestQueue.length > 0

    let dirtyResponse = false

    while (keyQueue.length > 0) {
      Keyboard.interpret(keyQueue.pop(), graphics)
      dirtyResponse = true
    }
    if (forgetKeys)
      keyQueue.length = 0

    while (requestQueue.length > 0) {

      const request = requestQueue.pop()

      if (DEBUG) console.debug("animation: request =", request)

      if (request.getShow() != 0) {
        if (DEBUG) console.debug("animate: show =", request.getShow())
        graphics.manager.current = request.getShow()
        dirtyResponse = true
     }

      if (request.getReset()) {
        if (DEBUG) console.debug("animate: reset")
        Rendering.Frames.reset(graphics.manager)
      }

      if (request.getUpsertList().length > 0) {
        if (DEBUG) console.debug("animate: upsert", request.getUpsertList().length)
        Rendering.Frames.insert(gl, request.getUpsertList(), graphics.manager)
      }

      if (request.getDeleteList().length > 0) {
        if (DEBUG) console.debug("anamiate: delete", request.getDeleteList().length)
        Rendering.Frames.delete(request.getDeleteList(), graphics.manager)
      }

      if (request.hasViewloc()) {
        const loc = request.getViewloc()
        graphics.pov.position = vec3.fromValues(loc.getPosx(), loc.getPosy(), loc.getPosz())
        graphics.pov.rotation = quat.fromValues(loc.getRotx(), loc.getRoty(), loc.getRotz(), loc.getRotw())
        dirtyResponse = true
        if (DEBUG) console.debug("animate: view =", graphics.pov)
      }

      if (request.hasToolloc()) {
        const loc = request.getToolloc()
        graphics.tool.position = vec3.fromValues(loc.getPosx(), loc.getPosy(), loc.getPosz())
        graphics.tool.rotation = quat.fromValues(loc.getRotx(), loc.getRoty(), loc.getRotz(), loc.getRotw())
        dirtyResponse = true
        if (DEBUG) console.debug("animate: tool =", graphics.tool)
      }

      if (request.hasOffsetloc()) {
        const loc = request.getOffsetloc()
        graphics.offset.position = vec3.fromValues(loc.getPosx(), loc.getPosy(), loc.getPosz())
        graphics.offset.rotation = quat.fromValues(loc.getRotx(), loc.getRoty(), loc.getRotz(), loc.getRotw())
        if (DEBUG) console.debug("animate: offset =", graphics.offset)
      }

      if (request.getMessage() != "") {
        graphics.message = request.getMessage()
        if (DEBUG) console.debug("animate: message = '", graphics.message, "'")
      }

    }

    if (dirtyGraphics) {

      setupCanvas(gl)

      Rendering.Frames.prepare(gl, graphics.manager)
      Rendering.Selector.prepare(gl, graphics.selector, graphics.tool.position, graphics.tool.rotation)

      const eyes = configuration.display.stereo ? 2 : 1
      for (let eye = 0; eye < eyes; ++eye) {

        gl.viewport((eyes - 1) * eye * gl.canvas.width / 2, 0, gl.canvas.width / eyes, gl.canvas.height)

        const eyeOffset = vec3.scale(
          vec3.create()
        , vec3.fromValues(
            configuration.display.eyeSeparation[0]
          , configuration.display.eyeSeparation[1]
          , configuration.display.eyeSeparation[2]
          )
        , (eyes - 1) * (2 * eye - 1) / 2
        )
        const eyePosition = vec3.add(
          vec3.create()
        , graphics.pov.position
        , vec3.transformQuat(vec3.create(), eyeOffset, graphics.pov.rotation)
        )

        graphics.manager.projection = Rendering.Projection.projection(configuration.display, eyePosition)
        graphics.manager.modelView = Rendering.Projection.modelView(graphics.offset.position, graphics.offset.rotation)

        Rendering.Frames.draw(gl, graphics.manager )
        Rendering.Selector.draw(gl, graphics.selector, graphics.manager.projection, graphics.manager.modelView)

      }

      for (let eye = 0; eye < 2; ++eye) {
        const messageElement = document.getElementById("uiMessage" + eye)
        messageElement.style.left  = ((eyes - 1) * eye * gl.canvas.width / 2) + "px"
        messageElement.style.right = ((3 - eyes + eye) * gl.canvas.width / 2) + "px"
        messageElement.style.width = ((3 - eyes) * 50) + "%"
        const message = messageElement.innerText
        if (graphics.message != message)
          messageElement.innerText = graphics.message
      }

    }

    if (dirtyResponse) {
      const response = new proto.Infovis.Response()
      response.setShown(graphics.manager.current)
      response.setViewloc  (makeLocation(graphics.pov ))
      response.setToolloc  (makeLocation(graphics.tool))
      respond(response)
    }

    window.requestAnimationFrame(animation)

  }

  window.requestAnimationFrame(animation)
}


module.exports = {
  setupCanvas      : setupCanvas
, visualizeBuffers : visualizeBuffers
, stop             : function() {isRunning = false}
}
