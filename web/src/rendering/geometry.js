
require("../gl-matrix")


const vec3 = glMatrix.vec3


const GLYPH_Cube     = 0
const GLYPH_Sphere   = 1

const GLYPH_Box      = 0
const GLYPH_Cylinder = 1


function defaultGeometry() {
  return {
    shape: {
      points: []
    , glyph : GLYPH_Cube
    }
  , size : 0
  , color: 0
  , text : ""
  }
}


const MASK_Posn =  1
const MASK_Size =  2
const MASK_Colr =  4
const MASK_Text =  8
const MASK_Glyp = 16


function deltaPosition(deltaGeometry) {
  return (deltaGeometry.getMask() & MASK_Posn) != 0
}

function deltaSize(deltaGeometry) {
  return (deltaGeometry.getMask() & MASK_Size) != 0
}

function deltaColor(deltaGeometry) {
  return (deltaGeometry.getMask() & MASK_Colr) != 0
}

function deltaText(deltaGeometry) {
  return (deltaGeometry.getMask() & MASK_Text) != 0
}

function deltaGlyph(deltaGeometry) {
  return (deltaGeometry.getMask() & MASK_Glyp) != 0
}


const GEOMETRY_Points     = 1
const GEOMETRY_Polylines  = 2
const GEOMETRY_Rectangles = 3
const GEOMETRY_Label      = 4
const GEOMETRY_Axis       = 5


function merge(geometry, deltaGeometry) {

  const mask = deltaGeometry.getMask()

  if ((mask & MASK_Posn) != 0) {

    const posx = deltaGeometry.getPosxList()
    const posy = deltaGeometry.getPosyList()
    const posz = deltaGeometry.getPoszList()

    switch (deltaGeometry.getType()) {

      case GEOMETRY_Points:
        {
          const pointses = []
          let i = 0
          deltaGeometry.getCntsList().forEach(function(n) {
            const points = []           
            for (let j = 0; j < n; ++j)
              points.push(vec3.fromValues(posx[i], posy[i], posz[i++]))
            pointses.push(points)
          })
          geometry.shape = {
            points: pointses
          , glyph : ("glyph" in geometry.shape) ? geometry.shape.glyph : GLYPH_Cube
          }
          break
        }

      case GEOMETRY_Polylines:
        {
          const polylines = []
          let i = 0
          deltaGeometry.getCntsList().forEach(function(n) {
            const polyline = []           
            for (let j = 0; j < n; ++j)
              polyline.push(vec3.fromValues(posx[i], posy[i], posz[i++]))
            polylines.push(point)
          })
          geometry.shape = {
            polylines: polylines
//        , glyph    : ("glyph" in geometry.shape) ? geometry.shape.glyph : GLYPH_Cube
          }
          break
        }

      case GEOMETRY_Rectangles:
        {
          const rectangles = []
          let i = 0
          deltaGeometry.getCntsList().forEach(function(n) {
            rectangles.push([
              vec3.fromValues(posx[i], posy[i], posz[i++])
            , vec3.fromValues(posx[i], posy[i], posz[i++])
            , vec3.fromValues(posx[i], posy[i], posz[i++])
            ])           
          })
          geometry.shape = {
            rectangles: rectangles
          }
          break
        }

      case GEOMETRY_Label:
        {
          geometry.shape = {
            label: [
              vec3.fromValues(posx[0], posy[0], posz[0])
            , vec3.fromValues(posx[1], posy[1], posz[1])
            , vec3.fromValues(posx[2], posy[2], posz[2])
            ]           
          }
          break
        }

      case GEOMETRY_Axis:
        {
          geometry.shape = {
            axis: [
              vec3.fromValues(posx[0], posy[0], posz[0])
            , vec3.fromValues(posx[1], posy[1], posz[1])
            ]           
          }
          break
        }

    }

  }

  if ((mask & MASK_Size) != 0)
    geometry.size = deltaGeometry.getSize()

  if ((mask & MASK_Colr) != 0)
    geometry.color = deltaGeometry.getColr()

  if ((mask & MASK_Text) != 0)
    geometry.text = deltaGeometry.getText()

  if ((mask & MASK_Glyp) != 0 && (("points" in geometry.shape) || ("polylines" in geometry.shape)))
    geometry.shape.glyph = deltaGeometry.getGlyp()

}


module.exports = {

  GEOMETRY_Points     : GEOMETRY_Points     
, GEOMETRY_Polylines  : GEOMETRY_Polylines  
, GEOMETRY_Rectangles : GEOMETRY_Rectangles 
, GEOMETRY_Label      : GEOMETRY_Label      
, GEOMETRY_Axis       : GEOMETRY_Axis       

, GLYPH_Cube          : GLYPH_Cube
, GLYPH_Sphere        : GLYPH_Sphere
, GLYPH_Box           : GLYPH_Box  
, GLYPH_Cylinder      : GLYPH_Cylinder

, defaultGeometry     : defaultGeometry
, deltaColor          : deltaColor
, deltaGlyph          : deltaGlyph
, deltaPosition       : deltaPosition
, deltaSize           : deltaSize
, deltaText           : deltaText
, merge               : merge
}
