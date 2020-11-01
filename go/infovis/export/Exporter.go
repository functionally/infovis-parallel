package export


import (
  "fmt"
  "io/ioutil"
  "github.com/qmuntal/gltf"
  "github.com/golang/glog"
  "github.com/golang/protobuf/proto"
  "bitbucket.org/bwbush/infovis-parallel/go/infovis/protobuf"
)


func Export(filenames []string) {

  var manager = makeManager()

  for _, filename := range filenames {
    if buffer, err := ioutil.ReadFile(filename); err != nil {
      glog.Errorf("Reading file %s encountered %v.\n", filename, err)
    } else {
      request := protobuf.Request{}
      if err = proto.Unmarshal(buffer, &request); err != nil {
        glog.Errorf("Could not unmarshal %s: %v.\n", filename, err)
      } else {
        manager.insert(request.Upsert)
      }
    }
  }

  for fram, frame := range manager.Frames {
    fmt.Printf("Frame %d\n", fram)
    for mesh, display := range frame.Displays {
      fmt.Printf("  Display %d\n", mesh)
      for iden, geometry := range display.Geometries {
        fmt.Printf("    Geometry %d\n", iden)
        fmt.Printf("      %v\n", geometry)
      }
    }
  }

  doc := gltf.NewDocument()
  buffers, _ := makeBuffers(doc)
  doc.Meshes = []*gltf.Mesh{
    buffers[cubeMesh     ].makeMesh(doc, [4]float32{1.0, 0.0, 0.0, 1.0}),
    buffers[sphereMesh   ].makeMesh(doc, [4]float32{0.0, 1.0, 0.0, 0.9}),
    buffers[polylineMesh ].makeMesh(doc, [4]float32{0.0, 0.0, 1.0, 0.8}),
    buffers[rectangleMesh].makeMesh(doc, [4]float32{1.0, 0.0, 0.0, 1.0}),
    buffers[axisMesh     ].makeMesh(doc, [4]float32{0.0, 1.0, 0.0, 0.9}),
  }
  doc.Nodes = []*gltf.Node{
    {
      Name       : "Cube",
      Mesh       : gltf.Index(0),
      Scale      : [3]float32{10, 10, 10},
      Translation: [3]float32{ 0,  0,  0},
    },
    {
      Name       : "Icosahedron",
      Mesh       : gltf.Index(1),
      Scale      : [3]float32{10, 10, 10},
      Translation: [3]float32{20,  0,  0},
    },
    {
      Name       : "Tube",
      Mesh       : gltf.Index(2),
      Scale      : [3]float32{40, 10, 10},
      Translation: [3]float32{ 0,  0, 20},
    },
    {
      Name       : "Square",
      Mesh       : gltf.Index(3),
      Scale      : [3]float32{10, 10, 10},
      Translation: [3]float32{ 0, 20,  0},
    },
   {
     Name       : "Arrow",
     Mesh       : gltf.Index(4),
     Scale      : [3]float32{40, 10, 10},
     Translation: [3]float32{ 0, 20, 20},
   },
  }
  doc.Scenes[0].Nodes = []uint32{0, 1, 2, 3, 4}
  if err := gltf.SaveBinary(doc, "./example.glb"); err != nil {
    panic(err)
  }
}
