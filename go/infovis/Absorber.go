package infovis


import (
  "log"
)


type Absorber struct {
  label   Label
  channel ProtobufChannel
  exit    bool
}


func NewAbsorber(label Label, verbose bool) *Absorber {

  var this = Absorber {
    label  : label                ,
    channel: make(ProtobufChannel),
    exit   : false                ,
  }

  go func() {
    for !this.exit {
      buffer, ok := <-this.channel
      if !ok {
        this.exit = true
        continue
      }
      if verbose {
        log.Println("Absorber", this.label, "received", len(buffer), "bytes.")
      }
    }
    if verbose {
      log.Println("Absorber", this.label, " closed.")
    }
    close(this.channel)
  }()

  return &this

}


func (this *Absorber) Label() Label {
  return this.label
}


func (this *Absorber) In() *ProtobufChannel {
  return &this.channel
}


func (this *Absorber) Exit() {
  this.exit = true
}


func (this *Absorber) Alive() bool {
  return !this.exit
}
