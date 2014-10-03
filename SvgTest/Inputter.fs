
namespace Eng
module Inputter=
  open OpenTK.Input
  type Button=
    { mutable Deside: bool
      mutable Cansel: bool
      mutable Up    : bool
      mutable Down  : bool
      mutable Left  : bool
      mutable Right : bool
      mutable EyeUp : bool
      mutable EyeDown:bool
      mutable EyeRight:bool
      mutable EyeLeft :bool

    }
  let mutable mouseX=0;
  let mutable mouseY=0;
  let State=
    {
      Deside=false
      Cansel=false
      Up=false
      Down=false
      Left=false
      Right=false
      EyeUp=false
      EyeDown=false
      EyeRight=false
      EyeLeft=false
    }
  let mutable Dev=null
  type Joy={
    mutable Stick:float32
    mutable Button:int
  }
  let JoyState={
    Stick=0.0f;
    Button=0;
  }
  
  open System
  //open Microsoft.DirectX
  //let joysticks=DirectInput.Manager.GetDevices(DirectInput.DeviceClass.GameControl,DirectInput.EnumDevicesFlags.AttachedOnly)
  let joy (dev:JoystickDevice)=
    dev.ButtonDown<-new EventHandler<JoystickButtonEventArgs>(fun o e->printfn "%s" (e.Button.ToString()))
    dev.Move<-new EventHandler<JoystickMoveEventArgs>(fun o e->JoyState.Stick<-e.Value)
    Dev<-dev

  let Poll()=
    let keystate=Keyboard.GetState()
    let mouse=Mouse.GetState()
    mouseX<- mouse.X
    mouseY<- mouse.Y
    let keyd=keystate.IsKeyDown
    State.Deside<-keyd(Key.Space)
    State.Cansel<-keyd(Key.Escape)
    State.Up<-keyd(Key.Up)
    State.Down<-keystate.IsKeyDown(Key.Down)
    State.Right<-keystate.IsKeyDown(Key.Right)
    State.Left<-keystate.IsKeyDown(Key.Left)
    State.EyeUp<-keystate.IsKeyDown(Key.W)
    State.EyeDown<-keystate.IsKeyDown(Key.S)
    State.EyeRight<-keyd Key.A
    State.EyeLeft<-keyd Key.D
//    for i in [0..5] do 
//      JoyState.Stick<-JoyState.Stick + (Dev.Axis.Item i )
//    for i in [0..Dev.Button.Count] do
//      JoyState.Button<-Dev.Button.Item i
      
//    let moved s (e:JoystickMoveEventArgs)=printfn "%f" e.Delta
//    let pushed s (e:JoystickButtonEventArgs)=printfn "%s" (e.Button.ToString())
//    dev.Move<-new EventHandler<JoystickMoveEventArgs>(moved)
//    dev.ButtonDown<-new EventHandler<JoystickButtonEventArgs>(pushed)
//      //printfn "%s" i
//    {new IDisposable with 
//      member x.Dispose()=
//       ()//dev.Move<-null;dev.ButtonDown<-null
//    } //クラスに入れないとGCの対象になるっぽい