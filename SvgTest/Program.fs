// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Windows.Forms
open SvgFs
open Eng
type Enable=EnableCap
module Main=
 let oneShot f=
   let called = ref false
   fun ()->if not !called then f() ; called := true
 let dum (fs:float32[]) =
   for i in fs do printfn "%f," i;
   printfn "%s" "ys=c("
 //0.2なら78
 let dumpX   = Array.zeroCreate 500
 let dumpY   = Array.zeroCreate 500
 let oneDump = oneShot (fun ()->dum dumpX;dum dumpY)
 let oninit()=
   GL.Enable(Enable.Texture2D);
   GL.Enable(Enable.Blend);
   GL.Enable(Enable.DepthTest)
   GL.Enable(Enable.ColorMaterial)
   GL.Enable(Enable.PointSprite)
   ()
 let thre  = 3.0f
 let one   = 1.0f
 let zero  = 0.0f
 let zerod = 0.0
 let tt t=
  thre * (one-t)* (one-t)* t
 let t3 t=
  thre*t*t*(one-t)
 let ttt t=t*t*t
 let bejx xx xxx t=
  (tt t) * xx * (t3 t) * xxx+ttt(t)
 type FLIS<'a>  = Microsoft.FSharp.Collections.FSharpList<'a>
 
 let ptoPoi c p = Loader.pathToPointsLis(p,c)
 //ポイントの配列、アニメーション
 let pointFromFloatArAr (fxx:float32[][]) t div = //divは使用しない
   let a = (
          //bejx one one 
            t) * 19.0f |> int
   Array.get fxx a
 //ポイントのリスト、アニメーションの時間0~1 分解率をとって GLVertexのポインタを返す
 let pointFromPath poi t div = //[|-0.9f ; 0.9f ; 0.9f ; 0.9f ; 0.0f ; -0.9f|]
  let anim = bejx one one t
  try
    Loader.anim(poi,t)|>ptoPoi div
  with
    _->System.Windows.MessageBox.Show("パスが一つしか含まれていません")|>ignore;[||]
 #if PVIEW
 type Paths = float32[][]
 #else
 type Paths= Loader.Path FLIS FLIS
 #endif
 type MovingToward=
   |Left
   |Right
   |TurnLeft
   |TurnRight
 let private epsilon = 0.003f
 let bejOne          = bejx one one
 let nearby t max min=
   abs(t-min)<epsilon || abs(t-max)<epsilon
 type Game(point:Paths , onQuit:float32->float32->float32->unit) as x=
   inherit OpenTK.GameWindow()
   do oninit()
   do Application.EnableVisualStyles();
   do Application.SetCompatibleTextRenderingDefault(false);
   let mutable cx      = 19.0f
   let mutable speed   = 1.0f
   let mutable roty    = 0.0f
   let walkmin,walkmax = 18.0f,200.0f
   let mutable mov     = Left
   let mutable rotCount= 90
   let rotWait         = 90
   let mutable anim    = 0.0f
   let mutable inanim  = false
   let mutable div     = 0.06f
   let mutable py=0.0f

   let begins=
     [BeginMode.LineLoop;BeginMode.LineStrip;
      BeginMode.LineStripAdjacency;BeginMode.Lines;
      BeginMode.Polygon;BeginMode.QuadStrip;
      BeginMode.Quads;BeginMode.TriangleFan;
      BeginMode.TriangleStrip]
   let mutable bi = 0
   #if PVIEW
   let p() = pointFromFloatArAr point anim 0//[|-0.9f ; 0.9f ; 0.9f ; 0.9f ; 0.0f ; -0.9f|]
   #else
   let p() = pointFromPath point anim div
   #endif
   let bai    = 210.f
   let elem c = [|0..c|] //|>Array.map(uint8) 
              //|>Array.collect(fun i->[|i;i+1|])
   do x.UpdateFrame.Add(x.UpDate)
   do x.RenderFrame.Add(x.Draw)
   do x.Closing.Add(fun e->onQuit cx py div)
   let varray()=
    GL.EnableClientState(ArrayCap.VertexArray)
    let pr    = p()
    GL.VertexPointer( 2 , VertexPointerType.Float , 0 , pr )
    let elemType=DrawElementsType.UnsignedInt
    let begi  = begins.[bi]
    x.Title<-begi.ToString("g")
    let elem  = elem (pr.Length-1)
    GL.DrawElements( begins.[bi] , elem.Length/2 , elemType , elem )

   let oldBeg()=
    let begi  = begins.[bi]
    GL.Begin(begi)
    let ri    = ref 0
    let addi()=ri := !ri+2
    let cp    =p()
    while !ri<cp.Length do
      let i = !ri
      let x = Array.get cp i
      let y = Array.get cp <|(i)+1
      let x = x /bai
      let y = -y/bai+0.5f
      Array.set dumpX (i/2) x
      Array.set dumpY (i/2) y
      GL.Vertex2(x,y)
      addi()
    //oneDump()
    GL.End()
   let mutable beforePush = false
   let walkAndRotMachine()=
    if rotCount > 0 then
      match mov with
      |Right    -> cx<-cx+speed 
      |Left     -> cx<-cx-speed
      |TurnLeft -> cx<-cx-speed
      |TurnRight-> cx<-cx+speed
      if cx>walkmax then
        mov     <- TurnLeft
        speed   <- 0.01f
        rotCount<- rotCount-1
        roty    <- bejOne <|float32(rotWait-rotCount)/(float32 rotWait)
        roty    <- (1.0f-roty)* 180.0f
      if cx<walkmin then
        mov     <- TurnRight
        speed   <- 0.01f
        rotCount<- rotCount-1
        roty    <- bejOne <|float32(rotWait-rotCount)/(float32 rotWait)
        roty    <- roty*180.0f
    else 
      rotCount<-rotWait
      speed   <-1.0f
      if mov=TurnLeft then mov<-Left else mov<-Right

   member x.UpDate e=
    Inputter.Poll()
    let state=Eng.Inputter.State
    //walkAndRotMachine()
    if(not state.Deside)        then beforePush<-false
    if(state.Left  && div>0.03f)then div<-div-0.001f
    if(state.Right && div<1.0f) then div<-div+0.001f
    if(state.Up)   then py<-py+0.8f
    if(state.Down) then py<-py-0.8f
    if(state.Deside && not beforePush)then bi<-bi+1 ; beforePush<-true
    let animf=0.05f
    if(not inanim)then anim<-anim+animf else anim<-anim-animf
    if(anim>1.00f)then inanim<-not inanim
    if(anim<0.00f)then inanim<-not inanim
    ()
      //printfn "%f" Inputter.JoyState.Stick
      //printfn "%b" Inputter.State.Down
   member x.Draw e=
    //x.MakeCurrent()
    GL.Clear( ClearBufferMask.ColorBufferBit ||| ClearBufferMask.DepthBufferBit )
    GL.MatrixMode(MatrixMode.Projection)
    GL.LoadIdentity()
    let wh = 187.0
    let w  = 380.0
    
    GL.Ortho( zerod , w , w , zerod , -100.0 ,100.0 )
    GL.MatrixMode(MatrixMode.Modelview)
    GL.LoadIdentity()
    let zax = new OpenTK.Vector3(zero,zero,one)
    let yax = new OpenTK.Vector3(zero,one,zero)
    let mx  = float32(Inputter.mouseX)
    let cx  = float cx
    GL.Translate( cx , w/2.0 , 0.0 )
    GL.Translate(0.0f,  py   , 0.0f)
    GL.Rotate( roty , yax ) //時計回りdeg
    //GL.Translate(-cx,-w/2.0,0.0)
    printfn "%d" Inputter.mouseX
    //oldBeg()
    varray()
    x.SwapBuffers()
 let arrayPrint xs=
   let len = Array.length xs
   let ax  = Array.zeroCreate <| len/2
   let ay  = Array.zeroCreate <| len/2
   for i in [0..2..len-1] do
     Array.set ax (i/2) (Array.get xs i)
     Array.set ay (i/2) (Array.get xs <|i+1)
   
   [0 .. len/2-1 ]|> Seq.iter(fun i-> printfn "%f," (Array.get ax i))
   printfn "%s" "ys=c("
   [0 .. len/2-1 ]|> Seq.iter(fun i-> printfn "%f," (Array.get ay i))
 let loadBynaryAnimFromFile name=
   ()

 let saveFile (input:string)(points:float32->float32->float32[]) div=

  let tob (si:float32)=System.BitConverter.GetBytes(si)
  let getByte (i:int)= System.BitConverter.GetBytes(i)
  
  let fls      = [for i in [0.0f..0.05f..1.0f] -> //pointsに渡すとベジエ補完された値になって帰ってくる
                  points i div |> Array.toList |> List.map(tob) |> Array.concat]//|>List.ofArray]
  let len      = fls.[0].Length/4|>getByte //byteなので4で割る

  let l        = fls.[0].Length/4
  let flamenum = fls.Length
  let header   = Array.append (flamenum|>getByte) (len)
  let destName = if input.Contains(".") then input.Substring( 0 , input.Length-4 ) else input
  let destName = destName + "allFlame"
  System.IO.File.WriteAllBytes( destName , Array.append header<|Array.concat fls)
  //let fla=Loader.pathToPoints((p),0.2f) |>Array.concat
  //arrayPrint fla


 #if PVIEW
  use stre = new System.IO.StreamReader(destName)
  use fb   = new System.IO.BinaryReader(stre.BaseStream)
  let flamenum=fb.ReadInt32()
  let siz  = fb.ReadInt32() //1フレームのサイズ
  let makePad()=new byte()
  let tof i (b:byte[])=System.BitConverter.ToSingle(b,i)
  let byt  = //Array.create flamenum 
    [0..flamenum]|>List.map(fun i-> Array.create siz ( makePad() ))
  let makeList f=
    [|for i in [0..flamenum-1] ->
      f i
    |]
    
    //fb.Read(aget byt i,i*siz,siz-1)
  let fls = makeList(fun i->
    let bs    = fb.ReadBytes(siz*4) //1フレーム全部を読む
    let x     = ref 0
    let upx() = x:= !x+4
    let ls    = ref <|Array.zeroCreate (siz)
    while !x < bs.Length do
      let v=tof (!x) bs
      //ls:= v:: !ls
      Array.set !ls (!x/4) v//20thが空に
      upx()
    !ls
  )
  fls
 #else
 #endif
 let loadAndView( argv : string[])=
  let input = if argv.Length=0 then "dragon.svg" else (Array.get argv 0)
  let ps    = Loader.openGimpPath(input)
  let aget  = //Array.get
    List.nth
  //let p     = ps.[0]
  let anim an    = bejx one one an
  let points i d = Loader.pathToPointsLis( Loader.anim( ps , anim i ) , d )
  //let f0,f2    = points zero , points 0.1f

  let win=new Game(

 #if PVIEW
                 saveFile input points
 #else
                 ps
               , fun cx py div->saveFile input points div
 #endif
      ) //ビューア起動
  win.Run(30.0)
 let wpfmain()=
  let c1() = new WpfControlLibrary1.UserControl1()
  let disp = System.Windows.Application.Current
  let disp = if disp=null then null else disp.Dispatcher
  let acce = true //disp.CheckAccess()
  if acce then
    let c = c1()
    c.ShowDialog()|>ignore
    ()
  else
    disp.Invoke(fun ()->c1()|>ignore)
 open System
 [<EntryPoint>]
 [<STAThread>]
 let main argv =
  let c=new BulletCLI.Class1()
  loadAndView argv
  0 // 整数の終了コードを返します
