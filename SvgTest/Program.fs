// F# の詳細については、http://fsharp.net を参照してください
// 詳細については、'F# チュートリアル' プロジェクトを参照してください。
open OpenTK.Graphics
open OpenTK.Graphics.OpenGL
open System.Windows.Forms
open SvgFs
type Enable=EnableCap
module Main=
 let oneShot f=
   let called=ref false
   fun ()->if not !called then f();called:=true
 let dum (fs:float32[])=
   for i in fs do printfn "%f," i;
   printfn "%s" "ys=c("
 //0.2なら78
 let dumpX=Array.zeroCreate 500
 let dumpY=Array.zeroCreate 500
 let oneDump=oneShot (fun ()->dum dumpX;dum dumpY)
 let oninit()=
   GL.Enable(Enable.Texture2D);
   GL.Enable(Enable.Blend);
   GL.Enable(Enable.DepthTest)
   GL.Enable(Enable.ColorMaterial)
   GL.Enable(Enable.PointSprite)
   ()
 let thre=3.0f
 let one=1.0f
 let zero=0.0f
 let tt t=
  thre * (one-t)* (one-t)* t
 let t3 t=
  thre*t*t*(one-t)
 let ttt t=t*t*t
 let bejx xx xxx t=
  (tt t) * xx * (t3 t) * xxx+ttt(t)
 type FLIS<'a>=Microsoft.FSharp.Collections.FSharpList<'a>
 type Paths= Loader.Path FLIS FLIS
 let ptoPoi c p=Loader.pathToPointsLis(p,c)

 type Game(point:Paths) as x=
   inherit OpenTK.GameWindow()
   do oninit()
   do Application.EnableVisualStyles();
   do Application.SetCompatibleTextRenderingDefault(false);
   let mutable anim=0.0f
   let mutable inanim=false
   let mutable div=0.06f
   let begins=
     [BeginMode.LineLoop;BeginMode.LineStrip;
      BeginMode.LineStripAdjacency;BeginMode.Lines;
      BeginMode.Polygon;BeginMode.QuadStrip;
      BeginMode.Quads;BeginMode.TriangleFan;
      BeginMode.TriangleStrip]
   let mutable bi=0
   let p()= //[|-0.9f ; 0.9f ; 0.9f ; 0.9f ; 0.0f ; -0.9f|]
    let anim=bejx one one anim
    Loader.anim(point,anim)|>ptoPoi div
   let bai=210.f
   let elem c=[|0..c|] //|>Array.map(uint8) 
              //|>Array.collect(fun i->[|i;i+1|])
   do x.UpdateFrame.Add(x.UpDate)
   do x.RenderFrame.Add(x.Draw)
   let varray()=
    GL.EnableClientState(ArrayCap.VertexArray)
    let pr=p()
    let mx,mn=Array.max pr,Array.min pr
    let prb=Array.map((/)bai) pr
    GL.VertexPointer(2,VertexPointerType.Float,0,pr)
    let elemType=DrawElementsType.UnsignedInt
    let begi=begins.[bi]
    let elem =elem (pr.Length-1)
    GL.DrawElements(begins.[bi],elem.Length/2,elemType,elem)

   let oldBeg()=
    let begi=begins.[bi]
    GL.Begin(begi)
    let ri=ref 0
    let addi()=ri:=!ri+2
    let cp=p()
    while !ri<cp.Length do
      let i= !ri
      let x=Array.get cp i
      let y=Array.get cp <|(i)+1
      let x=x/bai
      let y= -y/bai+0.5f
      Array.set dumpX (i/2) x
      Array.set dumpY (i/2) y
      GL.Vertex2(x,y)
      addi()
    //oneDump()
    GL.End()
   let mutable beforePush=false
   member x.UpDate e=
    Eng.Inputter.Poll()
    let state=Eng.Inputter.State
    if(not state.Deside)then beforePush<-false

    if(state.Left && div>0.03f)then div<-div-0.001f
    if(state.Right && div<1.0f)then div<-div+0.001f
    if(state.Deside && not beforePush)then bi<-bi+1;beforePush<-true
    let animf=0.05f
    if(not inanim)then anim<-anim+animf else anim<-anim-animf
    if(anim>1.00f)then inanim<-not inanim
    if(anim<0.00f)then inanim<-not inanim
    ()
      //printfn "%f" Inputter.JoyState.Stick
      //printfn "%b" Inputter.State.Down
   member x.Draw e=
    //x.MakeCurrent()
    GL.Clear(ClearBufferMask.ColorBufferBit|||ClearBufferMask.DepthBufferBit)
    GL.MatrixMode(MatrixMode.Projection)
    GL.LoadIdentity()
    let wh=187.0
    GL.Ortho(0.0,wh,0.0,wh,-1.0,1.0)
    GL.MatrixMode(MatrixMode.Modelview)
    GL.LoadIdentity()
//    GL.Begin(BeginMode.Lines)
//    GL.Vertex2(0,0);
//    GL.Vertex2(100,100);
//    GL.End()
    //oldBeg()
    varray()
    x.SwapBuffers()
 let arrayPrint xs=
   let len=Array.length xs
   let ax=Array.zeroCreate <|len/2
   let ay=Array.zeroCreate <|len/2
   for i in [0..2..len-1] do
     Array.set ax (i/2) (Array.get xs i)
     Array.set ay (i/2) (Array.get xs <|i+1)
   
   [0..len/2-1]|>Seq.iter(fun i->printfn "%f," (Array.get ax i))
   printfn "%s" "ys=c("
   [0..len/2-1]|>Seq.iter(fun i->printfn "%f," (Array.get ay i))
 [<EntryPoint>]
 let main argv =
  let ps=Loader.openGimpPath("squid3.svg")
  let aget= //Array.get
    List.nth
  let p=ps.[0]
  let f=95.0f
  let n=95
  let tob (si:float32)=System.BitConverter.GetBytes(si)

  let anim an=bejx one one an
  let points i=Loader.pathToPointsLis(Loader.anim(ps,anim i),0.06f)
  let f0,f2=points zero ,points 0.1f
  let fls=[for i in [0.0f..0.05f..1.0f] ->
             points i|>Array.toList|>List.map(tob)|>Array.concat]//|>List.ofArray]
  let len=fls.[0].Length/4|>System.BitConverter.GetBytes
  let l=fls.[0].Length/4
  System.IO.File.WriteAllBytes("oneFlame",fls.[0])
  System.IO.File.WriteAllBytes("allFlame",Array.append len <|Array.concat fls)
  //let fla=Loader.pathToPoints((p),0.2f) |>Array.concat
  //arrayPrint fla
　　//todoビューアでアニメーションバイナリ読み込めるようにしたい、C++でバイナリパスデータパースはちょっと勘弁願いたい

  let win=new Game(ps) //ビューア起動
  win.Run(30.0)
  0 // 整数の終了コードを返します
