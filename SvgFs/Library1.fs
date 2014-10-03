namespace SvgFs
 open Svg
 module Loader=
  let openSv (s:string)=
    SvgDocument.Open s
  type flo=float32
  type FP=(flo*flo)
  let inline (.*) i (f) = fst f*i    , snd f*i
  let inline (.+) f (i) = fst f+fst i, snd f+snd i
  let private po=
    4. .* (2.,3.)
  let private pp=
    (3.,4.) .+ po
  //[<Struct>]
//  type Cu=
//    inherit Pa
//    {
//      P1:FP
//      P2:FP
//      P3:FP
//      P4:FP
//    }
  type Path=
    |Curve of (flo*flo) * (flo*flo) * (flo*flo) * FP
    |Close of unit//FP*FP
    |Move  of FP * FP
  let private ptoTup (p:System.Drawing.PointF)=
    p.X,p.Y
  let private pathData (p:Pathing.SvgPathSegment)=
    match p with
    | :? Pathing.SvgCubicCurveSegment as cur->Curve(ptoTup cur.Start,ptoTup cur.End,ptoTup cur.FirstControlPoint,ptoTup cur.SecondControlPoint)
    | :? Pathing.SvgClosePathSegment  as close->Close() //(ptoTup close.Start,ptoTup close.End)
    | :? Pathing.SvgMoveToSegment     as move ->Move(ptoTup move.Start,ptoTup move.End)
  let test()=
    let el = openSv "squid.svg"
    [for i in el.Children ->
      match i with
      | :? SvgPath as path ->[for j in path.PathData -> pathData j]
      |_->[]
    ]
  let openGimpPath p=
    let el = openSv p
    [for i in el.Children ->
      match i with
      | :? SvgPath as path ->[for j in path.PathData -> pathData j]
      |_->[]
    ]
  let getMove =function
    |Curve(p1,p2,p3,p4)->[|fst p1;snd p1
                           fst p2;snd p2;
                           fst p3;snd p3;
                           fst p4;snd p4;
                         |]
    |Move(p1,p2)->[|fst p1;snd p1
                    fst p2;snd p2;|]
    |Close()->[||]
  let private zero = 0.0f
  let private one  = 1.0f
  let private three= 3.0f
  let private tt t=
    three * (one-t)* (one-t)* t
  let private t3 t=
    three*t*t*(one-t)
  let private ttt t=t*t*t
  let bejy p pp ppp pppp t=
    (ttt (one - t)) .* p .+ ((tt t) .* pp) .+ ((t3 t) .* ppp) .+ (ttt t .* pppp)
  let lerpP p pp t=
    ((one - t) .* p) .+ (t.* pp)
  let lerp  x xx t=
    ((one - t) * x) + (t * xx)
  let mutable lastMove  = (zero,zero)
  let mutable lastPoint = (zero,zero)
  let private pathBej p =
       match p with
       |Curve(p1,p2,p3,p4)->lastPoint<-p2;bejy p1 p3 p4 p2
       |Move(p1,p2)       ->lastMove<-(p1);lerpP p1 p2 //move->curve->moveときたらLine型にしてやれば
       |Close()           ->lerpP lastPoint lastMove
    
  let pathFuns (p:Path[])=
    [|for i in p->pathBej i
    |]
  let pathToPoints p c=
    pathFuns p
     |> Array.map(
      fun i->[|0.0f..c..one|]|> Array.map(fun j->[|fst <|i j;snd <|i j|])
              |>Array.concat)
  //上はコピー発生しまくる実装
  let concat =List.concat   
  let map    =List.map
  let pathToPointsLis (p:Path list) c=
    let map l = map l [zero..c..one]
    let pathToLis p =
      let b = pathBej p|>map;
      b|>List.map(fun p->[fst p;snd p]) |> concat//|>Array.ofList
    p|>List.map(pathToLis) |> concat |> Array.ofList
  let lerpPath p pp a=
    match p,pp with
    |Curve(i,ii,i3,i4),Curve(j,jj,j3,j4)->
      Curve( lerpP i j a , lerpP ii jj a , lerpP i3 j3 a , lerpP i4 j4 a )
    |Close(),Close()->Close()
    |Move(i,ii),Move(j,jj)->
      Move ( lerpP i j a , lerpP ii jj a )

  type Paths=Path list list
  let anim (p:Paths) a= //1->2 2->3と決め打ちなら
    let p1 = p.[0]
    let p2 = p.[1]
    let aget p i=List.nth p i
    [0..p1.Length-1] |> map(fun i->lerpPath (aget p1 i) (aget p2 i) a)
  //[<EntryPoint>]
  let main args=
    //let u=test().[0];
    let u  = openGimpPath "squid.svg"
    let xs =
     [for i in pathToPointsLis u.[0] 0.2f do
       ()//for j in i -> fst j
     ]
    let ys=
     [for i in pathToPointsLis u.[0] 0.2f do
       ()//for j in i -> snd j
     ]
    //0の時クローズ、+=3
    xs|>List.iter (printfn "%f,")
    printfn "%s" "ys=c("
    ys|>List.iter (printfn "%f,")

    6