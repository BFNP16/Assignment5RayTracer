// Exercise 5.2 : Implement a module Point exposing the below signature. You can use type augmentation and type extension as explained in Chapter 7 of the F# book.

module Point

type Vector = Vector.Vector;;
type Point =
  | P of float * float * float
  override p.ToString() =
    match p with
      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"

let mkPoint x y z = P(x,y,z);;
let getX (P(x,_,_)) = x;;
let getY (P(_,y,_)) = y;;
let getZ (P(_,_,z)) = z;;
let getCoord (P(x,y,z)) = (x,y,z);;
let move (P(px,py,pz)) (V:Vector) = mkPoint (px + Vector.getX(V)) (py + Vector.getY(V)) (pz + Vector.getZ(V));;
let distance (P(px,py,pz)) (P(qx,qy,qz)) = Vector.mkVector (qx-px) (qy-py) (qz-pz);;
let direction p q = Vector.normalise(distance p q)
let round (P(px,py,pz)) (d:int) = P(System.Math.Round(px,d),System.Math.Round(py,d),System.Math.Round(pz,d));;

//module Point
//
//type Vector = Vector.Vector
//type Point =
//  | P of float * float * float
//  override p.ToString() =
//    match p with
//      P(x,y,z) -> "("+x.ToString()+","+y.ToString()+","+z.ToString()+")"
//
//let mkPoint x y z = ...
//let getX (P(x,_,_)) = ...
//let getY (P(_,y,_)) = ...
//let getZ (P(_,_,z)) = ...
//let getCoord (P(x,y,z)) = ...
//let move ...
//let distance (P(px,py,pz)) (P(qx,qy,qz)) = ...
//let direction p q = Vector.normalise(distance p q)
//let round (P(px,py,pz)) (d:int) = ...
