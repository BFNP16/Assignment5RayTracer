// Exercise 5.1 Implement a module Vector exposing the below signature. You can use type augmentation and type extension as explained in Chapter 7 of the F# book.

module Vector

type Vector =
  | V of float * float * float
  override v.ToString() =
    match v with
      V(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"

let mkVector x y z = V(x,y,z);;
let getX (V(x,_,_)) = x;;
let getY (V(_,y,_)) = y;;
let getZ (V(_,_,z)) = z;;
let getCoord (V(x,y,z)) = (x,y,z);;
let multScalar (V(x,y,z)) s = V(x*s,y*s,z*s);;
let magnitude (V(x,y,z)) = System.Math.Sqrt((x*x) + (y*y) + (z*z));;
let dotProduct (V(x1,y1,z1)) (V(x2,y2,z2)) = (x1*x2)+(y1*y2)+(z1*z2);;
let crossProduct (V(x1,y1,z1)) (V(x2,y2,z2)) =  V((y1*z2)-(z1*y2),(z1*x2)-(x1*z2),(x1*y2)-(y1*x2));;
let normalise (V(x,y,z) as v) =
                        let mag = magnitude v
                        V(x/mag,y/mag,z/mag);;
let round (V(x,y,z)) (d:int) = V(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d));;

type Vector with
  static member ( ~- ) (V(x,y,z)) = V(-x,-y,-z)
  static member ( + ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux+vx,uy+vy,uz+vz)
  static member ( - ) (V(ux,uy,uz),V(vx,vy,vz)) = V(ux-vx,uy-vy,uz-vz)
  static member ( * ) (s,(V(ux,uy,uz) as v)) = multScalar v s
  static member ( * ) ((V(ux,uy,uz) as u), (V(vx,vy,vz) as v)) = dotProduct u v;;

//module Vector
//
//type Vector =
//  | V of float * float * float
//  override v.ToString() =
//    match v with
//      V(x,y,z) -> "["+x.ToString()+","+y.ToString()+","+z.ToString()+"]"
//
//let mkVector x y z = ...
//let getX (V(x,_,_)) = ...
//let getY (V(_,y,_)) = ...
//let getZ (V(_,_,z)) = ...
//let getCoord ...
//let multScalar ...
//let magnitude ...
//let dotProduct ...
//let crossProduct ...
//let normalise (V(x,y,z) as v) = ...
//let round (V(x,y,z)) (d:int) = V(System.Math.Round(x,d),System.Math.Round(y,d),System.Math.Round(z,d))
//
//type Vector with
//  static member ( ~- ) (V(x,y,z)) = V(-x,-y,-z)
//  static member ( + ) (V(ux,uy,uz),V(vx,vy,vz)) = ...
//  static member ( - ) (V(ux,uy,uz),V(vx,vy,vz)) = ...
//  static member ( * ) ...
//  static member ( * ) ...
