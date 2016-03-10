// Interface of module 

module Point
//[<Sealed>]

type Vector = Vector.Vector
type Point

val mkPoint : float -> float -> float -> Point
val getX : Point -> float
val getY : Point -> float
val getZ : Point -> float
val getCoord : Point -> float * float * float
val move : Point -> Vector -> Point
val distance : Point -> Point -> Vector
val direction : Point -> Point -> Vector
val round : Point -> int -> Point

