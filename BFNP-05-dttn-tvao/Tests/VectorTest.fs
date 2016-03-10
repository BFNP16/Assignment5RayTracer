module VectorTest

open System.Threading
open System.Globalization
Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

let chk (name,t,r) =
  printf "%s %s\n" name (if t = r then "OK" else "FAILED[t="+(string)t+",r="+(string)r+"]")

let v1 = Vector.mkVector 1.0 1.0 1.0
let v2 = Vector.mkVector -1.5 1.5 2.5

let tests =
  [("Test01",(string)v1,"[1,1,1]");
   ("Test02",(string)(-v1),"[-1,-1,-1]");
   ("Test03",(string)(v1+v2),"[-0.5,2.5,3.5]");
   ("Test04",(string)(v1-v2),"[2.5,-0.5,-1.5]");
   ("Test05",(string)(2.5*v2),"[-3.75,3.75,6.25]");
   ("Test06",(string)(v1*v2),"2.5");
   ("Test07",(string)(Vector.getX v2),"-1.5");
   ("Test08",(string)(Vector.getY v2),"1.5");
   ("Test09",(string)(Vector.getZ v2),"2.5");
   ("Test10",(string)(Vector.getCoord v2),"(-1.5, 1.5, 2.5)");
   ("Test11",(string)(Vector.multScalar v2 2.0),"[-3,3,5]");
   ("Test12",(string)(System.Math.Round(Vector.magnitude v2,3)),"3.279");
   ("Test13",(string)(System.Math.Round(Vector.dotProduct v1 v2,3)),"2.5");
   ("Test14",(string)(Vector.crossProduct v1 v2),"[1,-4,3]");
   ("Test15",(string)(Vector.round (Vector.normalise v2) 3),"[-0.457,0.457,0.762]")
   ]

let doTest() =
  printf "VectorTest\n"
  List.iter chk tests

//let _ = doTest()

  
