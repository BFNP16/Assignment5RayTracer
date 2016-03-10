module PointTest

open System.Threading
open System.Globalization
Thread.CurrentThread.CurrentCulture <- CultureInfo.InvariantCulture

let chk (name,t,r) =
  printf "%s %s\n" name (if t = r then "OK" else "FAILED[t="+(string)t+",r="+(string)r+"]")

let p1 = Point.mkPoint 1.0 1.5 2.5
let p2 = Point.mkPoint -2.5 -3.0 1.5
let p3 = Point.mkPoint -2.355432 -3.564523 443.23245

let v1 = Vector.mkVector 1.0 2.5 -3.5

let tests =
  [("Test01",(string)p1,"(1,1.5,2.5)");
   ("Test02",(string)(Point.getX p1),"1");
   ("Test03",(string)(Point.getY p1),"1.5");
   ("Test04",(string)(Point.getZ p1),"2.5");
   ("Test05",(string)(Point.getCoord p1),"(1, 1.5, 2.5)");
   ("Test06",(string)(Point.move p1 v1),"(2,4,-1)");
   ("Test07",(string)(Point.distance p1 p2),"[-3.5,-4.5,-1]");
   ("Test08",(string)(Vector.round (Point.direction p1 p2) 3),"[-0.605,-0.777,-0.173]");
   ("Test09",(string)(Point.round p3 3),"(-2.355,-3.565,443.232)")
   ]

let doTest() =
  printf "PointTest\n"
  List.iter chk tests

let _ = doTest()
