module ExprParse

(* Grammar:

E    = T Eopt .
Eopt = "+" T Eopt | e .
T    = F Topt .
Topt = "*" F Topt | e .
F    = P Fopt .
Fopt = "^" Int | e .
P    = Int [ Float | Var | "(" E ")" .

e is the empty sequence.
*)

type terminal =
  Add | Mul | Pwr | Lpar | Rpar | Int of int | Float of float | Var of string

let isblank c = System.Char.IsWhiteSpace c
let isdigit c  = System.Char.IsDigit c
let isletter c = System.Char.IsLetter c
let isletterdigit c = System.Char.IsLetterOrDigit c

// Explode string into chars 
let explode s = [for c in s -> c]

let floatval (c:char) = float((int)c - (int)'0')
let intval(c:char) = (int)c - (int)'0'

exception Scanerror

// Used to differ floats/fractals from integers 
let rec scnum (cs, value) = 
  match cs with 
    '.' :: c :: cr when isdigit c -> scfrac(c :: cr, (float)value, 0.1)
  | c :: cr when isdigit c -> scnum(cr, 10* value + intval c)
  | _ -> (cs,Int value)    (* Number without fraction is an integer. *)
and scfrac (cs, value, wt) =
  match cs with
    c :: cr when isdigit c -> scfrac(cr, value+wt*floatval c, wt/10.0)
  | _ -> (cs, Float value)

let rec scname (cs, value) =
  match cs with
    c :: cr when isletterdigit c -> scname(cr, value + c.ToString())
  | _ -> (cs, value)

// Scan a sequence of characters and return a list of terminals 
let scan s =
  let rec sc cs = 
    match cs with
      [] -> []
    | '+' :: cr -> Add :: sc cr      
    | '*' :: cr -> Mul :: sc cr      
    | '^' :: cr -> Pwr :: sc cr
    | '(' :: cr -> Lpar :: sc cr     
    | ')' :: cr -> Rpar :: sc cr     
    | '-' :: c :: cr when isdigit c -> let (cs1, t) = scnum(cr, -1 * intval c)
                                       t :: sc cs1
    | c :: cr when isdigit c -> let (cs1, t) = scnum(cr, intval c) 
                                t :: sc cs1
    | c :: cr when isblank c -> sc cr
    | c :: cr when isletter c -> let (cs1, n) = scname(cr, (string)c)
                                 Var n :: sc cs1
    | _ -> raise Scanerror
  sc (explode s)

exception WrongFormatError // Used for cases where numbers are not separated by multiplicity 

let rec insertMult = function 
  Float r :: Var x :: ts -> Float r :: Mul :: insertMult (Var x :: ts) // "2.0 x x" 
| Float r1 :: Float r2 :: ts -> raise WrongFormatError  // Float r1 :: Mul :: Float r2 :: insertMult(Float r2 :: ts) // "2.0 * 2.0 x"
| Float r :: Int i :: ts -> raise WrongFormatError //Float r :: Mul :: insertMult(Int i :: ts) // "2.0 * 1x" ? -> raise WrongFormatError from exception WrongFormatError 
| Var x :: Float r :: ts -> Var x :: Mul :: insertMult (Float r :: ts) 
| Var x1 :: Var x2 :: ts -> Var x1 :: Mul :: insertMult (Var x2 :: ts) 
| Var x :: Int i :: ts -> Var x :: Mul :: insertMult (Int i :: ts) 
| Int i :: Float r :: ts -> raise WrongFormatError // "1 2.0 x" does not make sence without multiplication "1 * 2.0 x" 
| Int i :: Var x :: ts -> Int i :: Mul :: insertMult (Var x :: ts) 
| Int i1 :: Int i2 :: ts -> raise WrongFormatError // "1 2 x" does not make sence without multiplication "1 * 2 x" 
| Float r :: Lpar :: ts -> Float r :: Mul :: insertMult (Lpar :: ts)  
| Var x :: Lpar :: ts -> Var x :: Mul :: insertMult (Lpar :: ts) 
| Int i :: Lpar :: ts -> Int i :: Mul :: insertMult (Lpar :: ts) 
| t :: ts -> t :: insertMult ts
| [] -> []
  
type expr = 
  | FNum of float
  | FVar of string
  | FAdd of expr * expr
  | FMult of expr * expr
  | FExponent of expr * int

exception Parseerror

// Return ret of non terminals with currently built abstract syntax tree 

// Parses non terminals from E with different functions 
// >> Forward composition in (T >> Eopt) is equal to Eopt (T ts) where T is parsed initially and then used to parse Eopt 
let rec E (ts:terminal list) = (T >> Eopt) ts // ts is a list of terminal symbols, T is parsed and then Eopt is parsed 

and Eopt (ts, inval) = // inval is an accumulated value used to build the abstract syntax tree 
  match ts with
    |Add :: tr ->   let (x,y) = T tr // Parse T first 
                    Eopt (x, FAdd(inval,y)) // Parse Eopt using above T and the currently built abstract syntax tree 
    | _ ->         (ts, inval) // If list is empty (e) return current abstract syntax tree 

and T ts = (F >> Topt) ts

and Topt (ts, inval) = 
    match ts with
    | Mul :: tr -> let (x,y) = F tr 
                   Topt (x, FMult(inval,y)) // Parse Topt using above F and currently built abstract syntax tree 
    | _ ->        (ts, inval) // If list is empty (e) return current abstract syntax tree

and F ts = (P >> Fopt) ts 

and Fopt (ts,inval) = 
    match ts with
    |Pwr :: tr -> 
                 match tr with
                    | Int x :: tp -> (tp, FExponent(inval,x)) // Parse int x and rest of list 
                    | _ -> raise Parseerror
    | _ -> (ts, inval) // If list is empty (e) return current abstract syntax tree 

and P ts = 
    match ts with
    Int i :: tr -> (tr, FNum(float(i)))
    | Float f :: tr -> (tr, FNum(f))
    | Var s :: tr -> (tr, FVar(s))
    | Lpar :: tr ->
                    let (rest, exp) = E tr // Rest of list with exp being content of paranthesis 
                    match rest with
                    | Rpar :: r -> (r,exp) // If first element in rest is a right paranthesis then continue on r (rest without Rpar)
                    | _ -> raise Parseerror 
    | _ -> raise Parseerror ;; // should not parse ")x+2(" or "x+(x))" 

let parse ts = 
  match E ts with
    ([], result) -> result
  | _ -> raise Parseerror

let dotAST ast =
  let fixStr (s:string) = s.Replace ("\"", "\\\"")
  let genDot s n e = "digraph G {\nlabel=\"" + (fixStr s) + "\"\n" + n + e + "\n}"
  // i is unique label such that nodes and edges are unique in DiGraph.
  let genNodeStr i l = "Node"+(string i)+" [label=\""+l+"\"];\n"
  let genEdgeStr i1 i2 = "Node"+(string i1)+" -> " + "Node"+(string i2)+";\n"
  // Edges are unique and stored in a set.
  // Nodes are not unique and stored in a map, i.e., node with "+" may happen several times. 
  let rec genNE (i,nmap,eset) = function
    FNum r -> (i,Map.add i (genNodeStr i ((string)r)) nmap,eset)            // Add node with number
  | FVar x -> (i,Map.add i (genNodeStr i x) nmap,eset)                      // Add node with variable
  | FAdd (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1         // Generate nodes and edges for e1 and e2
                    let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                    (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "+") nmap2,                      // Add node for "+"
                     Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "+"->e1 and "+"->e2
  | FMult (e1,e2) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1        // Generate nodes and edges for e1 and e2
                     let (i2,nmap2,eset2) = genNE (i1+1,nmap1,eset1) e2
                     (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "*") nmap2,                      // Add node for "*"
                      Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset2))  // Add edge for "*"->e1 and "*"->e2
  | FExponent (e1,ie) -> let (i1,nmap1,eset1) = genNE (i+1,nmap,eset) e1                                // Generate nodes and edges for e1
                         let (i2,nmap2) = (i1+1,Map.add (i1+1) (genNodeStr (i1+1) ((string)ie)) nmap1)  // Add node for integer (exponent)
                         (i2+1,Map.add (i2+1) (genNodeStr (i2+1) "^") nmap2,                            // Add node for "^"
                          Set.add (genEdgeStr (i2+1) i2) (Set.add (genEdgeStr (i2+1) i1) eset1))        // Add edges for "^"->e1 and "^"->ie
  let (_,nmap,eset) = genNE (0,Map.empty,Set.empty) ast  // Generate map for nodes and set for edges
  genDot (sprintf "%A\n" ast) (Map.fold (fun acc _ s -> acc + s) "" nmap) (Set.fold (fun acc s -> acc + s) "" eset)  // Generate big string with dot-code.
  
