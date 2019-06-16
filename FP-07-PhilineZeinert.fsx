/// ###########################################################################
/// ######################### Functional  Programming #########################
/// ############################  Assignment  7  ##############################
/// ###########################################################################
/// Author: Philine Zeinert <phze@itu.dk>
///
/// Notes:
/// - The anonymous let bindings below functions (the
///   `let _ = <some function name> : <some type>` statements) enforce type
///   constraints on functions given in the assignments (i.e. ensure that your
///   functions have the correct types) and are meant as a help to you.
///   If the function cannot be cast to the type specified in the assignment
///   text, a (type mismatch) error will be raised.
/// - The actual exercises start below the sections that have clearly been
///   marked as defining helper functions and type definitions.
/// - Mono does not have any apparent way of enabling tail-recursion
///   elimination, meaning:
///   1. It is way slower than it is supposed to be
///   2. StackOverflows can happen even in continuation-based tail-recursive
///      functions.
module a7

// Note: submission does not pass all tests on CodeJudge: There are errors in 7.2, 7.5 and 7.6 - I wrote the questions/ problems above the exercises


(* Exercise 7.1 (HR 9.1) *)

(* Reproduce the stack after a call, `g 2`, where `g` is the following:
 *)
  //let xs = [1;2];;

  //let rec g = function
    //  | 0 -> xs
    //  | n -> let ys = n::g(n-1)
    //         List.rev ys;;
  //let result = g 2;;


// push stack frame sf0 
// sf0:
// xs -> heap: [1;2] 
// g [result: ?] "closure for g"

// execution g 2: push stack frame sf1
// 2 
// xs -> heap: [1;2]
// ys -> heap: [2] point to g 1
// rev -> heap: g 1 point to [2]

// execution g 1: push stack frame sf2
// 1 
// xs -> heap: [1;2]
// ys -> heap: [1] point to g 0
// rev -> heap: g 0 point to [1]

//execution g 0: push stack frame sf3
// 0 
// xs -> heap: [1;2]

//pop g 0 : delete stack frame sf3
// 0
// xs -> heap: [1;2]

//pop g 1 : delete stack frame sf2
// 1 
// xs -> heap: [1;2]
// ys -> heap: [1] point to [1;2] //link is made from [1] to xs -> garbage collector in the next step
// rev -> heap: [2] point to [1] point to [1] // copy is made

//pop g 2 : delete stack frame sf1
// 2 
// xs -> heap: [1;2]
// ys -> heap: [2] point to [2;1;1] //link is made from [2] to rev of g 1 -> garbage collector in the next step
// rev -> heap: [1] point to [1] point to [2] point to [2] //copy is made

// pop sf0:
// xs -> [1;2]
// g [result] -> heap: [1;1;2;2]



(* Exercise 7.2 (HR 9.3) *)
// m+1 + m+2 + m+3 +....+ m+n
let rec g (m,n) = let rec aux n' acc = 
                    if n'=0 then acc+m else aux (n'-1) (m+n'+acc)
                  aux n 0;;
                  
let _ = g : int * int -> int
let result3 = g (3,2);;
let result4 = g (7,8);;
//3+2+2+2+2+1 = 12


(* Exercise 7.3 (HR 9.4) *)
//one iteration is enough

/// Compute the length of a list
 
/// tail-recursive solution
let rec length (l,n) = match l with
                  | [] -> n
                  | _::rest -> length (rest,n+1);;
let _ = length : 'a list * int -> int;;

/// is not recursive
let rec length3 (l,n) = let mutable sum = n
                        let f x = sum <- sum+1
                        List.map f l
                        sum;;


//alternative recursive approach but not one iteration only
let rec length2 (l,n) = match l with
                  | [] -> n
                  | _::rest -> 1 + length2 (rest,n);;


length([1;2;3],17);;



(* HELPERS *)
// fact 15 is the last factorial sequence index that will fit in a 32-bit integer
let xs15 = List.init 1000000 (fun i -> 15)
(* END OF HELPERS *)


(* Exercise 7.4 (HR 9.6) *)

/// Compute the n'th factorial number using continuations
let rec factC n c = if n=0 then c(1) else factC (n-1) (fun result -> c(n*result));;
let _ = factC : int -> (int -> int) -> int

#time;;
for i in xs15 do let reult4 = factC i id in ();;
#time;;

// TODO Write some performance analysis and comparison to factA here...

  //time factA
let rec factA = function
        | (0,m) -> m
        | (n,m) -> factA(n-1,n*m);;

#time;;
for i in xs15 do let reult5 = factA(i,1) in ();;
#time;;

  //result factA: Real: 00:00:00.044, CPU: 00:00:00.041, GC gen0: 0, gen1: 0
  //result factC n c: Real: 00:00:00.375, CPU: 00:00:00.372, GC gen0: 119, gen1: 0

//-> factA is much faster, because no data structure needs to be built in the heap.
// Nonetheless for the continuation version, a stack overflow can be avoided by just creating one stack framework.


(* Exercise 7.5 (HR 8.6) *)

/// Compute the `nth` number of the Fibonacci sequence using a while loop.
let whFib n = let mutable n' = n
              let mutable second = 0
              let mutable first = 1
              let mutable result = 0
              if n<0 then 1 else 
              while n'>1 do //that it executes also the root within the while loop
                result <- second + first
                second <- first
                first <- result
                n' <- n'-1
              result;;
let _ = whFib : int -> int
whFib 5;;


(* Exercise 7.6.1 (HR 9.7.1) *)

let rec fibA n f1 f2 = if n>1 then fibA (n-1) (f1+f2) f1 else (if n<>0 then f1+f2 else 0); 
let _ = fibA : int -> int -> int -> int
let result8 = fibA 10 1 0;;


(* Exercise 7.6.2 (HR 9.7.2) *)
let rec fibC (n:int) (c:int->int) = if n=0 then c(0) else if n=1 then c(1) else fibC (n-2) (fun a -> fibC (n-1) (fun b -> c (a+b)));;
let _ = fibC : int -> (int -> int) -> int
let result9 = fibC 10 id;;

(* HELPERS *)
let xs45 = List.init 1000000 (fun i -> 45);;
#time;;
for i in xs45 do let _ = whFib i in ();;
for i in xs45 do let _ = fibA i 1 0 in ();;
for i in xs45 do let _ = fibC i id in ();;
#time;;
(* END OF HELPERS *)

// performance analysis:
//result whFib: Real: 00:00:00.053, CPU: 00:00:00.055, GC gen0: 0, gen1: 0
//result fibA: Real: 00:00:00.070, CPU: 00:00:00.068, GC gen0: 0, gen1: 0
//result fibC: loading too long..?
