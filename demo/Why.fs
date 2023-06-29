module Fsia.Demo.Why

(*
   ____                 _                              
  / ___|___  _ __   ___(_)___  ___ _ __   ___  ___ ___ 
 | |   / _ \| '_ \ / __| / __|/ _ \ '_ \ / _ \/ __/ __|
 | |__| (_) | | | | (__| \__ \  __/ | | |  __/\__ \__ \
  \____\___/|_| |_|\___|_|___/\___|_| |_|\___||___/___/
*)

// one-liners
[1..100] |> List.sum |> printfn "sum=%d"

// no curly braces, semicolons or parentheses
let square x = x * x
let sq = square 42

// simple types in one line
type Person = {First:string; Last:string}

// complex types in a few lines
type Employee =
  | Worker of Person
  | Manager of Employee list

// type inference
let jdoe = {First="John"; Last="Doe"}
let worker = Worker jdoe








(*
   ____                           _                     
  / ___|___  _ ____   _____ _ __ (_) ___ _ __   ___ ___ 
 | |   / _ \| '_ \ \ / / _ \ '_ \| |/ _ \ '_ \ / __/ _ \
 | |__| (_) | | | \ V /  __/ | | | |  __/ | | | (_|  __/
  \____\___/|_| |_|\_/ \___|_| |_|_|\___|_| |_|\___\___|
*)

// automatic equality and comparison
// type Person = {First:string; Last:string}
let person1 = {First="john"; Last="Doe"}
let person2 = {First="john"; Last="Doe"}
printfn "Equal? %A"  (person1 = person2)

// easy IDisposable logic with "use" keyword
// use reader = new StreamReader(..)

// easy composition of functions
let add2times3 = (+) 2 >> (*) 3
let result = add2times3 5










(*
   ____                         _                       
  / ___|___  _ __ _ __ ___  ___| |_ _ __   ___  ___ ___ 
 | |   / _ \| '__| '__/ _ \/ __| __| '_ \ / _ \/ __/ __|
 | |__| (_) | |  | | |  __/ (__| |_| | | |  __/\__ \__ \
  \____\___/|_|  |_|  \___|\___|\__|_| |_|\___||___/___/
*)

// strict type checking
// printfn "print string %s" 123 //compile error

// type Person = {First:string; Last:string}
// all values immutable by default
// person1.First <- "new name"  //assignment error

// never have to check for nulls
let makeNewString str =
   //str can always be appended to safely
   let newString = str + " new!"
   newString

[<Measure>]
type m

[<Measure>]
type s

[<Measure>]
type km

[<Measure>]
type h

[<Measure>]
type ft
// units of measure
// let distance = 10<m> + 10<ft> // error!

let mySpeed = 2.0<m/s>

let metersInKm = 1000.0<m/km>

let convertMetersToKm (dist: float<m>) = dist / metersInKm

let secondsInHour = 3600.0<s/h>

let convertSecondsToHour (seconds: float<s>) = seconds / secondsInHour

let toKmPerHour (spd: float<m/s>) =
   let metersPerHour = spd * secondsInHour
   metersPerHour / metersInKm

let mySpeedKmToHour = toKmPerHour mySpeed

let tenSeconds = 10.0<s>

let tenTimeSpan = System.TimeSpan.FromSeconds <| float tenSeconds

// Usage of Result type vs Exceptions
type UnvalidatedPerson = {
   First : string
   Last : string
}

type ParseError =
   | FirstMustNotBeEmpty
   | LastMustNotBeEmpty
let parseIntoDomain 
   (unvalidatedPerson: UnvalidatedPerson)
  : Result<Person, ParseError> =
   match unvalidatedPerson.First, unvalidatedPerson.Last with
   | null, _ | "", _ -> 
      Result.Error FirstMustNotBeEmpty
   | _, null | _, "" -> 
      Result.Error LastMustNotBeEmpty
   | first, last -> 
      Result.Ok { First = first; Last = last}














(*
   ____                                                      
  / ___|___  _ __   ___ _   _ _ __ _ __ ___ _ __   ___ _   _ 
 | |   / _ \| '_ \ / __| | | | '__| '__/ _ \ '_ \ / __| | | |
 | |__| (_) | | | | (__| |_| | |  | | |  __/ | | | (__| |_| |
  \____\___/|_| |_|\___|\__,_|_|  |_|  \___|_| |_|\___|\__, |
                                                       |___/ 
*)

let concurrency = async {
    // easy async logic with "async" keyword
    let! result = async { return 1 }
    return result
}

// easy parallelism
// Async.Parallel [ for i in 0..40 ->
//       async { return fib(i) } ]

// message queues
MailboxProcessor.Start(fun inbox -> async{
   let! msg = inbox.Receive()
   printfn "message is: %s" msg
   })











(*
   ____                      _      _                           
  / ___|___  _ __ ___  _ __ | | ___| |_ ___ _ __   ___  ___ ___ 
 | |   / _ \| '_ ` _ \| '_ \| |/ _ \ __/ _ \ '_ \ / _ \/ __/ __|
 | |__| (_) | | | | | | |_) | |  __/ ||  __/ | | |  __/\__ \__ \
  \____\___/|_| |_| |_| .__/|_|\___|\__\___|_| |_|\___||___/___/
                      |_|                                       
*)
// impure code when needed
let mutable counter = 0

// create C# compatible classes and interfaces
type IEnumerator<'a> =
    abstract member Current : 'a
    abstract MoveNext : unit -> bool

// extension methods
type System.Int32 with
    member this.IsEven = this % 2 = 0

let i=20
if i.IsEven then printfn "'%i' is even" i

// UI code
// open System.Windows.Forms
// let form = new Form(Width = 400, Height = 300,
//    Visible = true, Text = "Hello World")
// form.TopMost <- true
// form.Click.Add (fun args -> printfn "clicked!")
// form.Show()




// Short feedback cycles
// Safe defaults