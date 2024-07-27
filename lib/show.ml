open Core

type show = [ `Show of unit -> string ]
type intshow = [ show | `Value of int ]

let show_value : intshow -> string =
  let a v =
    match v with
    | `Show f -> f ()
    | `Value i -> sprintf "%d" i
  in
  a

let _ = show_value (`Show (fun () -> "hello"))
let _ = show_value (`Value 10) (* This will cause a type error *)

module Show = struct
  type t = show

  let create s = `Show (fun () -> s)
end

module IntShow = struct
  type t = intshow

  let create v = `Show (fun () -> sprintf "%d" v)
end

let show_value (`Show show) = show ()
let _ = show_value IntShow.(create 10)
let _ = show_value Show.(create "hello")
