open! Validator__Core
open! Validator__StringValidators

let myValidator = {
  open! Infix
  // (!isShorterThan(100) && !isLongerThan(5)) || isHelloWorld || isEmail
  isLongerThan(10) || isLongerThan(11) || isLongerThan(12)
}

Js.log2("Validator:", myValidator)

let result = validate(myValidator, "Hello")

// Js.log2("Result:", result)

switch result {
| Ok(value) => Js.log(value)
| Error(report) => Js.log(report.errors)
}

module type F = {
  type t
  let identity: t => t
}

module F = (
  T: {
    type t
  },
) => {
  type t = T.t
  external identity: t => t = "%identity"
}

type id<'a> = ..

module type Id = {
  type kind
  type id<_> +=
    | Id: id<kind>
  type t = id<kind>
  let id: t
}

module Newtype = (
  T: {
    type kind
  },
): (Id with type kind = T.kind) => {
  type kind = T.kind
  type t = id<kind>
  type id<'a> +=
    | Id: id<kind>
  let id: t = Id
}

type value<'a> = {
  value: 'a,
  id: module(Id with type kind = 'a),
}

let newtype = (type a, value: a) => {
  let id: module(Id with type kind = a) = module(
    Newtype({
      type kind = a
    })
  )
  {
    value: value,
    id: id,
  }
}
