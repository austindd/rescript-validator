open! Validator__Core
open! Validator__StringValidators

let myValidator = {
  open! Infix
  (!shorterThan(3) && !longerThan(24)) || isHelloWorld
}

Js.log2("Validator:", myValidator)
let result = evalSync(myValidator, "Hi")
Js.log2("Result:", result)
switch result {
| Ok({passed, failed}) =>
  Js.log("PASSED")
  Array.iter(Js.log, passed)
  Js.log("FAILED")
  Array.iter(Js.log, failed)
| Error({passed, failed}) =>
  Js.log("PASSED")
  Array.iter(Js.log, passed)
  Js.log("FAILED")
  Array.iter(Js.log, failed)
}
