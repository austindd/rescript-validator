open! Validator__Core2
open! Validator__StringValidators2

let myValidator = {
  open! Infix
  // (!isShorterThan(100) && !isLongerThan(5)) || isHelloWorld || isEmail
  isLongerThan(10) || isLongerThan(11) || isLongerThan(12)
}

Js.log2("Validator:", myValidator)

let result = evalSync(myValidator, "Hello")

Js.log2("Result:", result)

switch result {
| Ok(value) =>
  Js.log(value)
| Error(report) =>
  Js.log(report)
}

