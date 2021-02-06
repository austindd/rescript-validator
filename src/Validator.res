open! Validator__Core
open! Validator__StringValidators

@bs.val external print: 'a => unit = "console.dir"

let myValidator = {
  open! Infix
  // (!isShorterThan(100) && !isLongerThan(5)) || isHelloWorld || isEmail
  isLongerThan(10) || isLongerThan(11) || isLongerThan(12)
}

Js.log2("Validator:", myValidator)

let result = validate(myValidator, "Hello")

// Js.log2("Result:", result)

switch result {
| Ok(value) => print(value)
| Error(report) => print(report.boolMap)
}

