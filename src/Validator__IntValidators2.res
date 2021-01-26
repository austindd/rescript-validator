module Impl = {
  open Validator__Core2

  let isGreaterThan = (n: int) => {
    let nStr = string_of_int(n)
    let name = j`greaterThan(${nStr})`
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=Some(string_of_int), ~name, ~value),
      (. value) => {
        value > n
      },
    )
  }

  let isLessThan = (n: int) => {
    let nStr = string_of_int(n)
    let name = j`isLessThan(${nStr})`
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=Some(string_of_int), ~name, ~value),
      (. value) => {
        value < n
      },
    )
  }

  let isMultipleOf = (n: int) => {
    let nStr = string_of_int(n)
    let name = j`isMultipleOf(${nStr})`
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=Some(string_of_int), ~name, ~value),
      (. value) => {
        if n === 0 {
          if value === 0 {
            true
          } else {
            false
          }
        } else {
          mod(value, n) === 0
        }
      },
    )
  }
}

include Impl
