module Impl = {
  open Validator__Core

  let greaterThan = (n: int) => {
    let nStr = string_of_int(n)
    make({
      name: j`greaterThan(${nStr})`,
      validate: value => {
        let valueStr = string_of_int(value)
        value > n ? Pass : Fail(j`Expected an integer greater than ${nStr}, but got ${valueStr}`)
      },
    })
  }

  let lessThan = (n: int) => {
    let nStr = string_of_int(n)
    make({
      name: j`lessThan(${nStr})`,
      validate: value => {
        let valueStr = string_of_int(value)
        value < n ? Pass : Fail(j`Expected an integer less than ${nStr}, but got ${valueStr}`)
      },
    })
  }

  let multipleOf = (n: int) => {
    let nStr = string_of_int(n)
    make({
      name: j`multipleOf(${nStr})`,
      validate: value => {
        if n === 0 {
          if value === 0 {
            Pass
          } else {
            let valueStr = string_of_int(value)
            Fail(j`Expected an integer that is a multiple of ${nStr}, but got ${valueStr}`)
          }
        } else {
          let valueStr = string_of_int(value)
          mod(value, n) === 0
            ? Pass
            : Fail(j`Expected an integer that is a multiple of ${nStr}, but got ${valueStr}`)
        }
      },
    })
  }
}
