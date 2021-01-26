module Impl = {
  type passOrFail = Pass | Fail(option<string>)

  type rec t<'a> =
    | Validator({name: string, validate: (. 'a) => bool, message: (. 'a) => string})
    | AND({name: string, a: t<'a>, b: t<'a>})
    | OR({name: string, a: t<'a>, b: t<'a>})
    | XOR({name: string, a: t<'a>, b: t<'a>})
    | NOT({name: string, a: t<'a>})

  let defaultErrorMessage = (
    type a,
    ~stringify: option<(. a) => string>,
    ~name: string,
    ~value: a,
  ) => {
    let valueString = switch stringify {
    | None => j`$value`
    | Some(toString) => toString(. value)
    }

    j`Invalid value "$valueString" passed to "$name"`
  }

  let getName = validator =>
    switch validator {
    | Validator({name})
    | AND({name})
    | OR({name})
    | XOR({name})
    | NOT({name}) => name
    }

  let updateName: (t<'a>, string) => t<'a> = (validator, name) =>
    switch validator {
    | Validator(x) => Validator({...x, name: name})
    | AND(x) => AND({...x, name: name})
    | OR(x) => OR({...x, name: name})
    | XOR(x) => XOR({...x, name: name})
    | NOT(x) => NOT({...x, name: name})
    }

  let notName = a => {
    let name = getName(a)
    j`NOT[ $name ]`
  }
  let andName = (a, b) => {
    let nameA = getName(a)
    let nameB = getName(b)
    j`AND[ $nameA, $nameB ]`
  }
  let orName = (a, b) => {
    let nameA = getName(a)
    let nameB = getName(b)
    j`OR[ $nameA, $nameB ]`
  }
  let xorName = (a, b) => {
    let nameA = getName(a)
    let nameB = getName(b)
    j`XOR[ $nameA, $nameB ]`
  }

  type error<'a> = {
    value: 'a,
    validator: t<'a>,
    validatorName: string,
    message: string,
  }

  type errorReport<'a> = {
    value: 'a,
    validatorName: string,
    validator: t<'a>,
    errors: array<error<'a>>,
  }

  let make: (~name: string, ~message: (. 'a) => string=?, (. 'a) => bool) => t<'a> = (
    ~name,
    ~message=?,
    validate,
  ) => Validator({
    name: name,
    validate: validate,
    message: switch message {
    | None => (. value) => defaultErrorMessage(~name, ~stringify=None, ~value)
    | Some(m) => m
    },
  })

  let and_: (t<'a>, t<'a>) => t<'a> = (a, b) => AND({name: andName(a, b), a: a, b: b})
  let or_: (t<'a>, t<'a>) => t<'a> = (a, b) => OR({name: orName(a, b), a: a, b: b})
  let xor: (t<'a>, t<'a>) => t<'a> = (a, b) => XOR({name: xorName(a, b), a: a, b: b})
  let not_: t<'a> => t<'a> = a => NOT({name: notName(a), a: a})

  let andU: (. t<'a>, t<'a>) => t<'a> = (. a, b) => AND({name: andName(a, b), a: a, b: b})
  let orU: (. t<'a>, t<'a>) => t<'a> = (. a, b) => OR({name: orName(a, b), a: a, b: b})
  let xorU: (. t<'a>, t<'a>) => t<'a> = (. a, b) => XOR({name: xorName(a, b), a: a, b: b})
  let notU: (. t<'a>) => t<'a> = (. a) => NOT({name: notName(a), a: a})

  module Infix = {
    let \"&&" = and_
    let \"||" = or_
    let \"^^" = xor
    let not = not_
  }

  let _makeError: (~stringify: option<(. 'a) => string>, t<'a>, 'a) => error<'a> = (
    ~stringify,
    validator,
    value,
  ) =>
    switch validator {
    | Validator({name, message}) => {
        value: value,
        validatorName: name,
        validator: validator,
        message: message(. value),
      }
    | AND({name})
    | OR({name})
    | XOR({name})
    | NOT({name}) => {
        value: value,
        validator: validator,
        validatorName: name,
        message: defaultErrorMessage(~stringify, ~name, ~value),
      }
    }

  let rec _evalSync = (validator: t<'a>, errStackRef: ref<array<t<'a>>>, value: 'a) => {
    open Belt
    let errStack = errStackRef.contents

    switch validator {
    | Validator({validate}) =>
      switch validate(. value) {
      | true => true
      | false =>
        let _ = Belt.Array.setUnsafe(errStack, Belt.Array.length(errStack), validator)
        false
      }
    | NOT({a: Validator({validate})}) =>
      switch validate(. value) {
      | false =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        true
      | true =>
        let _ = Array.setUnsafe(errStack, Array.length(errStack), validator)
        false
      }
    | NOT({a: NOT({a: x})}) => _evalSync(x, errStackRef, value)
    | NOT({a}) =>
      switch _evalSync(a, errStackRef, value) {
      | false =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        true
      | true =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        false
      }
    | AND({a, b}) =>
      switch _evalSync(a, errStackRef, value) {
      | false =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        false
      | true =>
        switch _evalSync(b, errStackRef, value) {
        | false =>
          let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
          false
        | true =>
          if Array.length(errStack) > 0 {
            errStackRef := []
          }
          true
        }
      }
    | OR({a, b}) =>
      switch _evalSync(a, errStackRef, value) {
      | true =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        true
      | false =>
        switch _evalSync(b, errStackRef, value) {
        | true =>
          if Array.length(errStack) > 0 {
            errStackRef := []
          }
          true
        | false =>
          let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
          false
        }
      }
    | XOR({a, b}) =>
      switch (_evalSync(a, errStackRef, value), _evalSync(b, errStackRef, value)) {
      | (true, false) =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        true
      | (false, true) =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        true
      | (false, false) =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        false
      | (true, true) =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        false
      }
    }
  }

  let validate = (~stringify=?, validator, value) => {
    open Belt
    let errStackRef = ref([])
    let errors = []
    let result = _evalSync(validator, errStackRef, value)
    switch result {
    | true => Ok(value)
    | false =>
      let errStack = errStackRef.contents
      let errorsIndexRef = ref(0)

      let () = for i in 0 to Array.length(errStack) - 1 {
        let errorsIndex = errorsIndexRef.contents

        switch Belt.Array.get(errStack, i) {
        | None => ()
        | Some(v) =>
          let _ = Belt.Array.setUnsafe(errors, errorsIndex, _makeError(~stringify, v, value))
          incr(errorsIndexRef)
        }
      }
      Error({
        value: value,
        validator: validator,
        validatorName: getName(validator),
        errors: errors,
      })
    }
  }

  type rec async<'a> =
    | Validator_Async({
        name: string,
        validate: 'a => Js.Promise.t<bool>,
        message: option<'a => string>,
      })
    | AND_Async({name: string, a: async<'a>, b: async<'a>})
    | OR_Async({name: string, a: async<'a>, b: async<'a>})
    | XOR_Async({name: string, a: async<'a>, b: async<'a>})
    | NOT_Async({name: string, a: async<'a>})

  module Async = {
    type t<'a> = async<'a>

    let getName = validator =>
      switch validator {
      | Validator_Async({name})
      | AND_Async({name})
      | OR_Async({name})
      | XOR_Async({name})
      | NOT_Async({name}) => name
      }

    let rename: (t<'a>, string) => t<'a> = (validator, name) =>
      switch validator {
      | Validator_Async(x) => Validator_Async({...x, name: name})
      | AND_Async(x) => AND_Async({...x, name: name})
      | OR_Async(x) => OR_Async({...x, name: name})
      | XOR_Async(x) => XOR_Async({...x, name: name})
      | NOT_Async(x) => NOT_Async({...x, name: name})
      }
  }
}

include Impl
