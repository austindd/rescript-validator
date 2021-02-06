module Impl = {

  type rec t<'a> =
    | Validator({name: string, validate: (. 'a) => bool, message: (. 'a) => string})
    | AND({name: string, left: t<'a>, right: array<t<'a>>})
    | OR({name: string, left: t<'a>, right: array<t<'a>>})
    | XOR({name: string, left: t<'a>, right: t<'a>})
    | NOT({name: string, right: t<'a>})

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
    name: string,
    message: string,
  }

  type rec resultTree<'a> =
    | Validator({name: string, validator: t<'a>})
    | AndTree({
        name: string,
        validator: t<'a>,
        left: result<resultTree<'a>, resultTree<'a>>,
        right: result<resultTree<'a>, resultTree<'a>>,
      })
    | OrTree({
        name: string,
        validator: t<'a>,
        left: result<resultTree<'a>, resultTree<'a>>,
        right: result<resultTree<'a>, resultTree<'a>>,
      })
    | XorTree({
        name: string,
        validator: t<'a>,
        left: result<resultTree<'a>, resultTree<'a>>,
        right: result<resultTree<'a>, resultTree<'a>>,
      })
    | NotTree({name: string, validator: t<'a>, right: result<resultTree<'a>, resultTree<'a>>})

  type report<'a> = {
    value: 'a,
    name: string,
    validator: t<'a>,
    errors: array<error<'a>>,
    errorMap: Js.Dict.t<error<'a>>,
    resultTree: option<resultTree<'a>>,
    boolMap: Js.Dict.t<bool>,
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

  let and_: (t<'a>, t<'a>) => t<'a> = (left, right) => {
    open Belt
    switch left {
    | AND({left: l1, right: r1}) =>
      let arr = Array.sliceToEnd(r1, 0)
      switch right {
      | AND({left: l2, right: r2}) =>
        let r2Length = Array.length(r2);
        let _ = arr->Js.Array2.push(l2)
        for i in 0 to r2Length - 1 {
          let _ = arr->Js.Array2.push(Array.getUnsafe(r2, i))
        }
        AND({
          name: andName(left, right),
          left: l1,
          right: arr,
        })
      | _ =>
        let _ = arr->Js.Array2.push(right)
        AND({
          name: andName(left, right),
          left: l1,
          right: arr,
        })
      }
    | _ =>
      AND({
        name: andName(left, right),
        left: left,
        right: [right],
      })
    }
  }

  let or_: (t<'a>, t<'a>) => t<'a> = (left, right) => {
    open Belt
    switch left {
    | OR({left: l1, right: r1}) =>
      let arr = Array.sliceToEnd(r1, 0)
      switch right {
      | OR({left: l2, right: r2}) =>
        let r2Length = Array.length(r2);
        let _ = arr->Js.Array2.push(l2)
        for i in 0 to r2Length - 1 {
          let _ = arr->Js.Array2.push(Array.getUnsafe(r2, i))
        }
        OR({
          name: orName(left, right),
          left: l1,
          right: arr,
        })
      | _ =>
        let _ = arr->Js.Array2.push(right)
        OR({
          name: orName(left, right),
          left: l1,
          right: arr,
        })
      }
    | _ =>
      OR({
        name: orName(left, right),
        left: left,
        right: [right],
      })
    }
  }

  let xor: (t<'a>, t<'a>) => t<'a> = (left, right) => XOR({
    name: xorName(left, right),
    left: left,
    right: right,
  })
  let not_: t<'a> => t<'a> = right => NOT({name: notName(right), right: right})

  let andU: (. t<'a>, t<'a>) => t<'a> = (. left, right) => {
    open Belt
    switch left {
    | AND({left: l1, right: r1}) =>
      let arr = Array.sliceToEnd(r1, 0)
      switch right {
      | AND({left: l2, right: r2}) =>
        let r2Length = Array.length(r2);
        let _ = arr->Js.Array2.push(l2)
        for i in 0 to r2Length - 1 {
          let _ = arr->Js.Array2.push(Array.getUnsafe(r2, i))
        }
        AND({
          name: andName(left, right),
          left: l1,
          right: arr,
        })
      | _ =>
        let _ = arr->Js.Array2.push(right)
        AND({
          name: andName(left, right),
          left: l1,
          right: arr,
        })
      }
    | _ =>
      AND({
        name: andName(left, right),
        left: left,
        right: [right],
      })
    }
  }

  let orU: (. t<'a>, t<'a>) => t<'a> = (. left, right) => {
    open Belt
    switch left {
    | OR({left: l1, right: r1}) =>
      let arr = Belt.Array.sliceToEnd(r1, 0)
      switch right {
      | OR({left: l2, right: r2}) =>
        let r2Length = Array.length(r2);
        let _ = arr->Js.Array2.push(l2)
        for i in 0 to r2Length - 1 {
          let _ = arr->Js.Array2.push(Array.getUnsafe(r2, i))
        }
        OR({
          name: orName(left, right),
          left: l1,
          right: arr,
        })
      | _ =>
        let _ = arr->Js.Array2.push(right)
        OR({
          name: orName(left, right),
          left: l1,
          right: arr,
        })
      }
    | _ =>
      OR({
        name: orName(left, right),
        left: left,
        right: [right],
      })
    }
  }

  let xorU: (. t<'a>, t<'a>) => t<'a> = (. left, right) => XOR({
    name: xorName(left, right),
    left: left,
    right: right,
  })

  let notU: (. t<'a>) => t<'a> = (. right) => NOT({name: notName(right), right: right})

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
        name: name,
        validator: validator,
        message: message(. value),
      }
    | AND({name})
    | OR({name})
    | XOR({name})
    | NOT({name}) => {
        value: value,
        validator: validator,
        name: name,
        message: defaultErrorMessage(~stringify, ~name, ~value),
      }
    }

  let rec _evalSync = (
    resultsDict: Js.Dict.t<bool>,
    errStackRef: ref<array<t<'a>>>,
    value: 'a,
    validator: t<'a>,
  ) => {
    open Belt
    let errStack = errStackRef.contents

    switch validator {
    | Validator({name, validate}) =>
      switch validate(. value) {
      | true =>
        Js.Dict.set(resultsDict, name, true)
        true
      | false =>
        Js.Dict.set(resultsDict, name, false)
        let _ = Belt.Array.setUnsafe(errStack, Belt.Array.length(errStack), validator)
        false
      }
    | NOT({name, right: Validator({validate})}) =>
      switch validate(. value) {
      | false =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        Js.Dict.set(resultsDict, name, true)
        true
      | true =>
        let _ = Array.setUnsafe(errStack, Array.length(errStack), validator)
        Js.Dict.set(resultsDict, name, false)
        false
      }
    | NOT({name, right: NOT({right: x})}) =>
      let res = _evalSync(resultsDict, errStackRef, value, x)
      Js.Dict.set(resultsDict, name, res)
      res
    | NOT({name, right}) =>
      switch _evalSync(resultsDict, errStackRef, value, right) {
      | false =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        Js.Dict.set(resultsDict, name, true)
        true
      | true =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        Js.Dict.set(resultsDict, name, false)
        false
      }
    | AND({name, left, right}) =>
      switch _evalSync(resultsDict, errStackRef, value, left) {
      | false =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        Js.Dict.set(resultsDict, name, false)
        false
      | true =>
        switch _ANDArrayEval(resultsDict, errStackRef, value, right) {
        | false =>
          let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
          Js.Dict.set(resultsDict, name, false)
          false
        | true =>
          if Array.length(errStack) > 0 {
            errStackRef := []
          }
          Js.Dict.set(resultsDict, name, true)
          true
        }
      }
    | OR({name, left, right}) =>
      switch _evalSync(resultsDict, errStackRef, value, left) {
      | true =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        Js.Dict.set(resultsDict, name, true)
        true
      | false =>
        switch _ORArrayEval(resultsDict, errStackRef, value, right) {
        | true =>
          if Array.length(errStack) > 0 {
            errStackRef := []
          }
          Js.Dict.set(resultsDict, name, true)
          true
        | false =>
          let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
          Js.Dict.set(resultsDict, name, false)
          false
        }
      }
    | XOR({name, left, right}) =>
      switch (
        _evalSync(resultsDict, errStackRef, value, left),
        _evalSync(resultsDict, errStackRef, value, right),
      ) {
      | (true, false) =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        Js.Dict.set(resultsDict, name, true)
        true
      | (false, true) =>
        if Array.length(errStack) > 0 {
          errStackRef := []
        }
        Js.Dict.set(resultsDict, name, true)
        true
      | (false, false) =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        Js.Dict.set(resultsDict, name, false)
        false
      | (true, true) =>
        let _ = Belt.Array.setUnsafe(errStack, Array.length(errStack), validator)
        Js.Dict.set(resultsDict, name, false)
        false
      }
    }
  }

  and _ANDArrayEval = (resultsDict, errStackRef, value, validatorArray) => {
    open Belt
    let len = Array.length(validatorArray)
    let result = ref(true)
    for i in 0 to len - 1 {
      let current = _evalSync(resultsDict, errStackRef, value, Array.getUnsafe(validatorArray, i))
      if current === false {
        result := false
      }
    }
    result.contents
  }

  and _ORArrayEval = (resultsDict, errStackRef, value, validatorArray) => {
    open Belt
    let len = Array.length(validatorArray)
    let result = ref(false)
    for i in 0 to len - 1 {
      let current = _evalSync(resultsDict, errStackRef, value, Array.getUnsafe(validatorArray, i))
      if current === true {
        result := true
      }
    }
    result.contents
  }

  let validate: (~stringify: (. 'a) => string=?, t<'a>, 'a) => result<report<'a>, report<'a>> = (
    ~stringify=?,
    validator,
    value,
  ) => {
    open Belt
    let name = getName(validator)
    let errStackRef = ref([])
    let boolMap = Js.Dict.empty()
    let result = _evalSync(boolMap, errStackRef, value, validator)
    switch result {
    | true =>
      Ok({
        value: value,
        validator: validator,
        name: name,
        errors: [],
        errorMap: Js.Dict.empty(),
        resultTree: None,
        boolMap: boolMap,
      })
    | false =>
      let (errorsArray, errorsDict) = ([], Js.Dict.empty())

      let errStack = errStackRef.contents
      let errorsIndexRef = ref(0)

      let () = for i in 0 to Array.length(errStack) - 1 {
        let errorsIndex = errorsIndexRef.contents

        switch Belt.Array.get(errStack, i) {
        | None => ()
        | Some(v) =>
          let currentError = _makeError(~stringify, v, value)
          let _ = Belt.Array.setUnsafe(errorsArray, errorsIndex, currentError)
          let _ = Js.Dict.set(errorsDict, getName(v), currentError)
          incr(errorsIndexRef)
        }
      }
      Error({
        value: value,
        validator: validator,
        name: name,
        errors: errorsArray,
        errorMap: errorsDict,
        resultTree: None,
        boolMap: boolMap,
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
