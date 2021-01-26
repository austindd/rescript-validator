module Impl = {
  open Validator__Core2

  let isShorterThan = (length: int) => {
    let name = j`isShorterThan($length)`
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.String.length(str) < length,
    )
  }

  let isLongerThan = (length: int) => {
    let name = j`isLongerThan($length)`
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.String.length(str) > length,
    )
  }

  let isHelloWorld = make(~name="isHelloWorld", (. str) =>
    switch {
      open Js.String2
      str->trim->toLowerCase
    } {
    | "helloworld"
    | "hello world"
    | "hello  world"
    | "helloworld!"
    | "hello world!"
    | "hello  world!" => true
    | _ => false
    }
  )

  let emailRegex = %re(
    "/^(([^<>()\[\]\\.,;:\s@\"]+(\.[^<>()\[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/"
  )

  let isEmail =
    make(~name="isEmail", (. str) => Js.Re.test_(emailRegex, str))
}

include Impl
