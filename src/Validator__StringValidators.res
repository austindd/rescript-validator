module Impl = {
  open Validator__Core

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

  let isEmail = {
    let name = "isEmail"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.Re.test_(emailRegex, str),
    )
  }

  let isNotEmpty = {
    let name = "isNotEmpty"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => str !== "",
    )
  }

  let isEmpty = {
    let name = "isEmpty"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => str === "",
    )
  }

  let whitespaceRegex = %re("/\s/g")

  let containsWhitespaceChar = {
    let name = "containsWhitespaceChar"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.Re.test_(whitespaceRegex, str),
    )
  }

  let nonWhitespaceRegex = %re("/\S/g")

  let containsNonWhitespaceChar = {
    let name = "containsNonWhitespaceChar"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.Re.test_(nonWhitespaceRegex, str),
    )
  }

  let onlyDigitsRegex = %re("/^\d+$/")

  let containsOnlyDigits = {
    let name = "containsOnlyDigits"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.Re.test_(onlyDigitsRegex, str),
    )
  }

  let floatRegex = %re("/^-?\d+\.?\d*$|^\d*\.?\d+$/")

  let isFloat = {
    let name = "isFloat"
    make(
      ~name,
      ~message=(. value) => defaultErrorMessage(~stringify=None, ~name, ~value),
      (. str) => Js.Re.test_(floatRegex, str),
    )
  }
}

include Impl
