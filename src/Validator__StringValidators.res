module Impl = {
  open Validator__Core

  let shorterThan = (length: int) =>
    make({
      name: j`shorterThan($length)`,
      validate: str => Js.String.length(str) < length ? Pass : Fail(""),
    })

  let longerThan = (length: int) =>
    make({
      name: j`longerThan($length)`,
      validate: str => Js.String.length(str) > length ? Pass : Fail(""),
    })

  let isHelloWorld = make({
    name: "isHelloWorld",
    validate: str =>
      switch {
        open Js.String2
        str->trim->toLowerCase
      } {
      | "helloworld"
      | "hello world"
      | "hello  world"
      | "helloworld!"
      | "hello world!"
      | "hello  world!" =>
        Pass
      | _ => Fail(j`Expected some variation of "Hello World", but got "$str" instead`)
      },
  })

  let emailRegex = %re(
    "/^(([^<>()\[\]\\.,;:\s@\"]+(\.[^<>()\[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/"
  )

  let isEmail = make({
    name: "isEmail",
    validate: str =>
      Js.Re.test_(emailRegex, str)
        ? Pass
        : Fail(j`Expected input to be a valid email address, but got ${str}`),
  })
}

include Impl
