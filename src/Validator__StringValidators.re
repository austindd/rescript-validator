module Impl = {
  open Validator__Core;

  let shorterThan = (length: int) =>
    make({
      name: {j|shorterThan($length)|j},
      validate: str => Js.String.length(str) < length ? Pass : Fail(""),
    });

  let longerThan = (length: int) =>
    make({
      name: {j|longerThan($length)|j},
      validate: str => Js.String.length(str) < length ? Pass : Fail(""),
    });

  let isHelloWorld =
    make({
      name: "isHelloWorld",
      validate: str =>
        switch (Js.String2.(str->trim->toLowerCase)) {
        | "helloworld"
        | "hello world"
        | "hello  world"
        | "helloworld!"
        | "hello world!"
        | "hello  world!" => Pass
        | _ =>
          Fail(
            {j|Expected some variation of "Hello World", but got "$str" instead|j},
          )
        },
    });
};

include Impl;
