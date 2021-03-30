# rescript-validator
A composable data-validation library for ReScript. Currently under construction.

The goal of this library is to allow users to intuitively compose a set of validators using boolean logic. For example:

```rescript
let isValidUserName = {
  open Validator.StringValidators;
  open Validator.Infix;

  let isValidNonEmailHandle =
    isLongerThan(3)
    && isShorterThan(65)
    && allCharsAreAscii
    && blacklistChars(bannedChars)
    && blacklistWords(bannedWords);
  
  let isValidNonEmailHandle = useName("isValidNonEmailHandle", isValidNonEmailHandle);

  useName("isValidUserName", isEmail || isValidNonEmailHandle);
}

let userName = "qF5wr*ywDLYta40^Dbwv";

let result = Validator.validate(isValidUserName, userName);
```

This ability to compose validators will hopefully make data validation feel intuitive and easy. Feel free to take a look and offer suggestions during development.
