open Jest
open Expect

module Core = Validator__Core2
module StringValidators = Validator__StringValidators2

module Utils = {
  let _isValid = validationResult =>
    switch validationResult {
    | Ok(_) => true
    | Error(_) => false
    }

  let _getError_byName = (validationResult, name) => {
    open Core
    switch validationResult {
    | Ok(_) => None
    | Error(report) => Js.Dict.get(report.errorMap, name)
    }
  }

  let _errorExists_byName = (validationResult, name) =>
    switch _getError_byName(validationResult, name) {
    | None => false
    | Some(_error) => true
    }

  let _getError = (validationResult, validator) =>
    _getError_byName(validationResult, Core.getName(validator))

  let _errorExists = (validationResult, validator) =>
    switch _getError_byName(validationResult, Core.getName(validator)) {
    | None => false
    | Some(_error) => true
    }

  let _didPass_byName = (validationResult, name) => {
    open Core
    switch validationResult {
    | Ok(report) | Error(report) =>
      switch Js.Dict.get(report.boolMap, name) {
      | None => false
      | Some(b) => b
      }
    }
  }

  let _didPass = (validationResult, validator) => {
    _didPass_byName(validationResult, Core.getName(validator))
  }

  let _allTrue = boolArray => Belt.Array.everyU(boolArray, (. item) => item)
}

let (
  isLongerThan_1,
  isLongerThan_2,
  isLongerThan_3,
  isLongerThan_4,
  isLongerThan_5,
  isLongerThan_6,
  isLongerThan_7,
  isLongerThan_8,
) = {
  open StringValidators
  (
    isLongerThan(1),
    isLongerThan(2),
    isLongerThan(3),
    isLongerThan(4),
    isLongerThan(5),
    isLongerThan(6),
    isLongerThan(7),
    isLongerThan(8),
  )
}

let (
  isShorterThan_1,
  isShorterThan_2,
  isShorterThan_3,
  isShorterThan_4,
  isShorterThan_5,
  isShorterThan_6,
  isShorterThan_7,
  isShorterThan_8,
) = {
  open StringValidators
  (
    isShorterThan(1),
    isShorterThan(2),
    isShorterThan(3),
    isShorterThan(4),
    isShorterThan(5),
    isShorterThan(6),
    isShorterThan(7),
    isShorterThan(8),
  )
}

describe("'AND' Operator", () => {
  test("Validation errors are reported correctly", () => {
    open Utils

    let left = isLongerThan_1
    let right = isShorterThan_5
    let validator = Core.and_(left, right)

    let failed_left = Core.validate(validator, "")
    let failed_right = Core.validate(validator, "1234567")
    let passed = Core.validate(validator, "123")

    let result = (
      _didPass(failed_left, left) === false,
      _didPass(failed_right, right) === false,
      _didPass(passed, validator) === true,
    )

    let expected = (true, true, true)

    expect(result) |> toEqual(expected)
  })
})

describe("'OR' Operator", () => {
  test("Validation errors are reported correctly", () => {
    open Utils

    let v1_left = isShorterThan_2
    let v1_right = isLongerThan_5
    let v1 = Core.or_(v1_left, v1_right)

    let v1_leftErr_rightErr = Core.validate(v1, "123")
    let v1_leftOk_rightErr = Core.validate(v1, "")
    let v1_leftErr_rightOk = Core.validate(v1, "123456")

    let v1_result = (
      v1_leftErr_rightErr->_didPass(v1) === false,
      v1_leftErr_rightOk->_didPass(v1_left) === false,
      v1_leftOk_rightErr->_didPass(v1_right) === false,
    )

    let v2_left = isLongerThan_2
    let v2_right = isShorterThan_5
    let v2 = Core.or_(v2_left, v2_right)

    let v2_leftErr_rightOk = Core.validate(v2, "1")
    let v2_leftOk_rightErr = Core.validate(v2, "123456")
    let v2_leftOk_rightOk = Core.validate(v2, "1234")

    let v2_result = (
      v2_leftErr_rightOk->_didPass(v2_left) === false,
      v2_leftOk_rightErr->_didPass(v2_right) === false,
      v2_leftOk_rightOk->_didPass(v2) === true,
    )

    let result = (v1_result, v2_result)

    let expected = ((true, true, true), (true, true, true))

    expect(result) |> toEqual(expected)
  })
})
