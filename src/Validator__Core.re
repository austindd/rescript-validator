module Impl = {
  type error('a) = {
    name: string,
    value: 'a,
    message: string,
  };

  type validation =
    | Pass
    | Fail(string);

  type definition('a) = {
    name: string,
    validate: 'a => validation,
  };

  type t('a) =
    | Validator(definition('a))
    | AND({
        name: string,
        a: t('a),
        b: t('a),
      })
    | OR({
        name: string,
        a: t('a),
        b: t('a),
      })
    | XOR({
        name: string,
        a: t('a),
        b: t('a),
      })
    | NOT({
        name: string,
        a: t('a),
      });

  let getName = validator =>
    switch (validator) {
    | Validator({name})
    | AND({name})
    | OR({name})
    | XOR({name})
    | NOT({name}) => name
    };

  let rename: (t('a), string) => t('a) =
    (validator, name) => {
      switch (validator) {
      | Validator(x) => Validator({...x, name})
      | AND(x) => AND({...x, name})
      | OR(x) => OR({...x, name})
      | XOR(x) => XOR({...x, name})
      | NOT(x) => NOT({...x, name})
      };
    };

  let notName = a => {
    let name = getName(a);
    {j|NOT[$name]|j};
  };
  let andName = (a, b) => {
    let nameA = getName(a);
    let nameB = getName(b);
    {j|AND[$nameA,$nameB]|j};
  };
  let orName = (a, b) => {
    let nameA = getName(a);
    let nameB = getName(b);
    {j|OR[$nameA,$nameB]|j};
  };
  let xorName = (a, b) => {
    let nameA = getName(a);
    let nameB = getName(b);
    {j|XOR[$nameA,$nameB]|j};
  };

  type report('a) = {
    value: 'a,
    name: string,
    isValid: bool,
    passed: array(string),
    failed: array(error('a)),
  };

  type mutReport('a) = {
    value: 'a,
    name: string,
    mutable isValid: bool,
    passed: array(string),
    failed: array(error('a)),
  };

  external sealReport: mutReport('a) => report('a) = "%identity";
  external editReport: report('a) => mutReport('a) = "%identity";

  let make: definition('a) => t('a) = v => Validator(v);
  let and_: (t('a), t('a)) => t('a) =
    (a, b) => AND({name: andName(a, b), a, b});
  let or_: (t('a), t('a)) => t('a) =
    (a, b) => OR({name: orName(a, b), a, b});
  let xor: (t('a), t('a)) => t('a) =
    (a, b) => XOR({name: xorName(a, b), a, b});
  let not: t('a) => t('a) = a => NOT({name: notName(a), a});

  module Msg = {
    module Not = {
      let passedButNegated = (~name: string, ~value: 'a) => {j|NOT[$name]: The input value "$value" passed "$name", but was negated by the "NOT" operator.|j};
      let failedButNegated = (~name: string, ~value: 'a) => {j|NOT[$name]: The input value "$value" failed "$name", but was negated by the "NOT" operator.|j};
      let doubleNegated = (~name: string, ~value: 'a) => {j|NOT[NOT[$name]]: The input value "$value" failed "$name", and was negated twice by the "NOT" operator.|j};
    };

    module And = {
      let leftButNotRight = (~left: string, ~right: string, ~value) => {j|AND[$left,$right]: The input value "$value" passed "$left" but failed "$right". |j};
      let rightButNotLeft = (~left: string, ~right: string, ~value) => {j|AND[$left,$right]: The input value "$value" passed "$right" but failed "$left". |j};
      let neitherLeftNorRight = (~left: string, ~right: string, ~value) => {j|AND[$left,$right]: The input value "$value" failed both "$left" and "$right".|j};
    };

    module Or = {
      let neitherLeftNorRight = (~left: string, ~right: string, ~value) => {j|OR[$left,$right]: The input value "$value" failed both "$left" and "$right".|j};
    };

    module Xor = {
      let passedLeftAndRight = (~left: string, ~right: string, ~value) => {j|XOR[$left,$right]: The input value "$value" passed both "$left" and "$right".|j};
      let failedLeftAndRight = (~left: string, ~right: string, ~value) => {j|XOR[$left,$right]: The input value "$value" failed both "$left" and "$right".|j};
    };
  };

  let%private rec __evalSync = (report, validator, value) => {
    switch (validator) {
    | Validator({name, validate}) =>
      switch (validate(value)) {
      | Pass =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      | Fail(e) as error =>
        let _ = report.failed->Js.Array2.push({name, value, message: e});
        error;
      }
    | NOT({name, a: Validator({name: vName, validate})}) =>
      switch (validate(value)) {
      | Pass =>
        let message = Msg.Not.passedButNegated(~name=vName, ~value);
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      | Fail(_) =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      }
    | NOT({name, a: NOT({a: innerValidator})}) =>
      switch (__evalSync(report, innerValidator, value)) {
      | Pass =>
        let _ = report.passed->Js.Array2.push(name);
        Pass
      | Fail(_) =>
        let message =
          Msg.Not.doubleNegated(~name=getName(innerValidator), ~value);
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      }
    | NOT({name, a}) =>
      switch (__evalSync(report, a, value)) {
      | Pass =>
        let message = Msg.Not.passedButNegated(~name=getName(a), ~value);
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      | Fail(_) =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      }
    | AND({name, a, b}) =>
      switch (__evalSync(report, a, value), __evalSync(report, b, value)) {
      | (Pass, Pass) =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      | (Fail(_), Fail(_)) =>
        let message =
          Msg.And.neitherLeftNorRight(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      | (Pass, Fail(_)) =>
        let message =
          Msg.And.leftButNotRight(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      | (Fail(_), Pass) =>
        let message =
          Msg.And.rightButNotLeft(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      }
    | OR({name, a, b}) =>
      switch (__evalSync(report, a, value), __evalSync(report, b, value)) {
      | (Pass, _)
      | (_, Pass) =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      | (Fail(_), Fail(_)) =>
        let message =
          Msg.Or.neitherLeftNorRight(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      }
    | XOR({name, a, b}) =>
      switch (__evalSync(report, a, value), __evalSync(report, b, value)) {
      | (Pass, Fail(_))
      | (Fail(_), Pass) =>
        let _ = report.passed->Js.Array2.push(name);
        Pass;
      | (Pass, Pass) =>
        let message =
          Msg.Xor.passedLeftAndRight(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      | (Fail(_), Fail(_)) =>
        let message =
          Msg.Xor.failedLeftAndRight(
            ~left=getName(a),
            ~right=getName(b),
            ~value,
          );
        let _ = report.failed->Js.Array2.push({name, value, message});
        Fail(message);
      }
    };
  };

  let evalSync = (validator, value) => {
    let mutReport = {
      name: getName(validator),
      value,
      isValid: true,
      passed: [||],
      failed: [||],
    };
    let result = __evalSync(mutReport, validator, value);
    switch (result) {
    | Pass => Ok(sealReport(mutReport))
    | Fail(_) =>
      mutReport.isValid = false;
      Error(sealReport(mutReport));
    };
  };

  module Infix = {
    let (&&) = and_;
    let (||) = or_;
    let (^^) = xor;
    let (!) = not;
  };

};

include Impl;
