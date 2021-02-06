open BenchmarkJs

let suiteName = "Validate Strings"

let suite = Suite.make(
  ~config={
    ...Suite.defaultConfig,
    onError: Js.log,
    onStart: _ => Js.log(j`Running Benchmarks: $suiteName\r\n`),
    onComplete: _ => Js.log("\r\nDONE\r\n"),
  },
  suiteName,
)

let resultRef = ref(None)
let isLongerThan_1_2_3_4 = {
  open Validator__Core2
  open Validator__StringValidators2
  open! Infix
  isLongerThan(1) && isLongerThan(2) && isLongerThan(3) && isLongerThan(4)
}

suite
->Suite.add(
  "isLongerThan(1) && isLongerThan(2) && isLongerThan(3) && isLongerThan(4) - PASSING",
  (. ()) => {
    open Validator__Core2

    resultRef :=
      Some({
        validate(isLongerThan_1_2_3_4, "Hello")
      })
  },
)
->Suite.add(
  "isLongerThan(1) && isLongerThan(2) && isLongerThan(3) && isLongerThan(4) - FAILING",
  (. ()) => {
    open Validator__Core2

    resultRef :=
      Some({
        validate(isLongerThan_1_2_3_4, "abc")
      })
  },
)
->Suite.run
->Suite.toArray
->Belt.Array.forEach(benchmark => Benchmark.toString(benchmark)->Js.log)
