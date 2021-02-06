open BenchmarkJs

let suiteName = "Create Validator"

let suite = Suite.make(
  ~config={
    ...Suite.defaultConfig,
    onError: Js.log,
    onStart: _ => Js.log(j`Running Benchmarks: $suiteName\r\n`),
    onComplete: _ => Js.log("\r\nDONE\r\n"),
  },
  suiteName,
)

let validatorRef = ref(None)

suite
->Suite.add("isLongerThan(1) && isLongerThan(2) && isLongerThan(3) && isLongerThan(4)", (. ()) => {
  open Validator__Core2
  open Validator__StringValidators2

  validatorRef :=
    Some({
      open! Infix
      isLongerThan(1) && isLongerThan(2) && isLongerThan(3) && isLongerThan(4)
    })
})
->Suite.run
->Suite.toArray
->Belt.Array.forEach(benchmark => Benchmark.toString(benchmark)->Js.log)
