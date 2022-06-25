import gleeunit
import gleeunit/should
import tibe.{EApply, ELambda, EVariable, TConstructor}
import gleam/list
import gleam/int
import gleam/map

pub fn main() {
  gleeunit.main()
}

pub fn infer_test() {
  let initial_environment =
    list.range(0, 99)
    |> list.map(int.to_string)
    |> list.map(fn(name) {
      #(name, TConstructor(name: "Int", type_parameters: []))
    })
    |> map.from_list()

  let initial_environment =
    ["+", "-", "*", "/"]
    |> list.map(fn(name) {
      let t =
        TConstructor(
          name: "Function1",
          type_parameters: [
            TConstructor(name: "Int", type_parameters: []),
            TConstructor(
              name: "Function1",
              type_parameters: [
                TConstructor(name: "Int", type_parameters: []),
                TConstructor(name: "Int", type_parameters: []),
              ],
            ),
          ],
        )
      #(name, t)
    })
    |> map.from_list()
    |> map.merge(initial_environment)

  should.equal(
    tibe.infer(
      initial_environment,
      ELambda(
        arguments: ["x"],
        body: EApply(
          lambda: EApply(
            lambda: EVariable(name: "+"),
            arguments: [EVariable(name: "x")],
          ),
          arguments: [EVariable(name: "x")],
        ),
      ),
    ),
    TConstructor(
      "Function1",
      [TConstructor("Int", []), TConstructor("Int", [])],
    ),
  )
}
