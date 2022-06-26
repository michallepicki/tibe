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
  should.equal(
    tibe.infer(
      initial_environment(),
      ELambda(
        arguments: ["x", "y"],
        body: EApply(
          lambda: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
        ),
      ),
    ),
    TConstructor(
      "Function2",
      [
        TConstructor("Int", []),
        TConstructor("Int", []),
        TConstructor("Int", []),
      ],
    ),
  )
}

fn initial_environment() {
  let initial_environment =
    list.range(0, 99)
    |> list.map(int.to_string)
    |> list.map(fn(name) {
      #(name, TConstructor(name: "Int", type_parameters: []))
    })
    |> map.from_list()

  ["+", "-", "*", "/"]
  |> list.map(fn(name) {
    let t =
      TConstructor(
        name: "Function2",
        type_parameters: [
          TConstructor(name: "Int", type_parameters: []),
          TConstructor(name: "Int", type_parameters: []),
          TConstructor(name: "Int", type_parameters: []),
        ],
      )

    #(name, t)
  })
  |> map.from_list()
  |> map.merge(initial_environment)
}
