import gleeunit
import gleeunit/should
import tibe.{EApply, ELambda, ELet, EVariable, TConstructor}
import gleam/list
import gleam/int
import gleam/map

pub fn main() {
  gleeunit.main()
}

pub fn lambda_test() {
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

pub fn let_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        value: EVariable(name: "10"),
        body: EApply(
          lambda: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    TConstructor("Int", []),
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
