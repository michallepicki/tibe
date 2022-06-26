import gleeunit
import gleeunit/should
import tibe.{
  EApply, EInt, ELambda, ELet, EString, EVariable, TConstructor, TypeCheckUnifyError,
  TypeMismatch,
}
import gleam/list
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
    Ok(TConstructor(
      "Function2",
      [
        TConstructor("Int", []),
        TConstructor("Int", []),
        TConstructor("Int", []),
      ],
    )),
  )
}

pub fn let_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        value: EInt(value: 10),
        body: EApply(
          lambda: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    Ok(TConstructor("Int", [])),
  )
}

pub fn type_mismatch_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EApply(
        lambda: EVariable(name: "+"),
        arguments: [EInt(value: 10), EString(value: "some_string")],
      ),
    ),
    Error(TypeCheckUnifyError(TypeMismatch(
      TConstructor(name: "Int", type_parameters: []),
      TConstructor(name: "String", type_parameters: []),
    ))),
  )
}

fn initial_environment() {
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
}
