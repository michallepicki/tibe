import gleeunit
import gleeunit/should
import tibe.{
  EApply, EArray, EFunction, EInt, ELet, EString, EVariable, NotInScope, TConstructor,
  TypeCheckScopingError, TypeCheckUnifyError, TypeMismatch,
}
import gleam/list
import gleam/map
import gleam/option.{None, Some}

pub fn main() {
  gleeunit.main()
}

pub fn scope_error_test() {
  should.equal(
    tibe.infer(initial_environment(), EVariable(name: "x")),
    Error(TypeCheckScopingError(NotInScope("x"))),
  )
}

pub fn function_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunction(
        arguments: [#("x", None), #("y", None)],
        maybe_return_type: None,
        body: EApply(
          function: EVariable(name: "+"),
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

pub fn function_annotated_test() {
  let t =
    TConstructor(
      "Function2",
      [
        TConstructor("Int", []),
        TConstructor("Int", []),
        TConstructor("Int", []),
      ],
    )
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunction(
        arguments: [
          #("x", Some(TConstructor("Int", []))),
          #("y", Some(TConstructor("Int", []))),
        ],
        maybe_return_type: Some(TConstructor("Int", [])),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
        ),
      ),
    ),
    Ok(t),
  )
}

pub fn function_type_mismatch_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EApply(
        function: EVariable(name: "+"),
        arguments: [EInt(value: 10), EString(value: "some_string")],
      ),
    ),
    Error(TypeCheckUnifyError(TypeMismatch(
      TConstructor(name: "Int", type_parameters: []),
      TConstructor(name: "String", type_parameters: []),
    ))),
  )
}

pub fn let_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        maybe_value_type: None,
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    Ok(TConstructor("Int", [])),
  )
}

pub fn let_annotated_test() {
  let t = TConstructor("Int", [])
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        maybe_value_type: Some(t),
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    Ok(t),
  )
}

pub fn array_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(maybe_item_type: None, items: [EInt(value: 10), EInt(value: 20)]),
    ),
    Ok(TConstructor("Array", [TConstructor("Int", [])])),
  )
}

pub fn array_annotated_test() {
  let t = TConstructor("Int", [])
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(
        maybe_item_type: Some(t),
        items: [EInt(value: 10), EInt(value: 20)],
      ),
    ),
    Ok(TConstructor("Array", [t])),
  )
}

pub fn array_type_mismatch_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(
        maybe_item_type: None,
        items: [EInt(value: 10), EString(value: "20")],
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
