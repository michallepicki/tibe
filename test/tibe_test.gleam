import gleeunit
import gleeunit/should
import tibe.{
  EApply, EArray, EFunction, EInt, ELet, ERecursiveFunctions, EString, EVariable,
  FunctionArgument, NotInScope, RecursiveFunction, TConstructor, TypeMismatch,
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
    Error(NotInScope("x")),
  )
}

pub fn function_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunction(
        arguments: [
          FunctionArgument(name: "x", argument_type: None),
          FunctionArgument(name: "y", argument_type: None),
        ],
        return_type: None,
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
        ),
      ),
    ),
    Ok(#(
      EFunction(
        arguments: [
          FunctionArgument(name: "x", argument_type: TConstructor("Int", [])),
          FunctionArgument(name: "y", argument_type: TConstructor("Int", [])),
        ],
        return_type: TConstructor("Int", []),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
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
          FunctionArgument(
            name: "x",
            argument_type: Some(TConstructor("Int", [])),
          ),
          FunctionArgument(
            name: "y",
            argument_type: Some(TConstructor("Int", [])),
          ),
        ],
        return_type: Some(TConstructor("Int", [])),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
        ),
      ),
    ),
    Ok(#(
      EFunction(
        arguments: [
          FunctionArgument(name: "x", argument_type: TConstructor("Int", [])),
          FunctionArgument(name: "y", argument_type: TConstructor("Int", [])),
        ],
        return_type: TConstructor("Int", []),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "y")],
        ),
      ),
      t,
    )),
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
    Error(TypeMismatch(int_type(), string_type())),
  )
}

pub fn let_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        value_type: None,
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    Ok(#(
      ELet(
        name: "x",
        value_type: TConstructor("Int", []),
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
      TConstructor("Int", []),
    )),
  )
}

pub fn let_annotated_test() {
  let t = TConstructor("Int", [])
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "x",
        value_type: Some(t),
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
    ),
    Ok(#(
      ELet(
        name: "x",
        value_type: t,
        value: EInt(value: 10),
        body: EApply(
          function: EVariable(name: "+"),
          arguments: [EVariable(name: "x"), EVariable(name: "x")],
        ),
      ),
      t,
    )),
  )
}

pub fn let_apply_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        name: "singleton",
        value_type: None,
        value: EFunction(
          arguments: [FunctionArgument(name: "x", argument_type: None)],
          return_type: None,
          body: EArray(item_type: None, items: [EVariable(name: "x")]),
        ),
        body: EApply(
          function: EVariable("singleton"),
          arguments: [EInt(value: 42)],
        ),
      ),
    ),
    Ok(#(
      ELet(
        "singleton",
        TConstructor(
          "Function1",
          [
            TConstructor("Int", []),
            TConstructor("Array", [TConstructor("Int", [])]),
          ],
        ),
        EFunction(
          arguments: [
            FunctionArgument(name: "x", argument_type: TConstructor("Int", [])),
          ],
          return_type: TConstructor("Array", [TConstructor("Int", [])]),
          body: EArray(
            item_type: TConstructor("Int", []),
            items: [EVariable(name: "x")],
          ),
        ),
        EApply(
          function: EVariable(name: "singleton"),
          arguments: [EInt(value: 42)],
        ),
      ),
      TConstructor("Array", [TConstructor("Int", [])]),
    )),
  )
}

pub fn array_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(item_type: None, items: [EInt(value: 10), EInt(value: 20)]),
    ),
    Ok(#(
      EArray(
        item_type: TConstructor("Int", []),
        items: [EInt(value: 10), EInt(value: 20)],
      ),
      TConstructor("Array", [TConstructor("Int", [])]),
    )),
  )
}

pub fn array_annotated_test() {
  let t = TConstructor("Int", [])
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(item_type: Some(t), items: [EInt(value: 10), EInt(value: 20)]),
    ),
    Ok(#(
      EArray(item_type: t, items: [EInt(value: 10), EInt(value: 20)]),
      TConstructor("Array", [t]),
    )),
  )
}

pub fn array_type_mismatch_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EArray(item_type: None, items: [EInt(value: 10), EString(value: "20")]),
    ),
    Error(TypeMismatch(int_type(), string_type())),
  )
}

pub fn recursive_functions_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ERecursiveFunctions(
        functions: [
          RecursiveFunction(
            name: "even",
            function_type: None,
            lambda: EFunction(
              arguments: [FunctionArgument(name: "x", argument_type: None)],
              return_type: Some(int_type()),
              body: EApply(
                function: EVariable(name: "odd"),
                arguments: [
                  EApply(
                    function: EVariable(name: "-"),
                    arguments: [EVariable(name: "x"), EInt(value: 1)],
                  ),
                ],
              ),
            ),
          ),
          RecursiveFunction(
            name: "odd",
            function_type: None,
            lambda: EFunction(
              arguments: [FunctionArgument(name: "x", argument_type: None)],
              return_type: None,
              body: EApply(
                function: EVariable(name: "even"),
                arguments: [
                  EApply(
                    function: EVariable(name: "-"),
                    arguments: [EVariable(name: "x"), EInt(value: 1)],
                  ),
                ],
              ),
            ),
          ),
        ],
        body: EApply(EVariable("even"), [EInt(42)]),
      ),
    ),
    Ok(#(
      ERecursiveFunctions(
        functions: [
          RecursiveFunction(
            name: "even",
            function_type: TConstructor(
              name: "Function1",
              type_parameters: [int_type(), int_type()],
            ),
            lambda: EFunction(
              arguments: [
                FunctionArgument(name: "x", argument_type: int_type()),
              ],
              return_type: int_type(),
              body: EApply(
                function: EVariable(name: "odd"),
                arguments: [
                  EApply(
                    function: EVariable(name: "-"),
                    arguments: [EVariable(name: "x"), EInt(value: 1)],
                  ),
                ],
              ),
            ),
          ),
          RecursiveFunction(
            name: "odd",
            function_type: TConstructor(
              name: "Function1",
              type_parameters: [int_type(), int_type()],
            ),
            lambda: EFunction(
              arguments: [
                FunctionArgument(name: "x", argument_type: int_type()),
              ],
              return_type: int_type(),
              body: EApply(
                function: EVariable(name: "even"),
                arguments: [
                  EApply(
                    function: EVariable(name: "-"),
                    arguments: [EVariable(name: "x"), EInt(value: 1)],
                  ),
                ],
              ),
            ),
          ),
        ],
        body: EApply(EVariable("even"), [EInt(42)]),
      ),
      int_type(),
    )),
  )
}

fn initial_environment() {
  let env =
    list.fold(
      ["+", "-", "*", "/"],
      map.new(),
      fn(acc, name) {
        let t =
          TConstructor(
            name: "Function2",
            type_parameters: [int_type(), int_type(), int_type()],
          )

        map.insert(acc, name, t)
      },
    )

  let env =
    list.fold(
      ["false", "true"],
      env,
      fn(acc, name) {
        let t = TConstructor(name: "Bool", type_parameters: [])

        map.insert(acc, name, t)
      },
    )

  let env =
    list.fold(
      ["==", "!=", "<", ">"],
      env,
      fn(acc, name) {
        let t =
          TConstructor(
            name: "Function2",
            type_parameters: [int_type(), int_type(), bool_type()],
          )

        map.insert(acc, name, t)
      },
    )
  env
}

fn int_type() {
  TConstructor(name: "Int", type_parameters: [])
}

fn string_type() {
  TConstructor(name: "String", type_parameters: [])
}

fn bool_type() {
  TConstructor(name: "Bool", type_parameters: [])
}
