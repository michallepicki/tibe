import gleeunit
import gleeunit/should
import tibe.{
  EApply, EArray, EFunction, EInt, ELet, ERecursiveFunctions, ESemicolon, EString,
  EVariable, FunctionArgument, GenericType, NotInScope, RecursiveFunction, TConstructor,
  TVariable, TypeMismatch,
}
import gleam/list
import gleam/map
import gleam/option.{None, Some}

pub fn main() {
  gleeunit.main()
}

pub fn scope_error_test() {
  should.equal(
    tibe.infer(initial_environment(), EVariable("x", [])),
    Error(NotInScope("x")),
  )
}

pub fn function_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunction(
        [FunctionArgument("x", None), FunctionArgument("y", None)],
        None,
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
    ),
    Ok(#(
      EFunction(
        [FunctionArgument("x", int_type()), FunctionArgument("y", int_type())],
        int_type(),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
      TConstructor("Function2", [int_type(), int_type(), int_type()]),
    )),
  )
}

pub fn function_annotated_test() {
  let t = TConstructor("Function2", [int_type(), int_type(), int_type()])
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunction(
        [
          FunctionArgument("x", Some(int_type())),
          FunctionArgument("y", Some(int_type())),
        ],
        Some(int_type()),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
    ),
    Ok(#(
      EFunction(
        [FunctionArgument("x", int_type()), FunctionArgument("y", int_type())],
        int_type(),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
      t,
    )),
  )
}

pub fn function_type_mismatch_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EApply(EVariable("+", []), [EInt(10), EString("some_string")]),
    ),
    Error(TypeMismatch(int_type(), string_type())),
  )
}

pub fn let_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        "x",
        None,
        EInt(10),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("x", [])]),
      ),
    ),
    Ok(#(
      ELet(
        "x",
        int_type(),
        EInt(10),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("x", [])]),
      ),
      int_type(),
    )),
  )
}

pub fn let_annotated_test() {
  let t = int_type()
  should.equal(
    tibe.infer(
      initial_environment(),
      ELet(
        "x",
        Some(t),
        EInt(10),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("x", [])]),
      ),
    ),
    Ok(#(
      ELet(
        "x",
        t,
        EInt(10),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("x", [])]),
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
        "singleton",
        None,
        EFunction(
          [FunctionArgument("x", None)],
          None,
          EArray(None, [EVariable("x", [])]),
        ),
        EApply(EVariable("singleton", []), [EInt(42)]),
      ),
    ),
    Ok(#(
      ELet(
        "singleton",
        TConstructor(
          "Function1",
          [int_type(), TConstructor("Array", [int_type()])],
        ),
        EFunction(
          [FunctionArgument("x", int_type())],
          TConstructor("Array", [int_type()]),
          EArray(int_type(), [EVariable("x", [])]),
        ),
        EApply(EVariable("singleton", []), [EInt(42)]),
      ),
      TConstructor("Array", [int_type()]),
    )),
  )
}

pub fn array_test() {
  should.equal(
    tibe.infer(initial_environment(), EArray(None, [EInt(10), EInt(20)])),
    Ok(#(
      EArray(int_type(), [EInt(10), EInt(20)]),
      TConstructor("Array", [int_type()]),
    )),
  )
}

pub fn array_annotated_test() {
  let t = int_type()
  should.equal(
    tibe.infer(initial_environment(), EArray(Some(t), [EInt(10), EInt(20)])),
    Ok(#(EArray(t, [EInt(10), EInt(20)]), TConstructor("Array", [t]))),
  )
}

pub fn array_type_mismatch_test() {
  should.equal(
    tibe.infer(initial_environment(), EArray(None, [EInt(10), EString("20")])),
    Error(TypeMismatch(int_type(), string_type())),
  )
}

pub fn e1_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "singleton",
            None,
            EFunction(
              [FunctionArgument("x", None)],
              None,
              EArray(None, [EVariable("x", [])]),
            ),
          ),
        ],
        ESemicolon(
          EApply(EVariable("singleton", []), [EInt(42)]),
          EApply(EVariable("singleton", []), [EString("foo")]),
        ),
      ),
    ),
    Ok(#(
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "singleton",
            GenericType(
              ["GenericVar3"],
              TConstructor(
                "Function1",
                [
                  TConstructor("GenericVar3", []),
                  TConstructor("Array", [TConstructor("GenericVar3", [])]),
                ],
              ),
            ),
            EFunction(
              [FunctionArgument("x", TConstructor("GenericVar3", []))],
              TConstructor("Array", [TConstructor("GenericVar3", [])]),
              EArray(TConstructor("GenericVar3", []), [EVariable("x", [])]),
            ),
          ),
        ],
        ESemicolon(
          EApply(EVariable("singleton", [int_type()]), [EInt(42)]),
          EApply(EVariable("singleton", [string_type()]), [EString("foo")]),
        ),
      ),
      TConstructor("Array", [string_type()]),
    )),
  )
}

pub fn e2_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "even",
            None,
            EFunction(
              [FunctionArgument("x", None)],
              None,
              EApply(
                EVariable("odd", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
          RecursiveFunction(
            "odd",
            None,
            EFunction(
              [FunctionArgument("x", None)],
              None,
              EApply(
                EVariable("even", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
        ],
        EApply(EVariable("even", []), [EInt(42)]),
      ),
    ),
    Ok(#(
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "even",
            GenericType(
              ["GenericVar3"],
              TConstructor(
                "Function1",
                [int_type(), TConstructor("GenericVar3", [])],
              ),
            ),
            EFunction(
              [FunctionArgument("x", int_type())],
              TConstructor("GenericVar3", []),
              EApply(
                EVariable("odd", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
          RecursiveFunction(
            "odd",
            GenericType(
              ["GenericVar3"],
              TConstructor(
                "Function1",
                [int_type(), TConstructor("GenericVar3", [])],
              ),
            ),
            EFunction(
              [FunctionArgument("x", int_type())],
              TConstructor("GenericVar3", []),
              EApply(
                EVariable("even", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
        ],
        EApply(EVariable("even", [TVariable(14)]), [EInt(42)]),
      ),
      TVariable(14),
    )),
  )
  // TODO: this seems wrong, why is a type variable returned from inference here?
  // But it's the same as in the tutorial's example, so...
}

pub fn e3_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "even",
            None,
            EFunction(
              [FunctionArgument("x", None)],
              None,
              EApply(
                EVariable("if", []),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  EFunction([], None, EVariable("true", [])),
                  EFunction(
                    [],
                    None,
                    EApply(
                      EVariable("odd", []),
                      [
                        EApply(
                          EVariable("-", []),
                          [EVariable("x", []), EInt(1)],
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
          RecursiveFunction(
            "odd",
            None,
            EFunction(
              [FunctionArgument("x", None)],
              None,
              EApply(
                EVariable("if", []),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  EFunction([], None, EVariable("false", [])),
                  EFunction(
                    [],
                    None,
                    EApply(
                      EVariable("even", []),
                      [
                        EApply(
                          EVariable("-", []),
                          [EVariable("x", []), EInt(1)],
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
        ],
        EApply(EVariable("even", []), [EInt(42)]),
      ),
    ),
    Ok(#(
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "even",
            GenericType(
              [],
              TConstructor("Function1", [int_type(), bool_type()]),
            ),
            EFunction(
              [FunctionArgument("x", int_type())],
              bool_type(),
              EApply(
                EVariable("if", [bool_type()]),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  EFunction([], bool_type(), EVariable("true", [])),
                  EFunction(
                    [],
                    bool_type(),
                    EApply(
                      EVariable("odd", []),
                      [
                        EApply(
                          EVariable("-", []),
                          [EVariable("x", []), EInt(1)],
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
          RecursiveFunction(
            "odd",
            GenericType(
              [],
              TConstructor("Function1", [int_type(), bool_type()]),
            ),
            EFunction(
              [FunctionArgument("x", int_type())],
              bool_type(),
              EApply(
                EVariable("if", [bool_type()]),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  EFunction([], bool_type(), EVariable("false", [])),
                  EFunction(
                    [],
                    bool_type(),
                    EApply(
                      EVariable("even", []),
                      [
                        EApply(
                          EVariable("-", []),
                          [EVariable("x", []), EInt(1)],
                        ),
                      ],
                    ),
                  ),
                ],
              ),
            ),
          ),
        ],
        EApply(EVariable("even", []), [EInt(42)]),
      ),
      bool_type(),
    )),
  )
}

pub fn e4_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "id",
            Some(GenericType(
              ["A"],
              TConstructor(
                "Function1",
                [TConstructor("A", []), TConstructor("A", [])],
              ),
            )),
            EFunction([FunctionArgument("x", None)], None, EVariable("x", [])),
          ),
        ],
        EApply(EVariable("id", []), [EString("foo")]),
      ),
    ),
    Ok(#(
      ERecursiveFunctions(
        [
          RecursiveFunction(
            "id",
            GenericType(
              ["A"],
              TConstructor(
                "Function1",
                [TConstructor("A", []), TConstructor("A", [])],
              ),
            ),
            EFunction(
              [FunctionArgument("x", TConstructor("A", []))],
              TConstructor("A", []),
              EVariable("x", []),
            ),
          ),
        ],
        EApply(EVariable("id", [string_type()]), [EString("foo")]),
      ),
      string_type(),
    )),
  )
}

fn initial_environment() {
  let env =
    list.fold(
      ["+", "-", "*", "/"],
      map.new(),
      fn(acc, name) {
        let t = TConstructor("Function2", [int_type(), int_type(), int_type()])

        map.insert(acc, name, GenericType([], t))
      },
    )

  let env =
    list.fold(
      ["false", "true"],
      env,
      fn(acc, name) {
        let t = bool_type()

        map.insert(acc, name, GenericType([], t))
      },
    )

  let env =
    list.fold(
      ["==", "!=", "<", ">"],
      env,
      fn(acc, name) {
        let t = TConstructor("Function2", [int_type(), int_type(), bool_type()])

        map.insert(acc, name, GenericType([], t))
      },
    )
  let env =
    map.insert(
      env,
      "if",
      GenericType(
        ["T"],
        TConstructor(
          "Function3",
          [
            bool_type(),
            TConstructor("Function0", [TConstructor("T", [])]),
            TConstructor("Function0", [TConstructor("T", [])]),
            TConstructor("T", []),
          ],
        ),
      ),
    )
  env
}

fn int_type() {
  TConstructor("Int", [])
}

fn string_type() {
  TConstructor("String", [])
}

fn bool_type() {
  TConstructor("Bool", [])
}
