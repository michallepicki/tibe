import gleeunit
import gleeunit/should
import tibe.{
  EApply, EArray, EFunctions, EInt, ELambda, ELet, ESemicolon, EString, EVariable,
  GenericFunction, GenericType, NotInScope, OccursError, Parameter, TConstructor,
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
      ELambda(
        [Parameter("x", None), Parameter("y", None)],
        None,
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
    ),
    Ok(#(
      ELambda(
        [Parameter("x", int_type()), Parameter("y", int_type())],
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
      ELambda(
        [Parameter("x", Some(int_type())), Parameter("y", Some(int_type()))],
        Some(int_type()),
        EApply(EVariable("+", []), [EVariable("x", []), EVariable("y", [])]),
      ),
    ),
    Ok(#(
      ELambda(
        [Parameter("x", int_type()), Parameter("y", int_type())],
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
        ELambda(
          [Parameter("x", None)],
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
        ELambda(
          [Parameter("x", int_type())],
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
      EFunctions(
        [
          GenericFunction(
            "singleton",
            None,
            ELambda(
              [Parameter("x", None)],
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
      EFunctions(
        [
          GenericFunction(
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
            ELambda(
              [Parameter("x", TConstructor("GenericVar3", []))],
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
      EFunctions(
        [
          GenericFunction(
            "even",
            None,
            ELambda(
              [Parameter("x", None)],
              None,
              EApply(
                EVariable("odd", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
          GenericFunction(
            "odd",
            None,
            ELambda(
              [Parameter("x", None)],
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
      EFunctions(
        [
          GenericFunction(
            "even",
            GenericType(
              ["GenericVar3"],
              TConstructor(
                "Function1",
                [int_type(), TConstructor("GenericVar3", [])],
              ),
            ),
            ELambda(
              [Parameter("x", int_type())],
              TConstructor("GenericVar3", []),
              EApply(
                EVariable("odd", []),
                [EApply(EVariable("-", []), [EVariable("x", []), EInt(1)])],
              ),
            ),
          ),
          GenericFunction(
            "odd",
            GenericType(
              ["GenericVar3"],
              TConstructor(
                "Function1",
                [int_type(), TConstructor("GenericVar3", [])],
              ),
            ),
            ELambda(
              [Parameter("x", int_type())],
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
      EFunctions(
        [
          GenericFunction(
            "even",
            None,
            ELambda(
              [Parameter("x", None)],
              None,
              EApply(
                EVariable("if", []),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  ELambda([], None, EVariable("true", [])),
                  ELambda(
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
          GenericFunction(
            "odd",
            None,
            ELambda(
              [Parameter("x", None)],
              None,
              EApply(
                EVariable("if", []),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  ELambda([], None, EVariable("false", [])),
                  ELambda(
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
      EFunctions(
        [
          GenericFunction(
            "even",
            GenericType(
              [],
              TConstructor("Function1", [int_type(), bool_type()]),
            ),
            ELambda(
              [Parameter("x", int_type())],
              bool_type(),
              EApply(
                EVariable("if", [bool_type()]),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  ELambda([], bool_type(), EVariable("true", [])),
                  ELambda(
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
          GenericFunction(
            "odd",
            GenericType(
              [],
              TConstructor("Function1", [int_type(), bool_type()]),
            ),
            ELambda(
              [Parameter("x", int_type())],
              bool_type(),
              EApply(
                EVariable("if", [bool_type()]),
                [
                  EApply(EVariable("==", []), [EVariable("x", []), EInt(0)]),
                  ELambda([], bool_type(), EVariable("false", [])),
                  ELambda(
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
      EFunctions(
        [
          GenericFunction(
            "id",
            Some(GenericType(
              ["A"],
              TConstructor(
                "Function1",
                [TConstructor("A", []), TConstructor("A", [])],
              ),
            )),
            ELambda([Parameter("x", None)], None, EVariable("x", [])),
          ),
        ],
        EApply(EVariable("id", []), [EString("foo")]),
      ),
    ),
    Ok(#(
      EFunctions(
        [
          GenericFunction(
            "id",
            GenericType(
              ["A"],
              TConstructor(
                "Function1",
                [TConstructor("A", []), TConstructor("A", [])],
              ),
            ),
            ELambda(
              [Parameter("x", TConstructor("A", []))],
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

pub fn e5_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      ELambda(
        [Parameter("x", None)],
        None,
        EApply(EVariable("x", []), [EVariable("x", [])]),
      ),
    ),
    Error(OccursError(
      index: 3,
      t: TConstructor("Function1", [TVariable(3), TVariable(1)]),
    )),
  )
}

pub fn e6_test() {
  should.equal(
    tibe.infer(
      initial_environment(),
      EFunctions(
        [
          GenericFunction(
            "compose",
            None,
            ELambda(
              [Parameter("f", None), Parameter("g", None)],
              None,
              ELambda(
                [Parameter("x", None)],
                None,
                EApply(
                  EVariable("f", []),
                  [EApply(EVariable("g", []), [EVariable("x", [])])],
                ),
              ),
            ),
          ),
        ],
        EVariable("compose", []),
      ),
    ),
    Ok(#(
      EFunctions(
        [
          GenericFunction(
            "compose",
            GenericType(
              ["GenericVar5", "GenericVar6", "GenericVar7"],
              TConstructor(
                "Function2",
                [
                  TConstructor(
                    "Function1",
                    [
                      TConstructor("GenericVar7", []),
                      TConstructor("GenericVar5", []),
                    ],
                  ),
                  TConstructor(
                    "Function1",
                    [
                      TConstructor("GenericVar6", []),
                      TConstructor("GenericVar7", []),
                    ],
                  ),
                  TConstructor(
                    "Function1",
                    [
                      TConstructor("GenericVar6", []),
                      TConstructor("GenericVar5", []),
                    ],
                  ),
                ],
              ),
            ),
            ELambda(
              [
                Parameter(
                  "f",
                  TConstructor(
                    "Function1",
                    [
                      TConstructor("GenericVar7", []),
                      TConstructor("GenericVar5", []),
                    ],
                  ),
                ),
                Parameter(
                  "g",
                  TConstructor(
                    "Function1",
                    [
                      TConstructor("GenericVar6", []),
                      TConstructor("GenericVar7", []),
                    ],
                  ),
                ),
              ],
              TConstructor(
                "Function1",
                [
                  TConstructor("GenericVar6", []),
                  TConstructor("GenericVar5", []),
                ],
              ),
              ELambda(
                [Parameter("x", TConstructor("GenericVar6", []))],
                TConstructor("GenericVar5", []),
                EApply(
                  EVariable("f", []),
                  [EApply(EVariable("g", []), [EVariable("x", [])])],
                ),
              ),
            ),
          ),
        ],
        EVariable("compose", [TVariable(9), TVariable(10), TVariable(11)]),
      ),
      TConstructor(
        "Function2",
        [
          TConstructor("Function1", [TVariable(11), TVariable(9)]),
          TConstructor("Function1", [TVariable(10), TVariable(11)]),
          TConstructor("Function1", [TVariable(10), TVariable(9)]),
        ],
      ),
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
