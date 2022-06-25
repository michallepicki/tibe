import gleam/io
import gleam/map.{Map}
import gleam/list
import gleam/int

type Expression {
  ELambda(x: String, e: Expression)
  EApply(e1: Expression, e2: Expression)
  EVariable(x: String)
}

type Type {
  TConstructor(name: String, generics: List(Type))
  TVariable(index: Int)
}

type Constraint {
  CEquality(t1: Type, t2: Type)
}

type Context {
  Context(
    substitution: Map(Int, Type),
    type_constraints: List(Constraint),
    environment: Map(String, Type),
  )
}

fn fresh_type_variable(context: Context) -> #(Type, Context) {
  let i = map.size(context.substitution)
  let result = TVariable(i)
  #(
    result,
    Context(
      ..context,
      substitution: map.insert(context.substitution, i, result),
    ),
  )
}

fn infer_type(expression: Expression, context: Context) -> #(Type, Context) {
  case expression {
    ELambda(x, e) -> {
      let #(t1, context) = fresh_type_variable(context)
      let context =
        Context(..context, environment: map.insert(context.environment, x, t1))
      let #(t2, context) = infer_type(e, context)
      #(TConstructor("Function1", [t1, t2]), context)
    }
    EVariable(x) -> {
      assert Ok(t) = map.get(context.environment, x)
      #(t, context)
    }
    EApply(e1, e2) -> {
      let #(t1, context) = infer_type(e1, context)
      let #(t2, context) = infer_type(e2, context)
      let #(t3, context) = fresh_type_variable(context)
      let context =
        Context(
          ..context,
          type_constraints: [
            CEquality(t1, TConstructor("Function1", [t2, t3])),
            ..context.type_constraints
          ],
        )
      #(t3, context)
    }
  }
}

fn solve_constraints(context: Context) -> Context {
  let substitution =
    context.type_constraints
    |> list.reverse()
    |> list.fold(
      context.substitution,
      fn(substitution, constraint) {
        assert CEquality(t1, t2) = constraint
        unify(substitution, t1, t2)
      },
    )
  Context(..context, substitution: substitution, type_constraints: [])
}

fn unify(substitution: Map(Int, Type), t1: Type, t2: Type) -> Map(Int, Type) {
  case t1, t2 {
    TConstructor(name1, generics1), TConstructor(name2, generics2) -> {
      assert True = name1 == name2
      assert True = list.length(generics1) == list.length(generics2)
      list.zip(generics1, generics2)
      |> list.fold(
        substitution,
        fn(substitution, t) {
          assert #(t1, t2) = t
          unify(substitution, t1, t2)
        },
      )
    }
    TVariable(i), TVariable(j) if i == j -> substitution
    t1, t2 ->
      case follow(substitution, t1) {
        Ok(t) -> unify(substitution, t, t2)
        Error(Nil) ->
          case follow(substitution, t2) {
            Ok(t) -> unify(substitution, t1, t)
            Error(Nil) ->
              case t1, t2 {
                TVariable(i), _ -> {
                  assert False = occurs_in(substitution, i, t2)
                  map.insert(substitution, i, t2)
                }
                _, TVariable(i) -> {
                  assert False = occurs_in(substitution, i, t1)
                  map.insert(substitution, i, t1)
                }
              }
          }
      }
  }
}

fn follow(substitution, to_follow) {
  case to_follow {
    TVariable(i) ->
      case map.get(substitution, i) {
        Ok(t) if t == to_follow -> Error(Nil)
        Ok(t) -> Ok(t)
        _ ->
          todo("If we have a variable, it was initiated pointing to itself in the substitution map!")
      }
    _ -> Error(Nil)
  }
}

fn occurs_in(substitution, index, t) {
  case t {
    TVariable(i) ->
      case follow(substitution, t) {
        Ok(t) -> occurs_in(substitution, index, t)
        _ -> i == index
      }
    TConstructor(_, generics) ->
      case list.find(generics, fn(t) { occurs_in(substitution, index, t) }) {
        Ok(_) -> True
        Error(Nil) -> False
      }
  }
}

fn substitute(substitution, t: Type) -> Type {
  case t {
    TVariable(_) ->
      case follow(substitution, t) {
        Ok(t) -> substitute(substitution, t)
        _ -> t
      }
    TConstructor(name, generics) ->
      TConstructor(
        name,
        list.map(generics, fn(t) { substitute(substitution, t) }),
      )
  }
}

pub fn main() {
  let initial_environment =
    ["+", "-", "*", "/"]
    |> list.map(fn(name) {
      let t =
        TConstructor(
          "Function1",
          [
            TConstructor("Int", []),
            TConstructor(
              "Function1",
              [TConstructor("Int", []), TConstructor("Int", [])],
            ),
          ],
        )
      #(name, t)
    })
    |> map.from_list()
  let initial_environment =
    list.range(0, 99)
    |> list.map(int.to_string)
    |> list.map(fn(name) { #(name, TConstructor("Int", [])) })
    |> map.from_list()
    |> map.merge(initial_environment)

  let infer = fn(environment: Map(String, Type), expression: Expression) -> Type {
    let #(t, context) =
      infer_type(expression, Context(map.new(), [], environment))
    let context = solve_constraints(context)
    substitute(context.substitution, t)
  }

  io.debug(infer(
    initial_environment,
    ELambda("x", EApply(EApply(EVariable("+"), EVariable("x")), EVariable("x"))),
  ))
  Nil
}
