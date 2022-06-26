//// [Type Inference by Example](https://ahnfelt.medium.com/type-inference-by-example-793d83f98382)
//// but implemented in Gleam.
//// 
//// Part 6d: Multiple function arguments, let expressions,
//// int and string literals, arrays

import gleam/map.{Map}
import gleam/list
import gleam/string
import gleam/int

/// AST type representing our language.
///
/// An Expression is the input of our typechecker program.
/// In `infer_type` it is processed to Types, Substutions, Environment mappings,
/// and Constraints.
pub type Expression {
  /// A lambda expression (an "abstraction") is a function.
  /// It is defined by a list of argument names and the expression
  /// it evaluates to (function body).
  ELambda(arguments: List(String), body: Expression)
  /// An application is a function call. It is defined by a lambda expression
  /// which will be called, and the argument expressions being passed
  /// to this lambda.
  EApply(lambda: Expression, arguments: List(Expression))
  /// A let expression allows to bind some value to a name, so that it can be
  /// accessed in the current scope (inside of let's body).
  ELet(name: String, value: Expression, body: Expression)
  /// An expression variable is a name that is bound to some value.
  /// This can be a lambda argument, a let binding, or some predefined value
  /// like the "+" abstraction.
  EVariable(name: String)
  /// Literal integer expression
  EInt(value: Int)
  /// Literal string expression
  EString(value: String)
  /// Literal array expression
  EArray(items: List(Expression))
}

/// The Type type represents the types of our small lanuguage.
///
/// It is used internally starting from the output of `infer_type` throughout
/// the constraint solving (unification) to substitution. It is the output
/// of our typechecker program.
pub type Type {
  /// A type constructor names a type and applies it to a list
  /// of type parameters. For example, it can be a simple type like Int
  /// with no type parameters, or a Function1 type with 2 type parameters:
  /// its argument type and return type.
  TConstructor(name: String, type_parameters: List(Type))
  /// A type variable is an internal typechecker representation
  /// of a non concrete type. It either was not yet inferred or substituted
  /// to a concrete type, or (for polymorphic functions) it represents
  /// a type parameter.
  TVariable(index: Int)
}

/// The context holds all data useful for type checking collected along the way.
pub type Context {
  Context(
    environment: Environment,
    type_constraints: TypeConstraints,
    substitution: Substitution,
  )
}

/// The environment maps bound value names to their types (either concrete
/// types, or just type variables)
pub type Environment =
  Map(String, Type)

/// A constraint represents the dependency between two type variables
/// or concrete types, that still needs to be checked.
pub type Constraint {
  CEquality(t1: Type, t2: Type)
}

/// The list of type constraints is one of the results of the `infer_type`
/// function, and get checked ("solved") using unification.
pub type TypeConstraints =
  List(Constraint)

/// The Substitution map holds the mapping from type variables indexes,
/// to their actual type values. Initially for each type variable
/// it points to itself. As the type constraints get solved in unification,
/// it points to more concrete types for them.
pub type Substitution =
  Map(Int, Type)

pub type ScopingError {
  NotInScope(name: String)
}

pub type UnifyError {
  OccursError(index: Int, t: Type)
  TypeMismatch(t1: Type, t2: Type)
}

pub type TypeCheckError {
  TypeCheckScopingError(ScopingError)
  TypeCheckUnifyError(UnifyError)
}

pub fn main() {
  Nil
}

pub fn infer(
  environment: Environment,
  expression: Expression,
) -> Result(Type, TypeCheckError) {
  case infer_type(
    expression,
    Context(
      environment: environment,
      type_constraints: [],
      substitution: map.new(),
    ),
  ) {
    Error(scoping_error) -> Error(TypeCheckScopingError(scoping_error))
    Ok(#(t, context)) ->
      case solve_constraints(context) {
        Ok(context) -> Ok(substitute(context.substitution, t))
        Error(unify_error) -> Error(TypeCheckUnifyError(unify_error))
      }
  }
}

/// A function which takes an expression, and recursively turns it into
/// (potentially unsolved) type variables, introducing new expression variables
/// and checking that referenced expression variables exist along the way.
pub fn infer_type(
  expression: Expression,
  context: Context,
) -> Result(#(Type, Context), ScopingError) {
  case expression {
    ELambda(arguments: args, body: e) -> {
      let #(argument_types_reversed, context) =
        list.fold(
          args,
          #([], context),
          fn(acc, arg_name) {
            let #(argument_types_acc, context) = acc
            let #(t, context) = fresh_type_variable(in: context)
            let context =
              Context(
                ..context,
                environment: map.insert(context.environment, arg_name, t),
              )
            #([t, ..argument_types_acc], context)
          },
        )
      try #(return_type, context) = infer_type(e, context)
      let type_parameters =
        list.reverse([return_type, ..argument_types_reversed])
      let type_name =
        string.join(
          ["Function", int.to_string(list.length(argument_types_reversed))],
          with: "",
        )
      let t = TConstructor(name: type_name, type_parameters: type_parameters)
      Ok(#(t, context))
    }
    EApply(lambda: lambda, arguments: args) -> {
      try #(lambda_type, context) = infer_type(lambda, context)
      try #(argument_types_reversed, context) =
        list.fold(
          args,
          Ok(#([], context)),
          fn(acc, arg) {
            case acc {
              Ok(#(argument_types_acc, context)) -> {
                try #(t, context) = infer_type(arg, context)
                Ok(#([t, ..argument_types_acc], context))
              }
              e -> e
            }
          },
        )
      let #(return_type_var, context) = fresh_type_variable(in: context)
      let type_parameters =
        list.reverse([return_type_var, ..argument_types_reversed])
      let type_name =
        string.join(["Function", int.to_string(list.length(args))], with: "")
      let context =
        Context(
          ..context,
          type_constraints: [
            CEquality(
              lambda_type,
              TConstructor(name: type_name, type_parameters: type_parameters),
            ),
            ..context.type_constraints
          ],
        )
      Ok(#(return_type_var, context))
    }
    ELet(name: name, value: value, body: body) -> {
      try #(value_type, context) = infer_type(value, context)
      let context =
        Context(
          ..context,
          environment: map.insert(context.environment, name, value_type),
        )
      infer_type(body, context)
    }
    EVariable(name: x) ->
      case map.get(context.environment, x) {
        Ok(t) -> Ok(#(t, context))
        Error(_) -> Error(NotInScope(x))
      }
    EInt(_) -> Ok(#(TConstructor("Int", []), context))
    EString(_) -> Ok(#(TConstructor("String", []), context))
    EArray(items: items) -> {
      let #(item_type_var, context) = fresh_type_variable(context)
      try context =
        items
        |> list.fold(
          Ok(context),
          fn(context_result, item_expression) {
            case context_result {
              Ok(context) -> {
                try #(t, context) = infer_type(item_expression, context)
                let context =
                  Context(
                    ..context,
                    type_constraints: [
                      CEquality(item_type_var, t),
                      ..context.type_constraints
                    ],
                  )
                Ok(context)
              }
              e -> e
            }
          },
        )
      Ok(#(TConstructor("Array", [item_type_var]), context))
    }
  }
}

/// A helper function used to assign a new index to a type variable,
/// and initialize it correctly in the substitution map in the context.
pub fn fresh_type_variable(in context: Context) -> #(Type, Context) {
  let i = map.size(context.substitution)
  let t = TVariable(index: i)
  let context =
    Context(..context, substitution: map.insert(context.substitution, i, t))
  #(t, context)
}

/// A function which "solves" (and gets rid of) type constraints
/// using unification.
pub fn solve_constraints(context: Context) -> Result(Context, UnifyError) {
  try substitution =
    context.type_constraints
    |> list.reverse()
    |> list.fold(
      Ok(context.substitution),
      fn(substitution_result, constraint) {
        case substitution_result {
          Ok(substitution) -> {
            assert CEquality(t1, t2) = constraint
            unify(substitution, t1, t2)
          }
          e -> e
        }
      },
    )
  Ok(Context(..context, type_constraints: [], substitution: substitution))
}

/// A function which checks that two types are (or can be) the same,
/// returns an error if they cannot, and returns an updated Substitution
/// mapping.
pub fn unify(
  substitution: Substitution,
  t1: Type,
  t2: Type,
) -> Result(Substitution, UnifyError) {
  case t1, t2 {
    TConstructor(name: name1, type_parameters: generics1), TConstructor(
      name: name2,
      type_parameters: generics2,
    ) ->
      case name1 != name2 || list.length(generics1) != list.length(generics2) {
        True -> Error(TypeMismatch(t1, t2))
        False ->
          list.zip(generics1, generics2)
          |> list.fold(
            Ok(substitution),
            fn(unify_result, t) {
              case unify_result {
                Ok(substitution) -> {
                  let #(t1, t2) = t
                  unify(substitution, t1, t2)
                }
                e -> e
              }
            },
          )
      }
    TVariable(index: i), TVariable(index: j) if i == j -> Ok(substitution)
    t1, t2 ->
      case follow(substitution, t1) {
        Ok(t) -> unify(substitution, t, t2)
        Error(Nil) ->
          case follow(substitution, t2) {
            Ok(t) -> unify(substitution, t1, t)
            Error(Nil) ->
              case t1, t2 {
                TVariable(index: i), _ ->
                  case occurs_in(substitution, i, t2) {
                    False -> Ok(map.insert(substitution, i, t2))
                    True -> Error(OccursError(i, t2))
                  }
                _, TVariable(index: i) ->
                  case occurs_in(substitution, i, t1) {
                    False -> Ok(map.insert(substitution, i, t1))
                    True -> Error(OccursError(i, t1))
                  }
                _, _ -> Error(TypeMismatch(t1, t2))
              }
          }
      }
  }
}

/// A helper function used to get the type some type variable (to_follow)
/// has been already unified with.
pub fn follow(substitution: Substitution, to_follow: Type) -> Result(Type, Nil) {
  case to_follow {
    TVariable(index: i) -> {
      assert Ok(t) = map.get(substitution, i)
      case t != to_follow {
        True -> Ok(t)
        False -> Error(Nil)
      }
    }
    _ -> Error(Nil)
  }
}

/// A function to help with checking that the types are not recursive.
pub fn occurs_in(substitution: Substitution, index: Int, t: Type) -> Bool {
  case t {
    TVariable(index: i) ->
      case follow(substitution, t) {
        Ok(t) -> occurs_in(substitution, index, t)
        _ -> i == index
      }
    TConstructor(name: _, type_parameters: generics) ->
      case list.find(generics, fn(t) { occurs_in(substitution, index, t) }) {
        Ok(_) -> True
        Error(Nil) -> False
      }
  }
}

/// A function to recursively replace all type variables inside a type
/// at the end of type checking with concrete types (where possible).
pub fn substitute(substitution: Substitution, t: Type) -> Type {
  case t {
    TVariable(_) ->
      case follow(substitution, t) {
        Ok(t) -> substitute(substitution, t)
        _ -> t
      }
    TConstructor(name: name, type_parameters: generics) ->
      TConstructor(
        name: name,
        type_parameters: list.map(
          generics,
          fn(t) { substitute(substitution, t) },
        ),
      )
  }
}
