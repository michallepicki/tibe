//// [Type Inference by Example](https://ahnfelt.medium.com/type-inference-by-example-793d83f98382)
//// but implemented in Gleam.
//// 
//// Part 6e: type annotations

import gleam/map.{Map}
import gleam/list
import gleam/string
import gleam/int
import gleam/option.{None, Option, Some}
import gleam/result

/// AST type representing our language.
///
/// An Expression is the input of our typechecker program.
/// In `infer_type` it is processed to Types, Substutions, Environment mappings,
/// and Constraints.
pub type Expression {
  /// A function is defined by a list of argument names and the expression
  /// it evaluates to (function body).
  EFunction(
    arguments: List(#(String, Option(Type))),
    maybe_return_type: Option(Type),
    body: Expression,
  )
  /// An application is a function call. It is defined by a function expression
  /// which will be called, and the argument expressions being passed
  /// to this function.
  EApply(function: Expression, arguments: List(Expression))
  /// A let expression allows to bind some value to a name, so that it can be
  /// accessed in the current scope (inside of let's body).
  ELet(
    name: String,
    maybe_value_type: Option(Type),
    value: Expression,
    body: Expression,
  )
  /// An expression variable is a name that is bound to some value.
  /// This can be a function argument, a let binding, or some predefined value
  /// like the "+" abstraction.
  EVariable(name: String)
  /// Literal integer expression
  EInt(value: Int)
  /// Literal string expression
  EString(value: String)
  /// Literal array expression
  EArray(maybe_item_type: Option(Type), items: List(Expression))
}

/// The Type type represents the types of our small lanuguage.
///
/// It is used internally starting from the input (expected type)
/// of `infer_type` throughout the constraint solving (unification)
/// to substitution. It is the output of our typechecker program.
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
  let context =
    Context(
      environment: environment,
      type_constraints: [],
      substitution: map.new(),
    )
  let #(t, context) = fresh_type_variable(context)
  case infer_type(expression, t, context) {
    Error(scoping_error) -> Error(TypeCheckScopingError(scoping_error))
    Ok(context) ->
      case solve_constraints(context) {
        Ok(context) -> Ok(substitute(context.substitution, t))
        Error(unify_error) -> Error(TypeCheckUnifyError(unify_error))
      }
  }
}

/// A function which takes an expression and its expected type
/// (to infer without checking, pass a fresh type variable as expected_type)
/// and recursively turns it into (potentially unsolved) type variables,
/// introducing new expression variables and checking that referenced expression
/// variables exist along the way.
pub fn infer_type(
  expression: Expression,
  expected_type: Type,
  context: Context,
) -> Result(Context, ScopingError) {
  case expression {
    EFunction(arguments: args, maybe_return_type: maybe_return_type, body: body) -> {
      let #(return_type, context) =
        type_or_fresh_variable(maybe_return_type, context)
      let #(_arguments_reversed, arg_types_reversed, context) =
        list.fold(
          args,
          #([], [], context),
          fn(acc, arg) {
            let #(arguments_acc, arg_types_acc, context) = acc
            let #(arg_name, arg_value_type) = arg
            let #(t, context) = type_or_fresh_variable(arg_value_type, context)
            let context =
              Context(
                ..context,
                environment: map.insert(context.environment, arg_name, t),
              )
            #(
              [#(arg_name, Some(t)), ..arguments_acc],
              [t, ..arg_types_acc],
              context,
            )
          },
        )
      try context = infer_type(body, return_type, context)
      let type_parameters = list.reverse([return_type, ..arg_types_reversed])
      let type_name =
        string.join(
          ["Function", int.to_string(list.length(arg_types_reversed))],
          with: "",
        )
      let t = TConstructor(name: type_name, type_parameters: type_parameters)
      let context = constrain_type(expected_type, t, context)
      Ok(context)
    }
    EApply(function: function, arguments: arguments) -> {
      let #(argument_types_reversed, context) =
        list.fold(
          arguments,
          #([], context),
          fn(acc, _arg) {
            let #(argument_types_acc, context) = acc
            let #(t, context) = fresh_type_variable(context)
            #([t, ..argument_types_acc], context)
          },
        )
      let type_name =
        string.join(
          ["Function", int.to_string(list.length(arguments))],
          with: "",
        )
      let type_parameters =
        list.reverse([expected_type, ..argument_types_reversed])
      let function_type =
        TConstructor(name: type_name, type_parameters: type_parameters)
      try context = infer_type(function, function_type, context)
      arguments
      |> list.zip(list.reverse(argument_types_reversed))
      |> list.try_fold(
        context,
        fn(context, argument_with_type) {
          let #(argument, argument_type) = argument_with_type
          infer_type(argument, argument_type, context)
        },
      )
    }
    ELet(
      name: name,
      maybe_value_type: maybe_value_type,
      value: value,
      body: body,
    ) -> {
      let #(value_type, context) =
        type_or_fresh_variable(maybe_value_type, context)
      try context = infer_type(value, value_type, context)
      let context =
        Context(
          ..context,
          environment: map.insert(context.environment, name, value_type),
        )
      infer_type(body, expected_type, context)
    }
    EVariable(name: x) ->
      case map.get(context.environment, x) {
        Ok(t) -> Ok(constrain_type(expected_type, t, context))
        Error(_) -> Error(NotInScope(x))
      }
    EInt(_) ->
      Ok(constrain_type(expected_type, TConstructor("Int", []), context))
    EString(_) ->
      Ok(constrain_type(expected_type, TConstructor("String", []), context))
    EArray(maybe_item_type: maybe_item_type, items: items) -> {
      let #(item_type, context) =
        type_or_fresh_variable(maybe_item_type, context)
      try context =
        list.try_fold(
          items,
          context,
          fn(context, item) { infer_type(item, item_type, context) },
        )
      Ok(constrain_type(
        expected_type,
        TConstructor("Array", [item_type]),
        context,
      ))
    }
  }
}

pub fn type_or_fresh_variable(
  maybe_type_annotation: Option(Type),
  context: Context,
) -> #(Type, Context) {
  case maybe_type_annotation {
    Some(type_annotation) -> #(type_annotation, context)
    None -> fresh_type_variable(context)
  }
}

/// A helper function used to assign a new index to a type variable,
/// and initialize it correctly in the substitution map in the context.
pub fn fresh_type_variable(context: Context) -> #(Type, Context) {
  let i = map.size(context.substitution)
  let t = TVariable(index: i)
  let context =
    Context(..context, substitution: map.insert(context.substitution, i, t))
  #(t, context)
}

pub fn constrain_type(t1: Type, t2: Type, context: Context) -> Context {
  Context(
    ..context,
    type_constraints: [CEquality(t1, t2), ..context.type_constraints],
  )
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
        substitution_result
        |> result.then(fn(substitution) {
          assert CEquality(t1, t2) = constraint
          unify(substitution, t1, t2)
        })
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
              unify_result
              |> result.then(fn(substitution) {
                let #(t1, t2) = t
                unify(substitution, t1, t2)
              })
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
      case list.find(generics, occurs_in(substitution, index, _)) {
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
        type_parameters: list.map(generics, substitute(substitution, _)),
      )
  }
}
