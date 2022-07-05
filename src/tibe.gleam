//// [Type Inference by Example](https://ahnfelt.medium.com/type-inference-by-example-793d83f98382)
//// but implemented in Gleam.
//// 
//// Part 7a recursive functions

import gleam/map.{Map}
import gleam/list
import gleam/string
import gleam/int
import gleam/option.{None, Option, Some}

/// AST type representing our language.
///
/// An Expression is the input of our typechecker program.
/// In `infer_type` it is processed to Types, Substutions, Environment mappings,
/// and Constraints.
pub type Expression(t) {
  /// A list of functions that may potentially call each other
  /// and are in scope in `body`.
  ERecursiveFunctions(
    functions: List(RecursiveFunction(t)),
    body: Expression(t),
  )
  /// A function is defined by a list of argument names and the expression
  /// it evaluates to (function body).
  /// A function can optionally have its return type or argument types
  /// annotated before type checking. These fields also hold inferred types
  /// after type checking.
  EFunction(
    arguments: List(FunctionArgument(t)),
    return_type: t,
    body: Expression(t),
  )
  /// An application is a function call. It is defined by a function expression
  /// which will be called, and the argument expressions being passed
  /// to this function.
  EApply(function: Expression(t), arguments: List(Expression(t)))
  /// A let expression allows to bind some value to a name, so that it can be
  /// accessed in the current scope (inside of let's body).
  /// It can have its type optionally annotated.
  /// Inferred type gets also set there after inference.
  ELet(name: String, value_type: t, value: Expression(t), body: Expression(t))
  /// An expression variable is a name that is bound to some value.
  /// This can be a function argument, a let binding, or some predefined value
  /// like the "+" abstraction.
  EVariable(name: String)
  /// Literal integer expression
  EInt(value: Int)
  /// Literal string expression
  EString(value: String)
  /// Literal array expression. Can have its item types optionally annotated
  /// (in `item_type`). This is also where inferred item type gets set.
  EArray(item_type: t, items: List(Expression(t)))
}

pub type RecursiveFunction(t) {
  RecursiveFunction(name: String, function_type: t, lambda: Expression(t))
}

pub type FunctionArgument(t) {
  FunctionArgument(name: String, argument_type: t)
}

/// The Type type represents the types of our small lanuguage.
///
/// It is used internally starting from the input (expected type)
/// of `infer_type` throughout the constraint solving (unification)
/// to substitution. At the end of our typechecker program, Expressions
/// have their type fields filled in (we go from Expression(Option(Type))
/// to Expression(Type).
pub type Type {
  /// A TConstructor is a concrete / ordinary / monomorphic type.
  /// This is what we want our types to look like after type inference.
  /// An ordinary type is defined by a type name and a list of type parameters.
  /// For example, it can be a simple type like Int with no type parameters,
  /// or a Function1 type with 2 type parameters: its argument type
  /// and return type.
  TConstructor(name: String, type_parameters: List(Type))
  /// A type variable is an internal typechecker representation
  /// for a non concrete type. It was not yet inferred or substituted
  /// to an ordinary type.
  TVariable(index: Int)
}

pub type TypeAnnotation =
  Option(Type)

/// The context holds all data useful for type checking collected along the way.
pub type Context {
  Context(
    environment: Environment,
    type_constraints: TypeConstraints,
    substitution: Substitution,
  )
}

/// The environment maps bound value names to their types
/// (either concrete types, or just type variables).
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
/// to their concrete type values. Initially for each type variable
/// it points to itself. As the type constraints get solved in unification,
/// it points to more concrete types for them.
/// At the end of type checking it's used to fill in the untyped expression
/// with types information.
pub type Substitution =
  Map(Int, Type)

pub type TypeCheckError {
  OccursError(index: Int, t: Type)
  TypeMismatch(t1: Type, t2: Type)
  NotInScope(name: String)
}

pub fn main() {
  Nil
}

pub fn infer(
  environment: Environment,
  expression: Expression(TypeAnnotation),
) -> Result(#(Expression(Type), Type), TypeCheckError) {
  let context =
    Context(
      environment: environment,
      type_constraints: [],
      substitution: map.new(),
    )
  let #(t, context) = fresh_type_variable(context)
  try #(expression, context) = infer_type(expression, t, context)
  try context = solve_constraints(context)
  Ok(#(
    substitute_expression(expression, context.substitution),
    substitute(t, context.substitution),
  ))
}

/// A function which takes an expression and its expected type
/// (to infer without checking, pass a fresh type variable as expected_type)
/// and recursively turns it into (potentially unsolved) type variables,
/// introducing new expression variables and checking that referenced expression
/// variables exist along the way.
pub fn infer_type(
  expression: Expression(TypeAnnotation),
  expected_type: Type,
  context: Context,
) -> Result(#(Expression(Type), Context), TypeCheckError) {
  case expression {
    ERecursiveFunctions(functions: functions, body: body) -> {
      let #(recursive_environment, context) =
        list.fold(
          functions,
          #(context.environment, context),
          fn(acc, function) {
            let #(recursive_environment, context) = acc
            let #(type_variable, context) =
              type_or_fresh_variable(function.function_type, context)
            #(
              map.insert(recursive_environment, function.name, type_variable),
              context,
            )
          },
        )
      try #(functions, recursive_functions_context) =
        list.try_fold(
          functions,
          #([], context),
          fn(acc, function) {
            let #(functions, context) = acc
            assert Ok(type_variable) =
              map.get(recursive_environment, function.name)
            try #(lambda, context) =
              infer_type(
                function.lambda,
                type_variable,
                Context(..context, environment: recursive_environment),
              )
            Ok(#(
              [
                RecursiveFunction(
                  name: function.name,
                  function_type: type_variable,
                  lambda: lambda,
                ),
                ..functions
              ],
              context,
            ))
          },
        )
      try recursive_functions_context =
        solve_constraints(recursive_functions_context)
      try #(body, recursive_functions_context) =
        infer_type(body, expected_type, recursive_functions_context)
      let context =
        Context(..recursive_functions_context, environment: context.environment)
      Ok(#(
        ERecursiveFunctions(functions: list.reverse(functions), body: body),
        context,
      ))
    }
    EFunction(arguments: args, return_type: return_type, body: body) -> {
      let #(return_type, context) = type_or_fresh_variable(return_type, context)
      let #(arguments_reversed, arg_types_reversed, context) =
        list.fold(
          args,
          #([], [], context),
          fn(acc, arg) {
            let #(arguments_acc, arg_types_acc, context) = acc
            let FunctionArgument(name: arg_name, argument_type: argument_type) =
              arg
            let #(t, context) = type_or_fresh_variable(argument_type, context)
            let context =
              Context(
                ..context,
                environment: map.insert(context.environment, arg_name, t),
              )
            #(
              [
                FunctionArgument(name: arg_name, argument_type: t),
                ..arguments_acc
              ],
              [t, ..arg_types_acc],
              context,
            )
          },
        )
      try #(body, context) = infer_type(body, return_type, context)
      let type_parameters = list.reverse([return_type, ..arg_types_reversed])
      let type_name =
        string.join(
          ["Function", int.to_string(list.length(arg_types_reversed))],
          with: "",
        )
      let t = TConstructor(name: type_name, type_parameters: type_parameters)
      let context = constrain_type(expected_type, t, context)
      Ok(#(
        EFunction(
          arguments: list.reverse(arguments_reversed),
          return_type: return_type,
          body: body,
        ),
        context,
      ))
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
      try #(function, context) = infer_type(function, function_type, context)
      try #(arguments_reversed, context) =
        arguments
        |> list.zip(list.reverse(argument_types_reversed))
        |> list.try_fold(
          #([], context),
          fn(acc, argument_with_type) {
            let #(arguments_acc, context) = acc
            let #(argument, argument_type) = argument_with_type
            try #(argument, context) =
              infer_type(argument, argument_type, context)
            Ok(#([argument, ..arguments_acc], context))
          },
        )
      Ok(#(
        EApply(function: function, arguments: list.reverse(arguments_reversed)),
        context,
      ))
    }
    ELet(name: name, value_type: value_type, value: value, body: body) -> {
      let #(value_type, context) = type_or_fresh_variable(value_type, context)
      try #(value, context) = infer_type(value, value_type, context)
      let context =
        Context(
          ..context,
          environment: map.insert(context.environment, name, value_type),
        )
      try #(body, context) = infer_type(body, expected_type, context)
      Ok(#(
        ELet(name: name, value_type: value_type, value: value, body: body),
        context,
      ))
    }
    EVariable(name: x) ->
      case map.get(context.environment, x) {
        Ok(t) ->
          Ok(#(EVariable(name: x), constrain_type(expected_type, t, context)))
        Error(_) -> Error(NotInScope(x))
      }
    EInt(value: v) ->
      Ok(#(
        EInt(value: v),
        constrain_type(expected_type, TConstructor("Int", []), context),
      ))
    EString(value: v) ->
      Ok(#(
        EString(value: v),
        constrain_type(expected_type, TConstructor("String", []), context),
      ))
    EArray(item_type: item_type, items: items) -> {
      let #(item_type, context) = type_or_fresh_variable(item_type, context)
      try #(items_reversed, context) =
        list.try_fold(
          items,
          #([], context),
          fn(acc, item) {
            let #(items_acc, context) = acc
            try #(item, context) = infer_type(item, item_type, context)
            Ok(#([item, ..items_acc], context))
          },
        )
      Ok(#(
        EArray(item_type: item_type, items: list.reverse(items_reversed)),
        constrain_type(
          expected_type,
          TConstructor("Array", [item_type]),
          context,
        ),
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
/// using unification, or returns unification errors.
pub fn solve_constraints(context: Context) -> Result(Context, TypeCheckError) {
  try substitution =
    context.type_constraints
    |> list.reverse()
    |> list.try_fold(
      context.substitution,
      fn(substitution, constraint) {
        assert CEquality(t1, t2) = constraint
        unify(t1, t2, substitution)
      },
    )
  Ok(Context(..context, type_constraints: [], substitution: substitution))
}

/// A function which checks that two types are (or can be) the same,
/// returns an error if they cannot, and returns an updated Substitution
/// mapping.
pub fn unify(
  t1: Type,
  t2: Type,
  substitution: Substitution,
) -> Result(Substitution, TypeCheckError) {
  case t1, t2 {
    TConstructor(name: name1, type_parameters: generics1), TConstructor(
      name: name2,
      type_parameters: generics2,
    ) ->
      case name1 != name2 || list.length(generics1) != list.length(generics2) {
        True -> Error(TypeMismatch(t1, t2))
        False ->
          list.zip(generics1, generics2)
          |> list.try_fold(
            substitution,
            fn(substitution, t) {
              let #(t1, t2) = t
              unify(t1, t2, substitution)
            },
          )
      }
    TVariable(index: i), TVariable(index: j) if i == j -> Ok(substitution)
    t1, t2 ->
      case follow(t1, substitution) {
        Ok(t) -> unify(t, t2, substitution)
        Error(Nil) ->
          case follow(t2, substitution) {
            Ok(t) -> unify(t1, t, substitution)
            Error(Nil) ->
              case t1, t2 {
                TVariable(index: i), _ ->
                  case occurs_in(i, t2, substitution) {
                    False -> Ok(map.insert(substitution, i, t2))
                    True -> Error(OccursError(i, t2))
                  }
                _, TVariable(index: i) ->
                  case occurs_in(i, t1, substitution) {
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
pub fn follow(to_follow: Type, substitution: Substitution) -> Result(Type, Nil) {
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
pub fn occurs_in(index: Int, t: Type, substitution: Substitution) -> Bool {
  case t {
    TVariable(index: i) ->
      case follow(t, substitution) {
        Ok(t) -> occurs_in(index, t, substitution)
        _ -> i == index
      }
    TConstructor(name: _, type_parameters: generics) ->
      case list.find(generics, occurs_in(index, _, substitution)) {
        Ok(_) -> True
        Error(Nil) -> False
      }
  }
}

/// A function to traverse an expression and reprace all type variables
/// with concrete types at the end of typechecking (where possible)
pub fn substitute_expression(
  expression: Expression(Type),
  substitution: Substitution,
) -> Expression(Type) {
  case expression {
    ERecursiveFunctions(functions: functions, body: body) -> {
      let functions =
        list.map(
          functions,
          fn(function) {
            let RecursiveFunction(
              name: name,
              function_type: function_type,
              lambda: lambda,
            ) = function
            let function_type = substitute(function_type, substitution)
            let lambda = substitute_expression(lambda, substitution)
            RecursiveFunction(
              name: name,
              function_type: function_type,
              lambda: lambda,
            )
          },
        )
      let body = substitute_expression(body, substitution)
      ERecursiveFunctions(functions: functions, body: body)
    }
    EFunction(arguments: arguments, return_type: return_type, body: body) -> {
      let return_type = substitute(return_type, substitution)
      let arguments =
        list.map(
          arguments,
          fn(argument) {
            FunctionArgument(
              ..argument,
              argument_type: substitute(argument.argument_type, substitution),
            )
          },
        )
      let body = substitute_expression(body, substitution)
      EFunction(arguments: arguments, return_type: return_type, body: body)
    }
    EApply(function: function, arguments: arguments) -> {
      let function = substitute_expression(function, substitution)
      let arguments =
        list.map(arguments, substitute_expression(_, substitution))
      EApply(function: function, arguments: arguments)
    }
    EVariable(_) -> expression
    ELet(name: name, value_type: value_type, value: value, body: body) -> {
      let value_type = substitute(value_type, substitution)
      let value = substitute_expression(value, substitution)
      let body = substitute_expression(body, substitution)
      ELet(name: name, value_type: value_type, value: value, body: body)
    }
    EInt(_) -> expression
    EString(_) -> expression
    EArray(item_type: item_type, items: items) -> {
      let item_type = substitute(item_type, substitution)
      let items = list.map(items, substitute_expression(_, substitution))
      EArray(item_type: item_type, items: items)
    }
  }
}

/// A function to recursively replace all type variables inside a type
/// at the end of type checking with concrete types (where possible).
pub fn substitute(t: Type, substitution: Substitution) -> Type {
  case t {
    TVariable(_) ->
      case follow(t, substitution) {
        Ok(t) -> substitute(t, substitution)
        _ -> t
      }
    TConstructor(name: name, type_parameters: generics) ->
      TConstructor(
        name: name,
        type_parameters: list.map(generics, substitute(_, substitution)),
      )
  }
}
