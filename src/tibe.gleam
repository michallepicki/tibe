//// [Type Inference by Example](https://ahnfelt.medium.com/type-inference-by-example-793d83f98382)
//// but implemented in Gleam.
//// 
//// Part 7

import gleam/map.{Map}
import gleam/list
import gleam/string
import gleam/int
import gleam/option.{None, Option, Some}
import gleam/set.{Set}

/// AST type representing our language.
///
/// An Expression is the input of our typechecker program.
/// In `infer_type` it is processed to Types, Substutions, Environment mappings,
/// and Constraints.
pub type Expression(g, t) {
  /// A list of functions that may potentially call each other
  /// and are in scope in `body`.
  ERecursiveFunctions(
    functions: List(RecursiveFunction(g, t)),
    body: Expression(g, t),
  )
  /// A function is defined by a list of argument names and the expression
  /// it evaluates to (function body).
  /// A function can optionally have its return type or argument types
  /// annotated before type checking. These fields also hold inferred types
  /// after type checking.
  EFunction(
    arguments: List(FunctionArgument(t)),
    return_type: t,
    body: Expression(g, t),
  )
  /// An application is a function call. It is defined by a function expression
  /// which will be called, and the argument expressions being passed
  /// to this function.
  EApply(function: Expression(g, t), arguments: List(Expression(g, t)))
  /// A let expression allows to bind some value to a name, so that it can be
  /// accessed in the current scope (inside of let's body).
  /// It can have its type optionally annotated.
  /// Inferred type gets also set there after inference.
  ELet(
    name: String,
    value_type: t,
    value: Expression(g, t),
    body: Expression(g, t),
  )
  /// An expression variable is a name that is bound to some value.
  /// This can be a function argument, a let binding, or some predefined value
  /// like the "+" abstraction.
  EVariable(name: String, generics: List(Type))
  /// Literal integer expression
  EInt(value: Int)
  /// Literal string expression
  EString(value: String)
  /// Literal array expression. Can have its item types optionally annotated
  /// (in `item_type`). This is also where inferred item type gets set.
  EArray(item_type: t, items: List(Expression(g, t)))
}

pub type RecursiveFunction(g, t) {
  RecursiveFunction(name: String, function_type: g, lambda: Expression(g, t))
}

pub type FunctionArgument(t) {
  FunctionArgument(name: String, argument_type: t)
}

pub type GenericType {
  GenericType(generics: List(String), uninstantiated_type: Type)
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

pub type GenericTypeAnnotation =
  Option(GenericType)

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
  Map(String, GenericType)

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
  expression: Expression(GenericTypeAnnotation, TypeAnnotation),
) -> Result(#(Expression(GenericType, Type), Type), TypeCheckError) {
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
  expression: Expression(GenericTypeAnnotation, TypeAnnotation),
  expected_type: Type,
  context: Context,
) -> Result(#(Expression(GenericType, Type), Context), TypeCheckError) {
  case expression {
    ERecursiveFunctions(functions: functions, body: body) -> {
      let #(recursive_environment, annotations_tracker, context) =
        list.fold(
          functions,
          #(context.environment, map.new(), context),
          fn(acc, function) {
            let #(recursive_environment, annotations_tracker, context) = acc
            let #(function_type, had_annotation, context) = case function.function_type {
              None -> {
                let #(t, context) = fresh_type_variable(context)
                #(
                  GenericType(generics: [], uninstantiated_type: t),
                  False,
                  context,
                )
              }
              Some(generic_function_type) -> #(
                generic_function_type,
                True,
                context,
              )
            }
            #(
              map.insert(recursive_environment, function.name, function_type),
              map.insert(annotations_tracker, function.name, had_annotation),
              context,
            )
          },
        )
      try #(ungeneralized_functions_reversed, recursive_functions_context) =
        list.try_fold(
          functions,
          #([], context),
          fn(acc, function) {
            let #(functions, context) = acc
            assert Ok(generic_function_type) =
              map.get(recursive_environment, function.name)
            try #(lambda, context) =
              infer_type(
                function.lambda,
                generic_function_type.uninstantiated_type,
                Context(..context, environment: recursive_environment),
              )
            Ok(#(
              [
                RecursiveFunction(
                  name: function.name,
                  function_type: generic_function_type,
                  lambda: lambda,
                ),
                ..functions
              ],
              context,
            ))
          },
        )
      let ungeneralized_functions =
        list.reverse(ungeneralized_functions_reversed)
      try recursive_functions_context =
        solve_constraints(recursive_functions_context)
      let new_functions =
        list.map(
          ungeneralized_functions,
          fn(function) {
            assert Ok(generic_function_type) =
              map.get(recursive_environment, function.name)
            assert Ok(had_annotation) =
              map.get(annotations_tracker, function.name)
            case had_annotation {
              True -> function
              False -> {
                let function_type = generic_function_type.uninstantiated_type
                let #(new_type_annotation, new_lambda) =
                  generalize(
                    recursive_functions_context,
                    function_type,
                    function.lambda,
                  )
                RecursiveFunction(
                  name: function.name,
                  function_type: new_type_annotation,
                  lambda: new_lambda,
                )
              }
            }
          },
        )
      let new_environment =
        list.fold(
          new_functions,
          context.environment,
          fn(acc, function) {
            map.insert(acc, function.name, function.function_type)
          },
        )
      let context =
        Context(..recursive_functions_context, environment: new_environment)
      try #(body, recursive_functions_context) =
        infer_type(body, expected_type, recursive_functions_context)
      let context =
        Context(..recursive_functions_context, environment: context.environment)
      Ok(#(ERecursiveFunctions(functions: new_functions, body: body), context))
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
                environment: map.insert(
                  context.environment,
                  arg_name,
                  GenericType(generics: [], uninstantiated_type: t),
                ),
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
          environment: map.insert(
            context.environment,
            name,
            GenericType(generics: [], uninstantiated_type: value_type),
          ),
        )
      try #(body, context) = infer_type(body, expected_type, context)
      Ok(#(
        ELet(name: name, value_type: value_type, value: value, body: body),
        context,
      ))
    }
    EVariable(name: x, generics: annotated_generics) ->
      case map.get(context.environment, x) {
        Ok(GenericType(
          generics: generics,
          uninstantiated_type: uninstantiated_type,
        )) -> {
          let #(new_generics_reversed, instantiation, context) =
            list.fold(
              generics,
              #([], map.new(), context),
              fn(acc, name) {
                let #(new_generics_acc, instantiation, context) = acc
                let #(v, context) = fresh_type_variable(context)
                let instantiation = map.insert(instantiation, name, v)
                #([v, ..new_generics_acc], instantiation, context)
              },
            )
          let new_generics = list.reverse(new_generics_reversed)
          let variable_type =
            instantiate(instantiation, uninstantiated_type, context)
          let context = case annotated_generics {
            [] -> context
            annotated_generics -> {
              assert True =
                list.length(annotated_generics) == list.length(new_generics)
              list.zip(annotated_generics, new_generics)
              |> list.fold(
                context,
                fn(context, el) {
                  let #(annotation, type_variable) = el
                  constrain_type(annotation, type_variable, context)
                },
              )
            }
          }
          let context = constrain_type(expected_type, variable_type, context)
          Ok(#(EVariable(name: x, generics: new_generics), context))
        }
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

fn instantiate(
  instantiation: Map(String, Type),
  t: Type,
  context: Context,
) -> Type {
  case map.size(instantiation) == 0 {
    True -> t
    False ->
      case t {
        TVariable(index: i) ->
          case map.get(context.substitution, i) {
            Ok(substituted_t) if substituted_t != t ->
              instantiate(instantiation, substituted_t, context)
            _ -> t
          }
        TConstructor(name: name, type_parameters: type_parameters) ->
          case map.get(instantiation, name) {
            Ok(instantiation_type) -> {
              assert True = list.length(type_parameters) == 0
              instantiation_type
            }
            _ ->
              TConstructor(
                name: name,
                type_parameters: list.map(
                  type_parameters,
                  fn(type_parameter) {
                    instantiate(instantiation, type_parameter, context)
                  },
                ),
              )
          }
        t -> t
      }
  }
}

fn generalize(
  context: Context,
  t: Type,
  expression: Expression(GenericType, Type),
) -> #(GenericType, Expression(GenericType, Type)) {
  let generic_type_variables =
    set.fold(
      free_in_environment(context),
      free_in_type(context, t),
      fn(acc, i) { set.delete(acc, i) },
    )
  let generic_type_variables =
    generic_type_variables
    |> set.to_list()
    |> list.sort(int.compare)
  let #(generic_names_reversed, local_substitution) =
    list.fold(
      generic_type_variables,
      #([], context.substitution),
      fn(acc, i) {
        let #(names, substitution) = acc
        let name = string.concat(["GenericVar", int.to_string(i)])
        #(
          [name, ..names],
          map.insert(
            substitution,
            i,
            TConstructor(name: name, type_parameters: []),
          ),
        )
      },
    )
  let new_expression = substitute_expression(expression, local_substitution)
  let new_type = substitute(t, local_substitution)
  #(
    GenericType(
      generics: list.reverse(generic_names_reversed),
      uninstantiated_type: new_type,
    ),
    new_expression,
  )
}

fn free_in_type(context: Context, t: Type) -> Set(Int) {
  case t {
    TVariable(index: i) ->
      case map.get(context.substitution, i) {
        Ok(substituted_t) if substituted_t != t ->
          free_in_type(context, substituted_t)
        _ -> set.insert(set.new(), i)
      }
    TConstructor(name: _, type_parameters: type_parameters) ->
      list.fold(
        type_parameters,
        set.new(),
        fn(acc, t) { set.union(acc, free_in_type(context, t)) },
      )
  }
}

fn free_in_environment(context: Context) -> Set(Int) {
  map.values(context.environment)
  |> list.fold(
    set.new(),
    fn(acc, generic_type) {
      set.union(acc, free_in_type(context, generic_type.uninstantiated_type))
    },
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
  expression: Expression(GenericType, Type),
  substitution: Substitution,
) -> Expression(GenericType, Type) {
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
            let function_type =
              GenericType(
                generics: function_type.generics,
                uninstantiated_type: substitute(
                  function_type.uninstantiated_type,
                  substitution,
                ),
              )
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
    EVariable(name: name, generics: generics) -> {
      let new_generics = list.map(generics, substitute(_, substitution))
      EVariable(name: name, generics: new_generics)
    }
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
