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
///
/// It is parametrized by Option(GenericType) and Option(Type)
/// (optional type annotations), and after typechecking it is parametrized by
/// GenericType and Type.
pub type Expression(g, t) {
  /// A list of functions that may potentially call each other
  /// and are in scope in `body`.
  ///
  /// In a real programming language, recursive functions could be declared with
  /// `let rec`, for example. The body is not that important, it's useful here
  /// as an example usage of those functions to demonstrate how typechecking
  /// works.
  EFunctions(functions: List(GenericFunction(g, t)), body: Expression(g, t))
  /// A lambda (local function) is defined by a list of parameter names
  /// and the expression it evaluates to (body).
  ///
  /// A lambda can optionally have its return type or parameter types
  /// annotated before type checking. These fields also hold inferred types
  /// after type checking.
  ELambda(
    parameters: List(Parameter(t)),
    return_type: t,
    body: Expression(g, t),
  )
  /// An application is a function call. It is defined by a function expression
  /// which will be called, and the argument expressions being passed
  /// to this function.
  EApply(function: Expression(g, t), arguments: List(Expression(g, t)))
  /// An expression variable is a name that is bound to some value.
  /// This can be a function argument, a let binding, or some predefined value
  /// like the "+" abstraction.
  ///
  /// Because the type of the value this name refers to can be generic,
  /// after typechecking the variable will hold a list of types used
  /// to instantiate this type.
  EVariable(name: String, generics: List(Type))
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
  /// Literal integer expression
  EInt(value: Int)
  /// Literal string expression
  EString(value: String)
  /// Literal array expression. Can have its item types optionally annotated
  /// (in `item_type`). This is also where inferred item type gets set.
  EArray(item_type: t, items: List(Expression(g, t)))
  /// By having a Semicolon expression, expressions can be joined together
  /// into a sequential list without a need for introducing bogus let bindigs.
  ESemicolon(before: Expression(g, t), after: Expression(g, t))
}

/// This is a (potentially) generic and/or recursive function
/// used in the EFunctions expression. It is defined by its name,
/// optional generic type annotation and its lambda expression definition.
/// After typechecking the function_type field will hold its inferred type.
pub type GenericFunction(g, t) {
  GenericFunction(name: String, function_type: g, lambda: Expression(g, t))
}

/// A generic type holds a list of its generic type parameter names
/// (e.g. A, B, etc) and its uninstantiated type that refers to those names.
pub type GenericType {
  GenericType(generics: List(String), uninstantiated_type: Type)
}

/// A parameter is a helper type used to define ELambda.
pub type Parameter(t) {
  Parameter(name: String, parameter_type: t)
}

/// The Type type represents the types of our small language.
///
/// It is used internally starting from the input (expected type)
/// of `infer_type` throughout the constraint solving (unification)
/// to substitution. At the end of our typechecker program, Expressions
/// have their type fields filled in
/// (we go from Expression(Option(GenericType), Option(Type))
/// to Expression(GenericType, Type).
pub type Type {
  /// A TConstructor is a concrete / ordinary / monomorphic type,
  /// or a placeholder referring to a generic type parameter with the same name.
  /// This is what we want our types to look like after type inference.
  /// It is defined by a type name and a list of type parameters.
  /// For example, it can be a simple type like Int with no type parameters,
  /// or a Function1 type with 2 type parameters: its argument type
  /// and return type.
  ///
  /// In Ahnfelt's tutorial type_parameters field is called generics.
  TConstructor(name: String, type_parameters: List(Type))
  /// A type variable is an internal typechecker representation
  /// for a non concrete type. It was not yet inferred or substituted
  /// to an ordinary type.
  TVariable(index: Int)
}

/// The context holds all data useful for type checking collected along the way.
/// In Ahnfelt's tutorial it is not needed because it uses mutation
/// and class-local variables.
pub type Context {
  Context(
    environment: Environment,
    type_constraints: TypeConstraints,
    substitution: Substitution,
  )
}

/// The environment maps bound value names to their types
/// (either concrete types, or just type variables).
/// For non-generic types, their generics list is just empty.
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

/// Initially for each type variable index, the Substitution map
/// points to itself (a TVariable with the same index).
/// As the type constraints get solved in unification,
/// it points to more concrete types for them (directly or indirectly
/// through pointing to other type variables).
/// At the end of type checking. it's used to fill in the untyped expression
/// with types information.
pub type Substitution =
  Map(Int, Type)

pub type TypeCheckError {
  OccursError(index: Int, t: Type)
  TypeMismatch(t1: Type, t2: Type)
  NotInScope(name: String)
}

/// This is the entry point of our typechecker program.
/// Given an initial environment (with some constants or functions)
/// and an untyped (optionally annotated) expression,
/// it returns a typed expression and its value type,
/// or a typechecking error.
pub fn infer(
  environment: Environment,
  expression: Expression(Option(GenericType), Option(Type)),
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

/// A function which takes an expression and its expected annotated type
/// and recursively turns it into (potentially unsolved) type variables.
/// To infer without checking, pass a fresh type variable as expected_type.
/// Along the way it is introducing new functions or let bindings
/// into the Environment, making sure that they don't escape the scope
/// in which they are declared, and checking that referenced name bindings
/// exist in the Environment.
pub fn infer_type(
  expression: Expression(Option(GenericType), Option(Type)),
  expected_type: Type,
  context: Context,
) -> Result(#(Expression(GenericType, Type), Context), TypeCheckError) {
  case expression {
    EFunctions(functions: functions, body: body) -> {
      // For a group of (potentially) recursive functions,
      // when checking one of them, a different one (or itself) can be called
      // in its lambda body. So, to have them available when checking,
      // we first go through all functions and add their name with type
      // to the environment. The type is either an annotation (when present),
      // or a fresh type variable.
      // We also keep track of which functions had a type annotation.
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
      // For each function, we try to infer the type of its definition,
      // and accumulate the "typed" (likely still with type variables)
      // functions into a list. When inference or checking fails
      // in this process, we return the error.
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
                GenericFunction(
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
      // We want to find the functions which don't have a type annotation
      // but are generic. To do that, we first try to solve type constraints
      // to confirm that the functions can typecheck at all. If they can't,
      // we return an error here.
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
                // If a function didn't have a type annotation, it could still
                // be generic, so we attempt to generalize it.
                let function_type = generic_function_type.uninstantiated_type
                let #(new_type_annotation, new_lambda) =
                  generalize(
                    context.environment,
                    recursive_functions_context.substitution,
                    function_type,
                    function.lambda,
                  )
                GenericFunction(
                  name: function.name,
                  function_type: new_type_annotation,
                  lambda: new_lambda,
                )
              }
            }
          },
        )
      // Because some function types could have been generalized,
      // we prepare a new environment to check the body expression
      // (similar to recursive_functions_context but with generic functions
      // having their lists of generics in generic type filled in).
      let new_environment =
        list.fold(
          new_functions,
          context.environment,
          fn(acc, function) {
            map.insert(acc, function.name, function.function_type)
          },
        )
      let new_context =
        Context(..recursive_functions_context, environment: new_environment)
      // We try to infer / check the body expression (or return an error).
      try #(body, body_context) = infer_type(body, expected_type, new_context)
      // We returned the typed EFunctions expression.
      // Because in our case the recursive functions were only in scope
      // for the body, and whole EFunctions expression evaluates to body,
      // we reset the environment to not have them.
      Ok(#(
        EFunctions(functions: new_functions, body: body),
        Context(..body_context, environment: context.environment),
      ))
    }
    ELambda(parameters: parameters, return_type: return_type, body: body) -> {
      let #(return_type, context) = type_or_fresh_variable(return_type, context)
      // We add lambda parameters to the environment for checking the body.
      // We also prepare a list of their types for convenience.
      let #(parameters_reversed, parameter_types_reversed, body_context) =
        list.fold(
          parameters,
          #([], [], context),
          fn(acc, parameter) {
            let #(parameters_acc, parameter_types_acc, context) = acc
            let Parameter(name: parameter_name, parameter_type: parameter_type) =
              parameter
            let #(t, context) = type_or_fresh_variable(parameter_type, context)
            let context =
              Context(
                ..context,
                environment: map.insert(
                  context.environment,
                  parameter_name,
                  GenericType(generics: [], uninstantiated_type: t),
                ),
              )
            #(
              [
                Parameter(name: parameter_name, parameter_type: t),
                ..parameters_acc
              ],
              [t, ..parameter_types_acc],
              context,
            )
          },
        )
      // We try to infer / check the type of lambda body, or return an error
      // if it fails.
      try #(body, body_context) = infer_type(body, return_type, body_context)
      // We constrain the the expected type (which comes either from
      // user's function annotation or EApply function call) with inferred
      // function type. To do that, we first glue together parameter and return
      // types into the expected format.
      let type_parameters =
        list.reverse([return_type, ..parameter_types_reversed])
      let type_name =
        string.join(
          ["Function", int.to_string(list.length(parameter_types_reversed))],
          with: "",
        )
      let t = TConstructor(name: type_name, type_parameters: type_parameters)
      let context =
        constrain_type(
          expected_type,
          t,
          Context(..body_context, environment: context.environment),
        )
      // We return the typed lambda expression.
      Ok(#(
        ELambda(
          parameters: list.reverse(parameters_reversed),
          return_type: return_type,
          body: body,
        ),
        context,
      ))
    }
    EApply(function: function, arguments: arguments) -> {
      // We prepare the expected function type using type variables
      // for each passed argument and expected_type (type of the whole EApply
      // expression) as the function call return type.
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
      // We infer / check the type of the called function
      // (which could be just a function name, but could be an in-line lambda
      // or another function call that returns a lambda), and make sure to pass
      // the type of expected function as expected_type. We return an error
      // if it fails.
      try #(function, context) = infer_type(function, function_type, context)
      // For each function argument, we try to infer its type
      // (or return an error). I think alternatively we could do that earlier
      // before inferring the type of the called function? The order may affect
      // which error would get returned in case of mismatched function and/or
      // parameter/argument types.
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
      // We return the typed function call expression.
      Ok(#(
        EApply(function: function, arguments: list.reverse(arguments_reversed)),
        context,
      ))
    }
    ELet(name: name, value_type: value_type, value: value, body: body) -> {
      let #(value_type, context) = type_or_fresh_variable(value_type, context)
      // We infer / check the type of binding value (or return an error),
      // and add it to the environment for inferring / checking the body.
      try #(value, context) = infer_type(value, value_type, context)
      let body_context =
        Context(
          ..context,
          environment: map.insert(
            context.environment,
            name,
            GenericType(generics: [], uninstantiated_type: value_type),
          ),
        )
      // We try to infer the type of the let body (or return an error).
      try #(body, body_context) = infer_type(body, expected_type, body_context)
      // We return the typed expression (without the name present in the
      // environment, since its scope is just the let body).
      Ok(#(
        ELet(name: name, value_type: value_type, value: value, body: body),
        Context(..body_context, environment: context.environment),
      ))
    }
    EVariable(name: x, generics: annotated_generics) ->
      // We look up the variable name in the environment (or return an error).
      case map.get(context.environment, x) {
        Error(_) -> Error(NotInScope(x))
        Ok(GenericType(
          generics: generics,
          uninstantiated_type: uninstantiated_type,
        )) -> {
          // Since the variable name can refer to a value of generic type,
          // and to type check the usage of this variable
          // we will need a concrete type, we prepare a map from each generic
          // type placeholder (like A, B etc) to a type variable.
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
          // We call a helper function to replace the generics in the type
          // fetched from the environment (type placeholders like A, B etc)
          // with previously prepared type variables. The result is the type
          // of our variable.
          let variable_type =
            instantiate(instantiation, uninstantiated_type, context)
          let context = case annotated_generics {
            [] -> context
            annotated_generics -> {
              // If the user supplied annotations for instantiating generics
              // on the variable usage, we make sure they provided the correct
              // number of generic type annotations.
              // TODO: this should be another possible error to be returned.
              assert True =
                list.length(annotated_generics) == list.length(new_generics)
              // We constrain our type variables (replacements for generic
              // type placeholders) with the types the user provided.
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
          // We constrain the instantiated type of our variable
          // with the type this variable usage is expected to have.
          let context = constrain_type(expected_type, variable_type, context)
          // We return the variable expression with type variables
          // in place of generics.
          Ok(#(EVariable(name: x, generics: new_generics), context))
        }
      }
    EInt(value: v) ->
      // For Integer literal expressions all we need to do is constrain
      // the expected type with the Int type.
      Ok(#(
        EInt(value: v),
        constrain_type(expected_type, TConstructor("Int", []), context),
      ))
    EString(value: v) ->
      // Same as EInt
      Ok(#(
        EString(value: v),
        constrain_type(expected_type, TConstructor("String", []), context),
      ))
    EArray(item_type: item_type, items: items) -> {
      // Each item in an array will need to have the same type,
      // so if the type is not annotated we create a new variable
      // for checking that.
      let #(item_type, context) = type_or_fresh_variable(item_type, context)
      // For each item in the array we try to infer its type passing the
      // expected item type (or return an error).
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
      // We return the typed array expression.
      Ok(#(
        EArray(item_type: item_type, items: list.reverse(items_reversed)),
        constrain_type(
          expected_type,
          TConstructor("Array", [item_type]),
          context,
        ),
      ))
    }
    ESemicolon(before: before, after: after) -> {
      let #(before_type, context) = fresh_type_variable(context)
      // We only care about the type of the last expression
      // in an expressions sequence, so for checking the "before" expression
      // we pass in a fresh type variable as expected type.
      try #(new_before, context) = infer_type(before, before_type, context)
      try #(new_after, context) = infer_type(after, expected_type, context)
      // We return the typed expressions sequence.
      Ok(#(ESemicolon(before: new_before, after: new_after), context))
    }
  }
}

// A helper function used to get a type annotation if the user supplied it,
// or generate a fresh type variable if not.
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

// A helper function used to add an equality constraint between two types
// to the context.
pub fn constrain_type(t1: Type, t2: Type, context: Context) -> Context {
  Context(
    ..context,
    type_constraints: [CEquality(t1, t2), ..context.type_constraints],
  )
}

// A helper function used to replace the generic type placeholders
// (like A, B etc) with some type according to the instantiation map.
// It recurses to do that everywhere in the type.
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

// A helper function used to replace some type variables inside a type
// with generic type placeholders (like A, B, C etc) where possible.
fn generalize(
  environment: Environment,
  substitution: Substitution,
  t: Type,
  expression: Expression(GenericType, Type),
) -> #(GenericType, Expression(GenericType, Type)) {
  let generic_type_variables =
    set.fold(
      free_in_environment(environment, substitution),
      free_in_type(environment, substitution, t),
      fn(acc, i) { set.delete(acc, i) },
    )
  let generic_type_variables =
    generic_type_variables
    |> set.to_list()
    |> list.sort(int.compare)
  let #(generic_names_reversed, local_substitution) =
    list.fold(
      generic_type_variables,
      #([], substitution),
      fn(acc, i) {
        let #(names, substitution) = acc
        // TODO: this should generate names like A, B, C etc
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

fn free_in_type(
  environment: Environment,
  substitution: Substitution,
  t: Type,
) -> Set(Int) {
  case t {
    TVariable(index: i) ->
      case map.get(substitution, i) {
        Ok(substituted_t) if substituted_t != t ->
          free_in_type(environment, substitution, substituted_t)
        _ -> set.insert(set.new(), i)
      }
    TConstructor(name: _, type_parameters: type_parameters) ->
      list.fold(
        type_parameters,
        set.new(),
        fn(acc, t) {
          set.union(acc, free_in_type(environment, substitution, t))
        },
      )
  }
}

fn free_in_environment(
  environment: Environment,
  substitution: Substitution,
) -> Set(Int) {
  map.values(environment)
  |> list.fold(
    set.new(),
    fn(acc, generic_type) {
      set.union(
        acc,
        free_in_type(
          environment,
          substitution,
          generic_type.uninstantiated_type,
        ),
      )
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
    EFunctions(functions: functions, body: body) -> {
      let functions =
        list.map(
          functions,
          fn(function) {
            let GenericFunction(
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
            GenericFunction(
              name: name,
              function_type: function_type,
              lambda: lambda,
            )
          },
        )
      let body = substitute_expression(body, substitution)
      EFunctions(functions: functions, body: body)
    }
    ELambda(parameters: parameters, return_type: return_type, body: body) -> {
      let return_type = substitute(return_type, substitution)
      let parameters =
        list.map(
          parameters,
          fn(parameter) {
            Parameter(
              ..parameter,
              parameter_type: substitute(parameter.parameter_type, substitution),
            )
          },
        )
      let body = substitute_expression(body, substitution)
      ELambda(parameters: parameters, return_type: return_type, body: body)
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
    ESemicolon(before: before, after: after) -> {
      let before = substitute_expression(before, substitution)
      let after = substitute_expression(after, substitution)
      ESemicolon(before: before, after: after)
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

pub fn main() {
  Nil
}
