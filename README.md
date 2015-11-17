# Introduction

Certain class-based programming languages provide concept of generic types. This is ability to parametrize classes with type parameters. Such a feature enables writing polymorphic code which can work for arbitrary actual type arguments. One of the key applications of generic types we can found in standard libraries, e.g. collections and algorithms working with them. For example, sorting algorithm can be implemented as a function taking some collection of comparable elements and invoked for lists, vectors or arrays.

There are several possible implementations of generics, including:

 * *type passing* - it preserves information about type parameters at runtime, which allows to distinguish for example `List<Integer>` from `List<String>`; this implementation is chosen in *.NET* languages like *C#*
 * *type instantiating* - for every instantiation of parametrized class with actual type arguments, there is separate class generated, which maintains no information about generic types - for example `List_Integer` and `List_String`; still we can distinguish between them; this implementation is present in *C++* language
 * *type erasure* - it eliminates informations about type parameters at compilation time, replacing them with their so-called *type bounds*; at runtime we have only single `List` class which can hold any elements; we cannot distinguish lists of integers from lists of string in this implementation; type erasure is used by *Java* language

In this article we will review implementation of two small programming languages that imitate subsets of Java, being syntactically compatible with full language. They were defined in whitepaper by A. Igarashi, B. C. Pierce and P. Wadler titled [*Featherweight Java: A Minimal Core Calculus for Java and GJ*](http://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf). There two languages are:

* *FJ* - minimal subset of Java with classes only
* *FGJ* - the language extended with type-parametrized classes and methods

We will define syntax, look at the examples and express type erasure as translation from *FGJ* to *FJ* that preserves some important properties about types and behaviour. We will not explain all the details of erasure implementation, but instead will look at the example programs and their erased version to see type erasure in action. For those readers who are willing to deeper understand erasure rules, I encourage to read original paper, where they are clearly defined and comprehensively explained.

The implementation is written in Scala 2.11. The only software that you should have installed in order run examples are [Java JDK](http://www.oracle.com/technetwork/java/javase/downloads/index.html) and [SBT](http://www.scala-sbt.org).

# Featherweight Java

## Idea

Looking for a tool to precise describing Java type system we need to focus on modelling only those parts of the language which are important from the type system perspective while ommiting those, which are not. Trying to model full Java in such a way will result enormous calculus which would be hard to grasp. Therefore *Featherweight Java* favours compactness over completeness providing only few combinators, while still being legal subset of Java, only little larger than original [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

To achieve simplicity, language is heavily reduced. It means:

* no concurrency primitives
* no reflection
* no interfaces
* no method overloading
* no inner classes
* no static members
* no member access control - all methods and fields are public
* no primitive types
* no null pointers
* no assignments/setters

Instead, we focus only on minimal language subset, including:

* mutually recursive class definitions
* object creation
* field access
* method invocation, overriding and recursion through `this`
* subtyping
* casting

## Syntax

Let's start with a simple example.

```java
class A extends Object {
  A() { super(); }
}
class B extends Object {
  B() { super(); }
}
class Pair extends Object {
  Object fst;
  Object snd;
  Pair(Object fst, Object snd) {
    super(); this.fst = fst; this.snd = snd;
  }
  Pair setfst(Object newfst) {
    return new Pair(newfst, this.snd);
  }
}
```

*FJ* is class-based language where we can define classes like in Java, but satisfying some constraints:

* we always write super class name, even if it's trivial (*Object*)
* we always write receiver of field or method, even if it's trivial (`this`)
* `this` is simply variable rather than a keyword, unlike to full Java
* we always write constructor which initialize all fields defined in that class and call `super` which refers to super class constructor, which initializes its fields, etc...
* constructors are only place where `super` or `=` appears

### Programs

In *FJ*, programs consist of class table and expression to be evaluated, which correspond to static `main` method in executable Java classes. We intuitively expect that such an expression:

```java
new Pair(new A(), new B()).setfst(new B())
```
... will evaluate to:
```java
new Pair(new B(), new B())
```

### Expressions

In FJ we have 5 types of expressions, which can appear in methods body:

* *variable access* - `newfst` or reference to `this`
* *object construction* - `new A()`, `new B()` or `new Pair(newfst, this.snd)`
* *field access* - in `this.snd` expression we accessing field named `snd` on the object reffered by variable `this`
* *method invocation* - `e3.setfst(e4)` this is example of invocation of method `setfst` on object `e3` with single argument `e4`
* *casts* - `(A)(new Pair(new A(), new B()).fst)` is example of type cast used to recover type information about `fst` field


## Extending with generic types

Let's back to our example implementation of `Pair` class and add generic type parameters `X` and `Y` as `first` and `second` field types.

```java
class Pair<X extends Object, Y extends Object> extends Object {
  X fst;
  Y snd;
  Pair(X fst, Y snd) {
    super(); this.fst = fst; this.snd = snd;
  }
  <Z extends Object> Pair<Z, Y> setfst(Z newfst) {
    return new Pair<Z, Y>(newfst, this.snd);
  }
}
```

The syntax is extended with:

* type parameters lists for classes and methods - in the example above `X` and `Y` are type parameters for class `Pair`, while `Z` is type paramether of method `setfst`
* every type parameter has to be bounded by some actual class type, possibly parametrized with type variables (e.g. `X extends C<X>`)
* in contrast to Java we always write the bound even if it is *Object*
* object construction and method invocation both take type arguments list like `new Pair<Z, Y>(...)` ot `.setfst<B>(...)`, but empty parameter lists (`<>`) can be omitted

Our refined program looks as follows.

```java
new Pair<A,B>(new A(), new B()).setfst<B>(new B())
```

And it evaluates to expression:

```java
new Pair<B,B>(new B(), new B())
```

## Type erasure as translation from FGJ to FJ

We can express type erasure as compilation from FGJ syntax to FJ by replacing all type variables with their bounds and inserting some number of casts, when needed to smartly recover type information from original *FGJ* code.

The example class `Pair<X, Y>` after erasure looks exactly like previous `Pair` class without generic types.

Similarly, following expression:
```java
new Pair<A,B>(new A(), new B()).snd
```
erases to:
```java
(B)new Pair(new A(), new B()).snd
```
Notice that the cast to `B` was inserted to restore type of `snd` field which is annotated with type `Object` in erased `Pair`.

# Implementation review

Having gradual introduction to *FJ*/*FGJ* behind, let's get sight of the Scala implementation of these small languages.

## FJ module

*FJ*-related code is contained in `src/main/scala/fj` directory. There are syntax for *FJ* programs defined in `AST.scala`, type checker in `Types.scala` and evaluator in `Eval.scala`.

### Syntax

Classes, fields and methods are represented by following set of case classes.

```scala
type VarName = String
type TypeName = String

case class Class(name: TypeName,
                 baseClass: TypeName,
                 fields: List[Field],
                 methods: List[Method])

case class Field(name: VarName,
                 fieldType: TypeName)

case class Method(name: VarName,
                  resultType: TypeName,
                  args: List[Argument],
                  body: Expr)

case class Argument(name: VarName,
                    argType: TypeName)
```

We represent *FJ* classes using 4 nested data structures which hold all necessary information about base classe, fields and methods. There are type aliases defined for type and variable names, internally represented as strings. Similarly, we encode expressions using following case classes:

```scala
trait Expr

case class Var(name: VarName) extends Expr

case class FieldAccess(expr: Expr,
                       fieldName: VarName) extends Expr

case class Invoke(expr: Expr,
                  methodName: VarName,
                  args: List[Expr]) extends Expr

case class New(className: TypeName,
               args: List[Expr]) extends Expr

case class Cast(className: TypeName,
                expr: Expr) extends Expr
```

In actual implementation all those classes have overriden method `toString` which prettifies syntax of our programs when printing to the console.

#### Example encoded in Scala

Let's review how we can encode our first example with `Pair` class implementation.

```scala
val A = Class("A", "Object")
val B = Class("B", "Object")
val Pair = Class(
  name = "Pair",
  baseClassName = "Object",
  fields = List(
    Field("fst", "Object"),
    Field("snd", "Object")
  ),
  methods = List(
    Method(
      name = "setfst",
      resultType = "Pair",
      args = List(Argument("newfst", "Object")),
      body = New("Pair", List(
	    Var("newfst"), FieldAccess(Var("this"), "snd")
	  ))
    )
  )
)
```

It's just straightforward rewriting our `Pair` class with two fields and one method.

We represent class tables and programs as follows.

```scala
type ClassTable = Map[TypeName, Class]
case class Program(classTable: ClassTable, main: Expr)

val classTable: ClassTable = buildClassTable(List(A, B, Pair))
val main: Expr = Invoke(
  New("Pair", List(New("A"), New("B"))),
  "setfst",
  List(New("B"))
)
val program = Program(classTable, main)
```

We have helper function `buildClassTable` which takes list of classes and returns class table built out of them. `Program` is just, following definition, paired class table with main expression to be evaluated.

### Type checker

There are type-checking rules provided in *FJ* paper, which are implemented in `fj.Types`.

Subtyping in *FJ* is reflexive and transitive closure of inheritance relation between classes. It can be decided only by looking at class table. Implementation of subtyping is given as recursive function at `fj.Types.isSubtype`.

Main type-checking function is `fj.Types.exprType` which find concrete type of expression in given typing context *Γ* or indicates that expression is incorrectly-typed. Context *Γ* contains information about actual types of available variables and is represented as `Map[VarName, SimpleType]`. There is also auxilliary function `fj.Types.progType` which type-checks a whole program, ensuring that all classes, fields, methods are well-typed according to the typing rules, and returns type of main expression.


### CBV Evaluator

In original *Featherweight Java* paper there was given reduction rules for expressions in the form of so-called *operational semantics*, which doesn't precise the order of evaluation. When you try to implement expression evaluator you have to choose some evaluation strategy. In this repository we provided implementation with [call by value](https://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_value) semantics, which corresponds to that from full Java, where method's arguments are evaluated from left to right.

From the *FJ* calculus point of view when some reduction error occurs (like trying to create object of unknown class or trying to invoke non-existing method), such configuration is called *stuck* and the evaluation cannot be continued. In this implementation we don't bother too much about error handling in the interpreter. When some error configuration is detected, we throw `RuntimeException` with appropriate error message, forgetting about result we computed so far.

The evaluator is rather simple adaptation of reduction rules. You can find it at `fj.Eval.evalExpr` for evaluation expressions in given context (class table) and auxilliary `fj.Eval.evalProg` which takes program, builds class table and evaluates its main expression.

### Running examples

```scala
println(program.main)
// it should print:
// new Pair(new A(), new B()).setfst(new B())

val programT = programType(program)
// it should compute Some(Pair), which is the type
// of main expression

val result = evalProg(program)
// should compute to
// New("Pair", List(New("B"), New("B")))
println(result)
// and should print:
// new Pair(new B(), new B())
```

You can run similar example by typing:

`sbt "runMain fj.examples.Pairs"`

Let's encode some more interesing program in our language. In *FJ* we don't have primitive types, especially numbers. But there is a way to encode natural numbers using just classes and objects, similarly to [Church numerals](https://en.wikipedia.org/wiki/Church_encoding#Church_numerals) in λ-calculus, but instead of folding functions, we will fold instances of class *Succ* n times over the instance of class *Zero* to represent number n.

```
class Nat extends Object {
  Nat() { super(); }
  Nat succ() { return new Succ(this); }
  Nat plus(Nat n) { return n; }
}

class Zero extends Nat { 
  Zero() { super(); }
}

class Succ extends Nat {
  Nat prev;
  Succ(Nat prev) { super(); this.prev = prev; }
  Nat plus(Nat n) { return this.prev.plus(n.succ()); }
}
```

We represent 0 as `new Zero()`, 1 as `new Succ(new Zero())`, 2 as `new Succ(new Succ(new Zero()))`, etc.

Addition is implemented as recursive function with base case at 0 (indeed, `0 + n = n`). Recursive step is in the class `Succ` and it reduces to transforming general addition `m + n` into `(m-1) + (n+1)` until we reach base case for `m = 0`. Method `succ` is implemented in class `Nat` as wrapping the actual number `this` into object of `Succ` class.

There is one subtelty connected with implementation of method `plus`. Base case of recursion has to be implemented in class `Nat` to satisfy type-checking of `Succ` class. In full Java we would probably defined this method as abstract in `Nat` class and provide two actual implementations in `Zero` and `Succ`. But in  *FJ* we don't have abstract methods and without method `plus` declared in `Nat` we could not proper type recursive invocation `this.prev.plus(...)` in `Succ` class.

You can find this example encoded at `fj.examples.Numbers` and run it by typing:

`sbt "runMain fj.examples.Numbers"`

## FGJ module

*FGJ*-related code is contained in `src/main/scala/fgj` directory. There are syntax for *FGJ* programs defined in `AST.scala` and type checker in `Types.scala`.

### Syntax

#### Types

As types are now part of our classes, methods and expressions AST, let's review them first.

```scala
type TypeVarName = String

trait Type

case class TypeVar(name: TypeVarName) extends Type

case class ClassType(className: TypeName,
                     argTypes: List[Type]) extends Type
```

We have new type alias *TypeVarName* for type variables (again, internally just strings). We have 2 form of types now: *type variables* and *class types* parametrized by some number of types (which again can be type variables or class types).

#### Classes

```scala
case class BoundedParam(typeVar: TypeVar,
                        boundClass: ClassType)

case class Class(name: TypeName,
                 typeParams: List[BoundedParam],
                 baseClass: ClassType,
                 fields: List[Field],
                 methods: List[Method])

case class Field(name: VarName, fieldType: Type)

case class Method(name: VarName,
                  typeParams: List[BoundedParam],
                  resultType: Type,
                  args: List[Argument],
                  body: Expr)
                  
case class Argument(name: VarName, argType: Type)
```

`BoundedParam` corresponds to single `Z extends Object` from our example. It is definition of type variable, bounded by some class type. Notice we can write recursive type expression in bounds (like `X extends C<X>`) thanks to that definition of class types, which are parametrized with arbitrary types.

Classes are parametrized by a list of *bounded parameters*. Notice change in `baseClass` signature which now is not only name reference, but is class type which can be parametrized with type variables, like in example below.

```java
class List<X extends Object> extends Collection<X> { ... }
```

Methods also can be parametrized with type variables. We can use them to encode method's return type, argument types.

`ClassTable` and `Program` definitions are straightforwardly adjusted to use refined types.

#### Expressions

AST for expressions is mostly unchanged. As before, we have 5 forms of them.

```scala
trait Expr

case class Var(name: VarName) extends Expr

case class FieldAccess(expr: Expr,
                       fieldName: VarName) extends Expr

case class Invoke(expr: Expr,
                  methodName: VarName,
                  typeArgs: List[Type],
                  args: List[Expr]) extends Expr

case class New(classType: ClassType,
               args: List[Expr]) extends Expr

case class Cast(classType: ClassType,
                expr: Expr) extends Expr
```

The only difference beside type adjustments is in *Invoke* expression which now takes also list of type parameters to be instantiated.

### Type checker

In *FGJ* typechecking rules are bit more complicated. First of all, subtyping is not relation between class names any more, but is generalized for all type forms, including type variables. Therefore we differentiate two separate relations:

* *subclassing* - it corresponds to *FJ*'s subtyping, can be decided only using class table
* *subtyping* - generalized relation between all types, can be decided using additional environment *Δ* which maps type variables to their bounds, where bounds are just class types with actual type arguments given

#### Covariant method overriding

Unlike to *FJ*, where we allowed method overriding only with corresponding (i.e. identical) signatures, in *FGJ* covariant method overriding on the method's result type is allowed. Result type of a method may be a subtype of the result type of the corresponding method in the superclass, although the bounds of type variables and the argument types must be identical (modulo renaming of type variables).

Function for typing expressions is located at `fgj.Types.exprType`, now it takes expression, class table and two contexts *Γ* and *Δ*. Again, we have auxilliary `fgj.Types.programType` which checks also well-typedness of classes and methods.

### Excercise: type-passing evaluator

We will not provide CBV evaluator for type-passing semantics, leaving it as an excercise for the reader to take *FJ* evaluator code and adjust it to support evaluating programs in syntax with generic types.

> **Hint:** as well as we maintain environment for variables and its values, you may need to maintain additional environment mapping type variables to actual class types.

## Type erasure

Erasure-related code is contained in `src/main/scala/erasure` directory.

### Implementation review

The technical idea of type erasure is to translate *FGJ* programs into *FJ* ones. To perform that task, we have to define erasure for all parts of our programs. Wanting to adopt erasure rules from *FJ* paper, several functions are defined, to translate:

* FGJ types to FJ types - `erasure.Erasure.eraseType`
* FGJ expressions to FJ expression - `erasure.Erasure.eraseExpr`
* FGJ classes to FJ classes - `erasure.Erasure.eraseClass`
* and finally we have auxilliary function which merge results and translate a whole FGJ program to FJ program - `erasure.Erasure.eraseProgram`

### Examples

Instead of exploring implementation details, let's catch some more interesting *FGJ* programs and their erased versions to see the rules in action.

#### Example #1 - natural numbers revisited

This is extended version of natural numbers implementation in *FGJ*.

```java
class Summable<X extends Object> extends Object {
  X plus(X other) { return other; }
}

class Nat extends Summable<Nat> {
  Nat() { super(); }
  Succ succ() { return new Succ(this); }
}

class Zero extends Nat { 
  Zero() { super(); }
}

class Succ extends Nat {
  Nat prev;
  Succ(Nat prev) { super(); this.prev = prev; }
  Nat plus(Nat n) {
    return this.prev.plus(n.succ());
  }
}
```

We introduced class `Summable<X>` which have one method `plus`. In Java we would probably make this class an interface, but in *FGJ* we don't have interfaces, so we have to provide default implementation returning some value of type `X`. Fortunately we have parameter of type `X`, so we use it as a return value. It turns out that it is still valid implementation of `plus` for class `Zero`, so we don't have to re-implement it there. We made our `Nat` class a subclass of `Summable<Nat>`. For class `Succ` implementation of `plus` is the same as before. Spot another slight difference in return type of `succ` method in class `Nat` - now it is declared to be `Succ`; we will need that to demonstrate erasure of covariant method overriding in result type in one of the following examples.

Let's use function `erasure.Erasure.eraseClass` to generate erasure for these classes.

```java
class Summable extends Object {
  Summable() { super(); }
  Object plus(Object other`) { return other`; }
}

class Nat extends Summable {
  Nat() { super(); }
  Succ succ() { return new Succ(this); }
}

class Zero extends Nat { 
  Zero() { super(); }
}

class Succ extends Nat {
  Nat prev;
  Succ() { super(); }
  Object plus(Object n`) {
    return (Nat)(this.prev.plus((Nat)(n`).succ())); 
  }
}
```

What did the erasure change here?

* in class `Summable` type parameters list was removed and all type variables were replaced with `Object` - which was declared bound for `X` variable (see `X extends Object` in original class
* class `Nat` now extends our erased `Summable` class
* according to `plus` method signature change in `Summable`, signature of `plus` in `Succ` class were adjusted to be identical (modulo argument names); to recover information about types, two casts to `Nat` were inserted: first over the access to `n'` variable, second over the invocation of method `plus` which happened to return natural number in generic version

Now, let's construct simple expression using these classes. This will correspond to arithmetic operation `2 + 1`

```java
new Succ(new Succ(new Zero())).plus(new Succ(new Zero()))
```

After erasure it is almost the same.

```java
(Nat)(new Succ(new Succ(new Zero())).plus(new Succ(new Zero())))
```

Now, our `plus` method returns an `Object`, but type erasure was smart enough to insert upcast around invocation of this method, to recover correct type from original program.

#### Example #2 - summable lists

Let's review another example - lists which can contain some summable elements and are able to compute total `sum` of all their elements.

```java
class List<X extends Summable<X>> extends Object {
  List() { super(); }
  X sum(X zero) { return zero; }
}

class Nil<X extends Summable<X>> extends List<X> {
  Nil() { super(); }
}

class Cons<X extends Summable<X>> extends List<X> {
  X head;
  List<X> tail;
  Cons(X head, List<X> tail) {
    super(); this.head = head; this.tail = tail;
  }
  X sum(X zero) {
    return this.tail.sum(zero).plus(this.head);
  }
}
```

We have base `List<X>` class and its two subclasses:

* `Nil` - corresponding to empty list
* `Cons` - list constructor which holds single element `head` of type `X` and rest of list - `tail` of type `List<X>`

For example list `[1, 0]` can be encoded as following expression:

```java
new Cons<Nat>(new Succ(new Zero()), new Cons<Nat>(new Zero(), new Nil<Nat>()))
```

Method `sum` takes parameter `zero` which will be summed with all elements of our list. Overriden occurence uses recursive call first to compute sum of `tail` (it will return `X`) and invoke method `plus` adding `head` element to the sum. Notice that in this example class there is no any occurrence of classes `Nat`, `Zero` or `Succ` - we were able to express `sum` operation on list using only abstract `plus` which we defined for summables.

> You can consider to make a `List` class subtype of `Summable`.
> 1. What is the meaning of `plus` regarding to lists?
> 2. How exactly would base class signature would look like?

Let's review erasure of lists implementation.

```java
class List extends Object {
  List() { super(); }
  Summable sum(Summable zero`) { return zero`; }
}

class Nil extends List {
  Nil() { super(); }
}

class Cons extends List {
  Summable head;
  List tail;
  Cons(Summable head, List tail) {
    super(); this.head = head; this.tail = tail;
  }
  Summable sum(Summable zero`) {
    return (Summable)(this.tail.sum(zero`).plus(this.head));
  }
}
```

Again, all type parameters were removed and occurrences of all type variables were replaced with `Summable`. Erasure is optimized in that way that it doesn't insert casts, if they are not necessary - see implementations of `sum` method and references to `zero'` argument which are not casted. The only cast we need to insert is around invocation of `plus` method from `Summable`, which still returns `Object`.

Having the context of `Nat` and `List` classes, let's consider such expression:

```java
new Cons<Nat>(
  new Succ(new Succ(new Succ(new Zero()))),
  new Cons<Nat>(
    new Succ(new Succ(new Zero())),
    new Nil<Nat>()
  )
).sum(new Zero())
```
...and its erased version:

```java
(Nat) new Cons(
  new Succ(new Succ(new Succ(new Zero()))),
  new Cons(
    new Succ(new Succ(new Zero())),
    new Nil()
  )
).sum(new Zero())
```

You recognize the trick? We constructed list of 2 natural numbers (3 and 2) by instantiating `Cons`s with type argument `Nat`, which were removed during erasure. Method `sum` returns `Summable`, but cast to `Nat` were inserted to ensure that both expressions have the same types in corresponding type checkers (both types to `Nat`).

We can now evaluate erased expression using *FJ* evaluator:

```java
new Succ(new Succ(new Succ(new Succ(new Succ(new Zero())))))
```

As a result, we got encoding of number 5 which is sum of list elements (3 and 2) with explicit 0 passed to `sum`.

#### Example #3 - functions

So far we have seen rather simple examples. Now let's try to encode something more advanced.

We want to encode interface for unary functions which takes single argument of type `X` and returns value of type `Y`.

```java
class UnaryFunc<X extends Object, Y extends Object> extends Object {
  Y ignored;
  UnaryFunc(Y ignored) { super(); this.ignored = ignored; }
  Y apply(X arg) {
    return this.ignored;
  }
}
```

We want to represent simple functions as instances of `UnaryFunc` class with single method `apply` for computing function value for given argument. Again, due to lack of interfaces, we have to provide trivial implementation for `apply`. Since we don't require `Y` as an argument for method, the trick is to create member of the same type as function's result type and return it in our trivial implementation.

Let's encode simple function for natural numbers `f(n) = 2 * n + 1`.

```java
class TwicePlus1 extends UnaryFunc<Nat, Nat> {
  TwicePlus1(Nat ignored) { super(ignored); }
  Succ apply(Nat n) {
    return n.plus(n).succ();
  }
}
```

Class `TwicePlus1` represents that function by replacing multiplication by 2 with addition of arguments and incrementation by calling `succ`. Notice that since for every natural argument, result of such a function will be positive number, we can encode it within type system by declaring result as `Succ` type, while still passing `Nat` as second type argument to `UnaryFunc`. This is demonstration of aforementioned *covariant method overriding* in *FGJ* - we can declare result type of overriden method as a subtype of result of method declared in super class, even if this type was a type variable - we can now see that subtyping takes care of resolving type variables and  actual type arguments passed; that's the reason why we need contexts *Δ*.

Let's review erasure of classes `UnaryFunc` and `TwicePlus1`.

```java
class UnaryFunc extends Object {
  Object ignored;
  UnaryFunc(Object ignored) { super(); this.ignored = ignored; }
  Object apply(Object arg`) {
    return this.ignored;
  }
}

class TwicePlus1 extends UnaryFunc {
  TwicePlus1(Object ignored) { super(ignored); }
  Object apply(Object n`) {
    return (Nat)((Nat)(n`).plus((Nat)n`)).succ();
  }
}
```

The same as before, generic types were removed from our classes and replaced with their bounds - `Object`s. Covariant method overriding is not present in *FJ*, so erasure had to ensure that types in methods signatures in both classes are identical. Proper casts were inserted in overriden method `apply`:

 * two casts around reference to the variable *n'* - to recapture its type, which was `Nat` in example with generic types
 * cast to `Nat` around invocation of method `plus`, as well in previous examples

#### Combining it together

Let's extend our `List` class to support mapping its elements with unary functions.

```java
class List<X extends Summable<X>> extends Object {
  ...
  <Y extends Summable<Y>> List<Y> map(UnaryFunc<X, Y> f) {
    return new Nil<Y>();
  }
}

class Cons<X extends Summable<X>> extends List<X> {
  ...
  <Y extends Summable<Y>> List<Y> map(UnaryFunc<X, Y> f) {
    return new Cons<Y>(f.apply(this.head), this.tail.map(f));
  }
}
```

In base class we added method `map` parametrized with type parameter `Y` which takes unary function and simply constructs empty list of summables `Y`. In `Cons` we return new list with function `f` applied to the `head` element and `tail` mapped by `f`.

How the erasure of the added methods look like?

```java
class List extends Object {
  ...
  List map(UnaryFunc f`) { return new Nil(); }
}

class Cons extends List {
  ...
  List map(UnaryFunc f`) {
    return new Cons(
      (Summable)(f`.apply(this.head)),
      this.tail.map(f`)
    );     
  }
}
```

`UnaryFunc` in `map` argument occurs in erased version. Then cast to `Summable` was inserted around `apply` invocation.

Finally, let's construct example program which uses all classes we defined so far.

```java
new Cons<Nat>(
  new Succ(new Zero()),
  new Cons<Nat>(
    new Succ(new Succ(new Zero())),
    new Nil<Nat>()
  )
).map<Nat>(new TwicePlus1(new Zero()))
 .sum(new Zero())
```

We construct list `[1,2]`, map it by function `TwicePlus1` and sum all elements of resulting list with 0.

Erased version contains only topmost cast to `Nat` (remember, `sum` result type was `Summable`, but we have concrete subclass here).

```java
(Nat)(new Cons(
  new Succ(new Zero()),
  new Cons(
    new Succ(new Succ(new Zero())),
    new Nil()
  )
).map(new TwicePlus1(new Zero()))
 .sum(new Zero())
)
```

Erased program evaluates to encoding of number `8`, as we expected.

```
new Succ(new Succ(new Succ(new Succ(new Succ(new Succ(new Succ(new Zero())))))))
```

# Erasure properties

We have seen type erasure in action on programming language, which although simplified to bare minimum, is able to encode, typecheck and evaluate quite advanced examples. We reviewed erasure of all examples and saw types of some of them and they corresponded  to types found by generic typechecker. Moreover, erased programs behaved exactly as we expected when we were defining their generic version. Is it matter of convenient examples, or is it kind of general property?

Authors of original *Featherweight Java* paper come with answer, stating several theorems, which most important are:

1. **Erasure preserves typing**. For all well-typed *FGJ* class tables, they are well-typed after erasing under *FJ* typing rules.
2. **Erasure preserves execution results**. If well-typed *FGJ* program evaluates to some value *w* in type-passing semantics, then erased program evaluates to erasure of value *w* in *FJ* evaluator.

There are some technical difficulties in proving second theorem, connected with insertion of casts during erasure. They are proved and well commented in the appendix to the *Featherweight Java* paper, which I strongly encourage to read, if you are interested in detailed explanation of type erasure.

# Conclusion

We have discussed one of possible implementation of generic types - *type erasure*. There are several known problems in languages or development platforms built on top of idea of erasing generic types, amongst which the most popular is *JVM*. Deep understanding of pure idea and ability to review (although simplified, yet) actual implementation will allow you to better understand consequences of the limitations and their real roots. 

# Resources

* A. Igarashi, B. C. Pierce, P. Wadler - [Featherweight Java: A Minimal Core Calculus for Java and GJ](http://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf)
* [my slides](http://www.ii.uni.wroc.pl/~dabi/courses/PJZO14/pkrzeminski/fj.pdf) from presentation at  PZJO14 seminary at University of Wrocław

