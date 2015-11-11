# Deep understanding of type erasure

[TOC]
## Introduction

Certain statically typed programming languages provide concept of generic types, which is ability to parametrize types/classes with type parameters. This allows to write polymorphic code which can work with any type arguments. One of the key application we can found in standard libraries and type-argument-independent code like Lists, Vectors, Sets, Maps and algorithms working with them (e.g. sorting algorithms) which can be implemented once as "template code" and pervasively used when applied to concrete types.

There are several different possible implementation of generics, including:

 * *type-passing* semantics - it preserves informations about type parameters at runtime, which allows to differentiate `List<Integer>` to `List<String>` for example; this implementation is chosen in *.NET* languages like *C#*.
 * *instance-generating* (better name needed?) - this one is present in C++ language; the compiler generate instance of type-parametrized class for every set of generic parameters. So at runtime we have different classes like `List_of_Integers` or `List_of_Strings`.
 * *type-erasure* semantics - it eliminates informations about type parameters at compilation time, replacing them with their so-called *type bounds*; at runtime we have only `List` classes both for list of integers and list of strings (it's just a list of non-distinguishable objects); This implementation is used by Java compiler.

In this article we will try to better understand the last one. As a tool, we whill use [Featherweight Java](http://www.cis.upenn.edu/~bcpierce/papers/fj-toplas.pdf) by A. Igarashi, B. C. Pierce and P. Wadler - minimal core calculus for Java and Generic Java. More precisely, we will review practical implementation of this calculus, included in this repository, written in Scala as a host language.

### Why this approach?

Well, there are plenty of articles describing *type erasure*, its practical consequences and limitations and tricks how to bypass them. As the opposite, this article will try to reveal some subtelties around understanding how the erasure is actually defined, what are the *type bounds* and how the substitutions work. To clearly talk about such delicate ideas, we need some precise and well-defined concepts, which *Featherweight Java* paper provides in approachable manner.

Although, reading the original paper is not required to understand the code included in this repository. All formal, mathematical definitions are straithforwardly translated to Scala code counterparts. 

## Featherweight Java

Looking for a tool to precise reasoning about Java's type system we need to focus on modlelling only those parts of the language which are important from the type system perspective while ommiting those language features, which are not. Trying to model full Java in such a way will result enormous calculus which would be hard to grasp. *Featherweight Java* favours compactness over completeness then, providing only few combinators, while still being legal subset of Java, only little larger than original [λ-calculus](https://en.wikipedia.org/wiki/Lambda_calculus).

To achieve simplicity, language is cut from "not important" features, which means:

* no concurrency primitives
* no reflection
* no interfaces
* no method overloading
* no inner classes
* no static members
* no member access control - all methods and fields are public
* no primitive types
* no null pointers
* no assignments

Instead, we focus only on minimal language subset, including:

* mutually recursive class definitions
* object creation
* field access
* method invocation, overriding and recursion through `this`
* subtyping
* casting

### Syntax

Now, let's define syntax for our reduced language. At the beginning we introduce type aliases for *variable names* and *type names*, wchich are in fact represented as strings in host language.

```scala
  type VarName = String
  type TypeName = String
```

#### Classes

As in full language, in *FJ* we have *classes*, which are identified by *name* and can declare super classes (*baseClassName*), which is obligatory (even if it is "Object") in our calculus. Beside, classes contain separate lists of field and method definitions.

*Field* is just type-annotated name.

*Method* is identified by its name, it annotates its result type, arguments list and body.

```scala
case class Class(name: TypeName,
                 baseClassName: TypeName,
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

##### Where are class constructors?

For now, we assume that every class has single constructor implicitly defined which satisfies following conditions:

* its number of arguments and their types corresponds to field definitions
* it only performs initialization of class fields

For example, having such Java class:

```
class A { 
  X x; Y y; Z z;
}
```

... we are assuming, that it has such straithforward constructor:

```java
public A(X x, Y y, Z z) {
  this.x = x; this.y = y; this.z = z;
}
```

For simplicity, we usually omit to write them down.

#### Expressions

So far so good, but what is definition of method bodies? How the language *abstract syntax tree* looks like?

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

We have 5 types of expressions:

* **variable access** - variable is denoted by its *name*
* **fields access** - field is denoted by its *fieldName* on object that will be result of computing *expr*
* **method invocation** - again we denote methods by its *methodName* on object computed from *expr*, but in this case before invoking the method we should also compute its arguments (*args*)
* **new object construction** - class is identified by *name*, similarly to method invocation arguments (*args*) must be computed before object construction
* **type cast** - expression should be casted to destination *className*

#### Example

OK, it's time for little example. Let's assume we want to encode simple program - implementation of immutable pairs.

```
class A extends Object { }
class B extends Object { }
class Pair extends Object {
  Object fst;
  Object snd;
  Pair(X fst, Y snd) {
    this.fst = fst; this.snd = snd;
  }
  Pair setfst(Object newfst) {
    return new Pair(newfst,this.snd);
  }
}
```

Such classes could be easily defined in our newly defined calculus, as follows.

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
  methods = List(Method(
    name = "setfst",
    resultType = "Pair",
    args = List(Argument("newfst", "Object")),
    body = New(
    className = "Pair",
    args = List(Var("newfst"), FieldAccess(Var("this"), "snd"))
  )
  ))
)
```

I think that example is self-describing. You can easily find corresponding parts from original Java syntax in our classes defined as Scala's data structures.

#### Executable programs

In full Java we have concept of *executable classes* which define static method:
```java
public static void main(String[] args)
```

But in our calculus we don't have concept of static methods. How then we define executable classes? In fact we doesn't have to use classes for that purpose. It is sufficient to defined *Program* as class table paired with expression we want to evaluate.

```scala
type ClassTable = Map[TypeName, Class]

case class Program(classTable: ClassTable, main: Expr)
```

In this example class table is mapping from type names to class definitions.

So let's extend our previous example with simple computation.

```
new Pair(new A(), new B()).setfst(new B())
```
... which should evaluate to the irreducible form:

```
new Pair(new B(), new B())
```

Translating our example to *FJ* structures in Scala, we would like to have:

```scala
val classTable: ClassTable = buildClassTable(List(A, B, Pair))
val main: Expr = Invoke(
  New("Pair", List(New("A"), New("B"))),
  "setfst",
  List(New("B"))
)
val program = Program(classTable, main)
val result = evalProg(program)
// should compute to
// New("Pair", List(New("B"), New("B")))
```

Helper function `buildClassTable` build Scala's *Map* from list of class definitions. 

Note that in example above we used function `evalProg` which executes our *Featherweight Java* programs. Details are described in one of following chapters. 

### Type system

Like every language with static typing, our *FJ* should have ability to check type correctness without actually running a program. Let's review type checker implementation that we have.

#### Types

So how the types clould be represented in our host language?

```scala
trait Type
case class SimpleType(name: TypeName) extends Type
case class MethodType(argTypes: List[SimpleType],
                      resultType: SimpleType) extends Type
```

Without generic types, our type system is really simple. It has only two form of types:

* **simple types** - they are reffering by name to valid class definitions;
* **method types** - they are auxilliary types useful when typechecking methods, while we have return type and types for methods' arguments; since in *FJ* methods are not first-class values, they refer to *SimpleTypes* rather than *Types*.

We distinguish special *top type* which every class extends. It is *Object* type, which can be simply encoded as:

```scala
SimpleType("Object")
```

#### Subtyping

As part of our type checking phase, we will have to settle so called *subtyping relation*. In simple words, *subtyping* is defined as reflexive and transitive closure of relation defined by `extends` keyword between classes.

We can decide this relation only by looking at class table and recursively resolving path from type *t* to *u*. Implementation of that function can look as follows.

```scala
def isSubtype(t: TypeName, u: TypeName)
             (ct: ClassTable): Boolean = {
  if(t == "Object") u == "Object"
  else (t == u) || {
    val tBase = ct(t).baseClassName
    tBase == u || isSubtype(tBase, u)(ct)
  }
}
```

#### Typing expressions

You could expect, that type-checking expression takes expression and returns a type if its well-typed or indicates type-error in expression. That's partially true. To decide that relation, we need class table and some kind of mapping from free variables to its types (more precisely, mapping only to *simple types* is enough). Let's call that mapping `Γ` (gamma).

```scala
type Γ = Map[VarName, SimpleType]
def exprType(e: Expr)(g: Γ, ct: ClassTable): Option[SimpleType]
```

I will not rewrite full implementation in this article. You can find it at `fj.Types.exprType` in this repository. Instead, I'll try to explain subtelties of deciding about typing expressions. Let's review it for all forms of expressions:

* **Var** -- we have variable name, but in case of expressions it is free variable, so it have to be defined in our mapping *Γ*; otherwise, we return *None*, as there is some type error (maybe incorrect variable name?)
* **FieldAccess** -- first, we have to type expression; having such type (more precisely, *SimpleType*), we can look up into class table and gather methods in that class and all superclasses; then we can find field by name and return its type as a resulting type
* **Invoke** -- this case is very similar to *FieldAccess*, but now we have type annotations for method arguments; we have to type-check all expressions passed as invocation parameters and ensure that every computed type is subtype of corresponding method argument type annotation; if it succeeds, as resulting type we take method's return type
* **New** -- this case is indeed similar to method invocation sice *new* operator actually invokes constructor (which we assumed to be defined implicitly); the only one difference is that instead of methods' arguments we are taking all fields from class definition (and all its super classes); as a result type, we take *SimpleType* with class name whose object we are trying to construct
* **Cast** -- this one is the most interesting one; let's assume that actual expression type is *D* and cast destination type is *C*; we have 3 following cases:
  * *D* is subtype of *C* -- this is case of so-called *upcasting* - we can always freely perform cast to superclass of actual type, yielding *C* as result type
  * *C* is subtype of *D* -- this is case of so-called *downcasting* - we can perform cast to subclass only if it is actual subclass (i.e. *C* != *D*, since our subtyping relation is reflexive)
  * no *C* is subtype of *D* neither *D* is subtype of *C* -- this is probably the most subtle case, it's called *stupid cast*; actually Java compiler doesn't allow stupid casts and treats them as type errors (which is intuitively correct); in *Featherweight Java* they are present to formulate type soundness proof in small-step semantics and its special nature is indicated by *supid warning*. You can read about details in original *FJ* paper.

#### Typing classes and methods

Ok, so how we type-check our class definitions? It seems, that it's enough to check all methods whether they are well-typed.

And when a method is well-typed? Well, in case of method, the only one thing we have to check is to check whether declared return type is compatible with actual inferred method body type. But wait - method body is an *Expr* and type-checking expressions requires special mapping *Γ* to be define. So how we define this mapping here?

As *Γ* is mapping from variable names to variable types, our question is: what variables we can refer to inside method body? Actually, there are 2 kind of such variables:

* method arguments
* class fields

But every class field is accessed through `this` metavariable, so it is enough to take into consideration only *this* mapped to class type inside method is defined together with all argument names mapped to their type annotations.

There is one subtelty connected with *method overriding* here. Since we have no method overloading in *Featherweight Java*, we say that some method *m1* is *overriding* in some class if method *m2* with such name is defined in one of its super classes. Since in case of *FJ* covariant result types is not allowed in case of method definitions, we have to ensure that in this case methods *m1* and *m2* corresponds considering its signatures. In other words, every for all *m1* and *m2* methods arguments must be the same; moreover result types of *m1* and *m2* must be the same.

All the implementations you can find in `fj.Types.methodTypes` and `fj.Types.classTypes`.

#### Typing programs

Our *Program* definition is well typed if and only if:

* all classes in class table are well-typed
* expression *main* is well-typed in empty *Γ* environment

We can express this type-checking of full programs as function with following signature.
```
Program => Option[Type]
```
Implementation of this function you can find in `fj.Types.programType`.

### Execution semantics

In original *Featherweight Java* paper there are only operational semantic rules given, which doesn't specify order of execution. We define evaluation that is the same as in full Java (it's called *call by value*), when we evaluate all methods' arguments before method is executed. 

Anyway, evaluation is defined as reduction to so called object *normal form*. How this normal form look like?

#### Values

Our language is object-oriented and every value in memory has form of object instantiated with arguments passed to a class's constructor are also objects.

```scala
case class Value(className: TypeName, args: List[Value])
```

#### Evaluating expressions

Our evaluator is just a function that takes expression, variable environment and class table and returns a value.

```scala
type Env = Map[VarName, Value]
def eval(expr: Expr)(env: Env, cm: ClassTable): Value
```

> **Error handling**
> For the sake of simplicity in this article we won't bother too much about error handling and encoding them in evaluator's return type. When expression can't be evaluated correctly it just throw *RuntimeException* with appropriate error message.

Let's review our expression's evaluator implementation considering form of expressions we want to evaluate:

* **Var** -- we access to a variable, so we lookup its value in our variable environment and return it as as value of evatuation
* **FieldAccess** -- first we have to evaluate underlying expression whose field we want to access; having such an object, we have to look up into class table for all fields (including super-type fields) of class of that object; then we have to select corresponding argument of computed underlying obejct, which corresponds to name we are trying to access
* **Invoke** -- this case first reduces to evaluating underlying expression; then we have to evaluate all actual arguments of method we are trying to evaluate; then we can evaluate method body within mapping *Env* enriched with:
  * `this` mapped to object (value) of evaluated underlying class
  * all argument names mapped to its actual evaluation results (values)
* **New** -- it is probably the simplest case - we instantiate object with evaluated arguments as constructor arguments
* **Cast** -- we seamlessly allow our underlying expression to be evauated further, as we checked types before

#### Evaluating full programs

Now, we can define our *FJ* program's evaluator. We simply evaluate our program's main body expression within empty *Env* and class table defined in our program.

The signature is:

```scala
def evalProg(prog: Program): Value
```

...and you can find the implementation under `fj.Eval.evalProg`.

## Featherweight Generic Java

There is one problem with `Pair` class example defined in FJ. The language provide types, but the `Pair` implementation is not type safe. Having object `new Pair(new A(), new B())` we can easily substitute first argument with `new B()`. To restrict that, we could define class `class Pair_A_B { A fst; B snd; }`, but that kind of implementation is not re-usable for any `A` and `B` types any more. In full Java, with generic types, we would want to solve this problem by parametrizing our `Pair` class with type parameters, let's say *X* and *Y* and use them as types of first and second field, correspondingly.

```
class Pair<X extends Object, Y extends Object> extends Object {
  X fst;
  Y snd;
  Pair(X fst, Y snd) {
    this.fst = fst; this.snd = snd;
  }
  <Z extends Object> Pair<Z, Y> setfst(Z newfst) {
    return new Pair<Z, Y>(newfst, this.snd);
  }
}
```

Now we have also generic `setfst` method, which can be executed for any `newfst` argument which now it has type `Z` in argument; this method can correctly return new pair with first type annotated to `Z`.

How could we extend our minimal-core language with such kind of generic types?

### Changes in syntax

Let's review extended syntax definition of our language, which we call *Featherweight Generic Java* (or *FGJ* shorthanded).

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

Ok, but how can we define our parametrized classes now?

```scala
case class BoundedParam(typeVar: TypeVar,
                        boundClass: ClassType)

case class Class(name: TypeName,
                 typeParams: List[BoundedParam],
                 baseClass: ClassType,
                 fields: List[Field],
                 methods: List[Method]) 
```

`BoundedParam` corresponds to single `Z extends Object` from our example. It is definition of type variable, bounded by some class type. In full Java, when bound is trivial (i.e. *Object*), we usually omit `extends Object` part and leave only type variable. Notice that using such definition, we can write type bound as recursive type expression, like: `X extends C<X>` which is perfectly OK.

Classes are parametrized by a list of *bounded parameters*. Notice change in base class signature which now is not only name reference, but can be parametrized with type variables, like in example below.

```
class List<X extends Object> extends Collection<X> { ... }
```

How our fields and methods are represented in *FGJ*?

```scala
case class Field(name: VarName, fieldType: Type)
case class Method(name: VarName,
                  typeParams: List[BoundedParam],
                  resultType: Type,
                  args: List[Argument],
                  body: Expr)
case class Argument(name: VarName, argType: Type)
```

Only noticeable change is that now methods now can be parametrized with list of bounded parameters, as well as classes are. Field types, argument types and result types of methods are just adjusted to refined definition of *Type*.

`ClassTable` and `Program` definitions are straightforwardly adjusted to use newly defined types.

#### Expressions

AST for expressions is mostly unchanged. 

```scala
trait Expr
case class Var(name: VarName) extends Expr
case class FieldAccess(expr: Expr, fieldName: VarName) extends Expr
case class Invoke(expr: Expr,
                  methodName: VarName,
                  typeArgs: List[Type],
                  args: List[Expr]) extends Expr
case class New(classType: ClassType, args: List[Expr]) extends Expr
case class Cast(classType: ClassType, expr: Expr) extends Expr
```

The only difference beside type adjustments is in *Invoke* expression which now takes also list of type parameters to be instantiated, in contrast to full Java, where inference of invocation type arguments is obligatory.

### Extended type-checking

I will not provide full step-by-step description of FGJ type-checker as it probably might result in very long, formal and hard to understand text. Instead, I will describe only key differences and general idea to type-check programs with bounded type-parameters. Full description of typechecking rules for *FGJ* you can find in chapter 3.2 of original *FJ* paper.

#### Subclassing vs. subtyping

As our types gets more complicated, previous implementation of subtyping is not sufficient in this case. We want to distinguish 2 different relations:

* *subclassing* as relation between class names, which totally corresponds to *FJ subtyping*
* *subtyping* as real relation between all type forms, including parametrized ones

#### Δ mapping and bounds resolving

Similarly as we had *Γ* mapping for variables and its values, we define mapping from type variables to actual types - *Δ*.

```scala
type Δ = Map[TypeVarName, ClassType]
```

We will maintain this type variable environment inside of classes and methods. It will be initialized using classes and methods parameter lists. Type variables will be mapped to its defined bounds.

Having such mapping, we can define function for bounds resolving, which works generally for type variables and class types (which are its trivial bounds).

```scala
def bound(t: Type)(d: Δ): ClassType = t match {
  case TypeVar(name) => d(name)
  case u: ClassType => u
}
```
It is useful both in type-checker and during type erasure.

#### Deciding subtyping with Δ

Subtyping relation in *FGJ* can be decided within given context *Δ*. Still it is reflexive and transitive relation, but now has 2 interesting base cases:

* *subtyping type variables* - type variable *X* is subtype of its direct bound, which is *Δ(X)*; in fact the bound is the only information about type variable that we have in the program, so we reuse it here
* *subtyping classes* - a class *A* instantiated with some type arguments is subtype of its base class *B* where type parameters from class *A* were substituded by those type arguments

Analyzing one of previous example with `List` and `Collection`, we would have `List<Integer>` is subtype of `Collection<Integer>`, which is absolutely desired.

#### Covariant method overriding

One key difference between *FJ* and *FGJ* is that now we allow covariant method overriding.


### Type-passing evaluator

As mentioned in the introduction, there are several possible implementation of generics, one of them is *type passing*. How could we implement such semantics?

It would be straightforward extension to our previous *FJ* evaluator.

```scala
case class Value(classType: ClassType, args: List[Value])
type Env = Map[VarName, Value]
def eval(expr: Expr)(env: Env, cm: ClassTable): Value
```

Notice that subtle difference that is present in definition of values. Now, instead of class names, we have full class types, which hold actual type arguments!

We will not provide such implementation leaving reader ability to experiment and define such evaluator as an excercise.

## Type erasure

Instead, we focus on main topic of this article, which is *type erasure*. So what this erasure is all about? It's about erasing type parameters from classes/methods/expressions. More precisely, using our definitions, it will be translation from *FGJ* `Program` to *FJ* `Program`, which preserves *meaning* of original *FGJ* program.

### Erasing types

Starting from ground up, let's define erasure for our types.

*FGJ* types are either type variables or class types instantiated with some type arguments. In case of class types, we can omit all type arguments and leave only class name as erased type.

*|Pair&lt;A, B&gt;|<sub>Δ</sub> = Pair*

But what in case of type parameters? Remember that all type parameters are bounded, so first we have to resolve bound for class variable which is a class type. Then we can process as above.

Generally, we can implement type erasure as follows.

```
def eraseType(t: FGJ.Type)(d: Δ): FJ.TypeName = {
  bound(t)(d).className
}
```

*|T|<sub>Δ</sub> = C*,
&nbsp;&nbsp;&nbsp;&nbsp; where *C&lt;T<sub>1</sub>, ..., T<sub>n</sub>&gt; = bound<sub>Δ</sub>(T)*

### Erasing expressions

Erasure of expressions actually depends on erasure of types. 



### Erasing methods

### Erasing classes


## Erasure examples



## More resources

* original *Featherweight Java* paper
* slides from my presentation at PJZO14 (*Fundaments of Object Oriented Languages*) seminary at University of Wrocław













