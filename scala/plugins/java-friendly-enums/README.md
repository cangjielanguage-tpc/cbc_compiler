Java-friendly Enums Plugin
==========================

Dotty plugin that allows you to use Scala enums in Java switch statements
(since official compatibility with Java [is](https://github.com/lampepfl/dotty/issues/16391) 
[broken](https://github.com/lampepfl/dotty/issues/12637)).

Usage
-----

Annotate your Scala enumeration with `com.huawei.excelsior.dotty.annot.javaFriendly` annotation:

```scala
import com.huawei.excelsior.dotty.annot.javaFriendly

@javaFriendly
enum Bit:
  case Zero, One
```

Now you're able to use it in Java switch statements (read the ["Problem"](#problem) part to understand why you wasn't 
able to do it before):

```java
final Bit bit = Bit.valueOf("Zero");
switch (bit) {
  case Zero: System.out.println("0"); break;
  case One:  System.out.println("1"); break;
  default:   System.out.println("Impossible");
}
```

Problem
-------

Consider arbitrary Scala 3 enumeration, such as following:
```scala
enum Color:
  case Red, Green, Blue
```

And also consider Java switch statement using one of this enumeration values as expression:
```java
final Color c = Color.valueOf("Red");
switch (c) {
  case Red:   System.out.println("It's red!");   break;
  case Green: System.out.println("It's green!"); break;
  case Blue:  System.out.println("It's blue!");  break;
}
```

If you try to build Java code containing this statement, compiler will fail with an error.
For example, `javac` that comes with OpenJDK will produce the following message:
```java
error: an enum switch case must be the unqualified name of an enumeration constant
  case Red:
       ^
```

Such behaviour is resulting from two factors:
- the way Java language specification treats Java enum classes,
- the way Dotty compiler generates code for Scala 3 enumerations.

The Java Language Specification says 
([JLS §14.11](https://docs.oracle.com/javase/specs/jls/se8/html/jls-14.html#jls-14.11)):
>If the type of the switch statement's expression is an enum type,
>then every case constant associated with the switch statement must be
>an enum constant of that type.

Therefore, identifier under a case label must refer to a static field
declared in the present enum class and containing appropriate enumeration value.

However, Dotty generates two classes for each enum definition `E`:
- class `E` representing enumeration type;
- class `E$` representing companion object with all static members of enum `E`.

Naturally, enum constants end up in the class `E$`, not `E`, and Java compiler fails to find them.

Solution
--------

This plugin fixes described inconsistency by implementing the following steps:

1. To satisfy Java language specification and `javac` behaviour, class `E` is extended
   with the set of public static final fields denotating enumeration constants.

   For example, here is an appropriate declarations for the enumeration `Color`:
   ```java
   class Color {
     ...
     public static final Color Red;
     public static final Color Green;
     public static final Color Blue;
   }
   ```

2. Since `javac` will use content of these fields later to generate lookup tables and perform `switch`
   over enumeration, each of them must hold the proper enum value.

   To achieve this, each field is assigned the value of corresponding companion object field.
   However, these assignments must be performed very precisely, because at some period of time
   companion object fields may hold `null` value (see https://github.com/lampepfl/dotty/issues/12637).

   Therefore, it must be guaranteed that the companion class is initialized before the primary one
   (meaning exiting the `<clinit>` method). Following scheme is used to satisfy this requirement:
     1. Put all field assignment logic for the primary class in special static method `$$initEnumValues`.
     2. Make a call to this method the last operation in the `<clinit>` of the companion class.
     3. Make any instruction triggering the initialization of companion class (e.g., `getstatic` of its field)
        the first operation in the `<clinit>` of the primary class.

   Here is how `Color` and its companion class `Color$` look after this step:
   ```java
   class Color {
     ...
     public static final Color Red;
     public static final Color Green;
     public static final Color Blue;

     public static void $$initEnumValues() {
         Color.Red = Color$.Red;
         Color.Green = Color$.Green;
         Color.Blue = Color$.Blue;
     }

     static {
       Color$.MODULE$; // getstatic Field Color$.MODULE$:LColor$;
     }
   }
   
   class Color$ {
     ...
     static {
       ...
       Color.$$initEnumValues();
     }
   }
   ```
