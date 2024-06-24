# Shiro Interpreter

## Project Overview

This project implements an interpreter for the Shiro programming language, as a solution for **Programming Languages and Paradigms** university course task. Shiro is a simple, statically typed language designed for educational purposes. The interpreter is written in Haskell and utilizes the BNFC parser generator.

The project structure is as follows:

- `Grammar/`: Contains the BNFC grammar for the Shiro language.
- `Src/`: Contains the Haskell source code for the interpreter.
    - `Common/`: Common data types and utilities used by both the type checker and evaluator.
    - `TypeChecker/`: Implementation of the Shiro type checker.
    - `Evaluator/`: Implementation of the Shiro evaluator.
- `Test/`: Contains test cases for the interpreter.

## Building

The interpreter can be built from source using the following commands:

```bash
make
```
This will generate an executable file named `interpreter` in the project root directory.

## Execution

To execute the interpreter with Shiro code from a file, use the following command:
```bash
./interpreter <file>
```

### Usage
```
USAGE:
    ./interpreter <FILE>
    ./interpreter OPTIONS

OPTIONS:
    -h, --help
        Display this help message.

ARGUMENTS:
    <FILE>
        File to interpret. Use no argument to read from standard input.
```

## Language Specification

### Types

Shiro is a statically-typed language, and every variable and expression has a type that is known at interpreting time. It offers three basic types:

*   **Int**: Represents integer values.

    ```
    val x: Int = 10; 
    ```

*   **Bool**: Represents boolean values - `True` or `False`.

    ```
    val flag: Bool = True;
    ```

*   **String**: Represents a sequence of characters.

    ```
    val message: String = "Hello, Shiro!";
    ```

### Variables and Constants

Shiro provides two ways to store data: as variables and constants.

*   **Variables:** Variables are mutable storage locations. You declare a variable using the `val` keyword, followed by its identifier, a colon, its type, an assignment operator (`=`), and an initial value.

    ```
    val count: Int = 0;  // Declares a variable 'count' of type Int and initializes it with 0

    count = count + 1;  // Updates the value of 'count'
    ```

*   **Constants:** Constants are immutable storage locations. Once declared, their values cannot be changed. You declare a constant using the `const` keyword, similar to variable declaration.

    ```
    const pi: Int = 3;     // Declares a constant 'pi' of type Int 

    pi = 4;              // This will result in a type checking error
    ```

**Scope:** Shiro uses block-level scoping, meaning a variable or constant declared inside a block (`{}`) is only accessible within that block.

### Static Typing

Shiro is a statically typed language. Every variable, function parameter, and expression is checked at the type-checking stage.

### Input/Output

Shiro provides 3 built-in functions `printInt`, `printString` , and `printBool` for outputting values to the console. 

**Print Functions:**

-   `printInt(int:  const Int)`: Prints the `int` value to the console. 
-   `printBool(bool:  const Bool)`: Prints the `bool` value to the console.
-   `printInt(string:  const String)`: Prints the `string` contents to the console.

**Examples:**

```
printInt(10);           // Output: 10
printBool(True);          // Output: True
printString("Hello");       // Output: Hello
```

### Literals

Literals are used to represent fixed values in your code. Shiro supports the following types of literals:

* **Integer Literals:**  Represented as whole numbers without any decimal points. 

    ```
    10
    0
    -5
    ```

* **Boolean Literals:** Represented as `True` or `False`.

    ```
    True
    False
    ```

* **String Literals:** Enclosed in double quotes (`"`).

    ```
    "Hello, Shiro!"
    "This is a string."
    ```

### Arithmetic Operators

Shiro supports the following arithmetic operators for integer values:

*   **`+`** (Addition): Adds two operands.
*   **`-`** (Subtraction): Subtracts the second operand from the first.
*   **`*`** (Multiplication): Multiplies two operands.
*   **`/`** (Division): Divides the first operand by the second (integer division).
*   **`%`** (Modulo): Returns the remainder of a division.

**Examples:**

```
5 + 2   // Evaluates to 7
10 - 3  // Evaluates to 7
4 * 5   // Evaluates to 20
10 / 3  // Evaluates to 3 
10 % 3  // Evaluates to 1
```

### Comparison Operators

Shiro provides comparison operators for comparing values, which return a boolean result (`True` or `False`):

*   **`==`** (Equal to): Checks if two operands are equal.
*   **`!=`** (Not equal to): Checks if two operands are not equal.
*   **`<`** (Less than): Checks if the left operand is less than the right operand.
*   **`>`** (Greater than): Checks if the left operand is greater than the right operand.
*   **`<=`** (Less than or equal to): Checks if the left operand is less than or equal to the right operand.
*   **`>=`** (Greater than or equal to): Checks if the left operand is greater than or equal to the right operand.

**Examples:**

```
5 == 5   // Evaluates to True
5 != 4   // Evaluates to True
3 < 5    // Evaluates to True
10 > 5   // Evaluates to True
2 <= 2   // Evaluates to True
5 >= 3   // Evaluates to True
```
### Logical Operators

Logical operators are used to combine or modify boolean expressions. Shiro supports:

*   **`&&`** (Logical AND): Returns `True` if both operands are `True`.
*   **`||`** (Logical OR): Returns `True` if at least one of the operands is `True`.
*   **`!`** (Logical NOT): Reverses the logical state of its operand.

**Examples:**

```
True && True    // Evaluates to True
True && False   // Evaluates to False
True || False   // Evaluates to True
False || False  // Evaluates to False
!True           // Evaluates to False
!False          // Evaluates to True
```

### Ternary Operator

Shiro provides a concise way to express conditional assignments using the ternary operator (`condition ? expr1 : expr2`):

*   **`condition ? expr1 : expr2`**:  If `condition` evaluates to `True`, the expression `expr1` is evaluated and its value is returned. Otherwise,  `expr2` is evaluated and its value is returned.

**Example:**

```
val age: Int = 25;
val status = age >= 18 ? "Adult" : "Minor"; 
// 'status' will be assigned "Adult"
```
### Conditional Statements

Conditional statements allow you to control the flow of execution based on conditions.

*   **`if` statement:**  Executes a block of code only if the condition evaluates to `True`.

    ```
    if (x > 10) {
        printString("x is greater than 10");
    }
    ```

*   **`else` statement:**  Optionally used with `if` to execute a block of code when the `if` condition is `False`.

    ```
    if (x > 10) {
        printString("x is greater than 10");
    } else {
        printString("x is not greater than 10");
    }
    ```

### Loops

Loops provide a way to execute a block of code repeatedly.

*   **`while` loop:**  Executes a block of code as long as the loop condition evaluates to `True`.

    ```
    val i: Int = 0;
    while (i < 5) {
        printInt(i);
        i = i + 1;
    }
    ```

*   **`for` loop:**  A structured loop used for iterating a specific number of times.

    ```
    for i from 1 to 5 {
        printInt(i); // Prints 1, 2, 3, 4, 5
    }
    ```

### Functions

Functions allow you to group reusable blocks of code.

#### Function Definitions

Functions are defined using the `fun` keyword, followed by the function name, parameters in parentheses, and the function body enclosed in curly braces:

```
fun greet(name: String): String {
    return "Hello, " + name + "!";
}

fun add(a: Int, b: Int): Int {
    return a + b;
} 
```

-   **Parameters:** Parameters are inputs to a function, defined within the parentheses. They have a name and a type.
-   **Return Type:**  The return type specifies the type of value the function will return. If omitted, it defaults to `Unit` (similar to `void` in other languages), indicating the function doesn't return a value.
-   **Function Body:** Contains the code to be executed when the function is called.

#### Function Calls

In Shiro functions are called by using its name followed by arguments in parentheses.

```
val message: String = greet("Alice");
printString(message);  // Output: Hello, Alice!

val sum: Int = add(5, 3);
printInt(sum);       // Output: 8
```

#### Recursion

Functions can be called from its own body, either directly or indirectly,

```
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1;
    } else {
        return n * factorial(n - 1); 
    }
}
```

### Parameter Passing

Shiro supports two main methods of parameter passing:

#### Pass-by-Value

When a parameter is passed by value, a copy of the argument's value is created and passed to the function. Modifying the parameter inside the function doesn't affect the original variable.

```
fun increment(num: Int) {
    num = num + 1; // Modifies the local copy
}

val x: Int = 5;
increment(x); 
printInt(x); // Output: 5 (original 'x' remains unchanged)
```

#### Pass-by-Reference

When a parameter is passed by reference, a reference to the original variable is passed to the function. This allows the function to modify the original variable directly.

```
fun swap(ref a: Int, ref b: Int) {
    val temp: Int = a;
    a = b;
    b = temp;
}

val x: Int = 10;
val y: Int = 20;
swap(x, y);
printInt(x); // Output: 20
printInt(y); // Output: 10
```
### Scope and Binding

Scope determines the visibility and lifetime of variables and functions in your code. Binding refers to the association of an identifier with its declared entity.

#### Lexical Scoping

Shiro uses lexical scoping (also known as static scoping). This means that the scope of a variable is determined by its position in the source code, specifically by the block in which it is declared. 

```
{ // Outer block
    val x: Int = 10;

    { // Inner block
        val y: Int = 5;
        printInt(x); // x is accessible here (10)
    }

    printInt(y); // Error! y is not accessible here 
}
```

#### Overriding

You can redefine a variable within an inner block, effectively hiding the variable with the same name from an outer scope. This is called variable overriding or shadowing.

```
val x: Int = 10;

{
    val x: Int = 5;  // Overrides the outer 'x'
    printInt(x);     // Output: 5
}

printInt(x);         // Output: 10 (outer 'x')
```

#### Static Binding

Shiro uses static binding (or early binding), meaning that the association of a function call with the function definition happens at interpreting time based on the types of the arguments and the function signature.

#### Nested Functions

Shiro supports defining functions within other functions. Nested functions enhance encapsulation and code organization. They have access to variables in their own scope as well as variables in the enclosing function's scope.

```
fun outerFunction(): Int {
    val x: Int = 10;

    fun innerFunction(y: Int): Int {
        return x + y; 
    }

    return innerFunction(5); // Call the nested function 
}
```

### Higher-Order Functions and Closures

Shiro supports features that allow functions to be treated as first-class citizens, enabling functional-like feel.

#### Higher-Order Functions

Higher-order functions are functions that can take other functions as arguments or return functions as results.

```
fun apply(x: Int, f: (Int) -> Int): Int {
    return f(x);
}

fun square(n: Int): Int {
    return n * n;
}

val result: Int = apply(5, square); 
printInt(result); // Output: 25
```

#### Anonymous Functions

Anonymous functions, also known as lambda expressions, allow you to define functions inline without giving them a name.

```
val addOne: (x: Int): Int = (x: Int) -> Int => x + 1; // Define an anonymous function
printInt(addOne(5));                  // Output: 6
```

#### Closures

Closures are anonymous functions that can "capture" and access variables from their surrounding scope, even after the outer function has finished executing. 

```
fun makeCounter(): (Int) -> Int {
    val count: Int = 0; // 'count' is captured by the closure

    return (increment: Int):Int => {
        count = count + increment; 
        return count;
    };
}

val myCounter: (Int) -> Int = makeCounter();
printInt(myCounter(1)); // Output: 1
printString("\n");
printInt(myCounter(5)); // Output: 6 
printString("\n");
```

### Runtime Exception Handling

While Shiro's is statically typed language some errors may occur during program execution (runtime).

#### Division by Zero Exception

Attempting to divide an integer by zero will result in a runtime error, halting program execution.

```
val result = 10 / 0; // This will cause a "division by zero" error.
```

## Feature Table (For assessment purposes)
for 15 pts:
- [x] 01 (three types)
- [x] 02 (literals, arthmetic, comparison)
- [x] 03 (variables, assignment)
- [x] 04 (print)
- [x] 05 (while, if)
- [x] 06 (functions/procedures, recursion)
- [x] 07 (passing arguments by value/reference)
- [x] 08 (read-only variables i pętla for)

for 20 pts:
- [x] 09 (shadowing and static binding)
- [x] 10 (runtype exception handling)
- [x] 11 (functions returning value)

for 30 pts:
- [x] 12 (4) (static typing)
- [x] 13 (2) (nested functions with static binding)
- [ ] 14 (1/2) (rekordy/listy/tablice/tablice wielowymiarowe)
- [ ] 15 (2) (krotki z przypisaniem)
- [ ] 16 (1) (break, continue)
- [x] 17 (4) (higher order functions, lambdas, closures)
- [ ] 18 (3) (generatory)

Razem: 30/31
