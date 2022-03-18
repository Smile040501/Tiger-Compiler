# Syntax of the SubTiger Language

Everything in SubTiger is actually an expression, and hence always evaluates to some value. Now, we may look at some sample code to understand the syntax better:

1. We can use a **variable** only after its declaration.\
   We need to separate every expression using a `;`.\
   The `print` function is a built-in function that prints the integer expression's value after evaluating.\
   The `println` function is also a built-in function that appends a **newline** also while printing.

    ```tiger
    x := 5;
    y := 7;
    z := x * y;
    println (x + z);
    print (z / y);
    ```

2. **For-loops** are also expressions. Both the start and end positions are inclusive. There are two types of for-loops:

    - Without any step size (defaults to `+1`)

        ```tiger
        for i := 0 to 10 do
            println i;
        done;
        ```

    - With step size mentioned

        ```tiger
        x := 5;

        for i := 10 to 1 by -2 do
            x := i - 2;
            println x;
        done;

        println x;
        ```

# Example Programs

Please refer to the example programs in the directory `tests/`
