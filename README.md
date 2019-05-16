Piotr Szulc 347277

Running on students.mimuw
1. Adjusting path: ```$ PATH=/home/students/inf/PUBLIC/MRJP/bin/:/home/students/inf/PUBLIC/MRJP/ghc-8.2.2/bin:$PATH```
2. Removing cabal hell ```$ rm -rf ~/.ghc ~/.cabal``` or storing contents of these folders elsewhere
3. Update cabal ```$ cabal update```
4. Install dependencies when in lakke's folder  ```$ cabal install --only-dependencies```
5. Generate grammar files: ```$ make generate_grammar```
6. Run lakke ```$ cabal run lakke```

Examples are in folder ```examples```.

Running programs from examples: ```$ cabal run lakke examples/good/fibonacci.lk```

***
Implemented all ticks below 20pts and above 20:

* Typechecker
   *** 
   Notes: Typechecker doesn't check whether function returns anything, it's runtime error: 

   ex.: ```int f() {} ``` Typechecks ok
   but ```int f() { return "hello string"; }``` throws typecheck error
    (examples/bad/typechecker)

* Nested functioncs with static binding (examples/good/nested.lk)
* Lambdas (examples/good/lambdas)
    ***
    Notes:
    ```
    (int) => bool isAgeGreaterThan100Predicate = bool :: (number :: int) -> {
        return number > 100;
    };
    ```
    
    ```() => bool``` - type of variable: function requring no arguments and returning bool

    isAgeGreaterThan100Predicate - variable name:

    ```
    lambdaReturnType :: (argumentName :: argumentType, ...) -> {...}

* Continue/Break (examples/good/continue_break.lk)