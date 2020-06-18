# General things
* Functional (at least mostly)
* Everything is an expression (i.e. evaluates to something)
* Every function is curried
* Tab sensitive
* Modules are defined by file names

# Syntax

**Comments**
```
# comment

# no
# multiline
# comments
```

**Variable declaration**

```
# Primitive
let boolean = True
let integerNum = 1
let floatNum = 1.3
let char = 'c'

# Other
let array = [1,2,3]      # homogenous
let string = "string"    # is array of chars
let tuple = (1, "hello") # heterogenous, immutable
```

**Conditions**
```
let x = 1
let y = 2

let res =
  if (x - y) > 0:
    let sum = x + y
    sum
  elif (x + y) < 2:
    x 
  else:
    y
```

**Functions**
```
let add = (x, y) -> x + y
let multiply = (x, y) -> x * y

let doSomeMoreStuff = (x, y) ->
  let sum = add(x, y)
  let mulBy2 = multiply(2) # partial application
  let prod = mulBy2(sum)

  prod # last thing is returned

# does the same as function above
let doStuffWithComposition = (x, y) ->
  add(x, y) |> multiply(2)

let apply = (func, value) -> func(value)

# functions are fist class citizens
apply(print, "Hello there")
```

**Modules**

TODO: would be usefull to export/import everything except something

```
# Include everything
include Module

Module.something

# Include specific thing
include Module only (
  something,
  something2 as somethingElse
)

something

# Define module
export (f)

let a = 1
let f = (x) -> x + a
```
