<p align="center">
  <img src="https://i.ibb.co/5hs2Ggg/mooncake-font-01.png" height="300px"/>
</p>

<br />
<br />

![Lint Review](https://github.com/yurtsiv/mooncake/workflows/Review/badge.svg)

Mooncake is a functional, dynamic programming language.

### ❗ In early development. Any kind of proposals are welcome since there's nothing to break :)

## Tutorial

**Data types**

```
let number = 1

let boolean = True

let string = "Hello there!"

let function = (a, b) do
  a + b
end

# Lists are heterogenous i.e. can contain elements of different types. It's a comment btw
let list = [number, boolean, string, function]
```

**Operators**

```
# Algebra
let sum = 4 + 2
let sub = 4 - 2
let div = 4 / 2
let mul = 4 * 2
let mod = 4 % 2

# Comparison
let eq = 2 == 2
let gt = 4 > 2
let gte = 4 >= 2
let lt = 2 < 4
let lte = 2 <= 4

# Boolean logic
let false = !True
let and = True && False
let or = True || False

# Lists/strings
let list = [1, 2, 3]
let firstElem = list(0)
let listLen = len([1, 2, 3])
let strLen = len("Mooncake")
let listConcat = [1, 2, 3] ++ ["Ready", "or", "not", "here", "I", "come"]
let strConcat = "Hello" ++ " there"
let strListConcat = "Hi" ++ [1, 2, 3] # will result in ["H", "i", 1, 2, 3]
let listStrConcat = [1, 2, 3] ++ "Hi" # will result in [1, 2, 3, "H", "i"]
```

**Conditional expressions**

```
# Conditional expressions are expressions, not statements i.e. they return something
let truth1 =
  if 4 >= 2 then
    "4 is greater or equal than 2"
  else
    "4 is smaller than 2"
  end

# No elseif yet
let truth2 =
  if !(2 == 2) then
    "2 is not equal to 2"
  else
    if 3 == 2 then
      "3 is equal to 2"
    else
      "3 is not equal to 2"
    end
  end
```

**Functions**

```
# The last statement of a function body is what the function returns
let add = (x, y) do
  let sum = x + y
  sum
end

let three = add(1, 2)

# Recursion
let fib = (n) do
  if n < 3 then
    1
  else
    fib(n - 1) + fib(n - 2)
  end
end

# Higher order functions
let mul = (x) do
  (y) do x * y end
end

let apply = (f, val) do
  f(val)
end
  
let eight = apply(mul(4), 2)
```

## Installation

### Build from source

**Install** [Stack](https://docs.haskellstack.org/en/stable/README/)

```
curl -sSL https://get.haskellstack.org/ | sh
```

or

```
wget -qO- https://get.haskellstack.org/ | sh
```

**Clone the repo**

```
git clone https://github.com/yurtsiv/mooncake.git && cd mooncake
```

**Build and install executable**

```
stack install
```

**Run a programm**

```
mooncake hello.mc
```

## TODO

Short-term

- [ ] Floating point numbers
- [x] Character type
- [x] Boolean `or` & `and`
- [x] List elements access (`list(0)`)
- [ ] Built-in `print` function
- [x] `/=` operator
- [x] Higher order functions
- [ ] Better error reporting
- [ ] Automatic function currying
- [ ] Documentation

Long-term

- [ ] CLI
- [ ] REPL
- [ ] Module system
- [ ] Virtual machine
- [ ] Static type system (?)

## Contributing

Feel free to create an issue or submit a PR (check out the development guide below). You can get in touch with me via E-mail yurtsiv.stepan@gmail.com

## Development

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)

```
curl -sSL https://get.haskellstack.org/ | sh
```

or

```
wget -qO- https://get.haskellstack.org/ | sh
```

2. Fork the repo

3. Make changes in the forked repo on a separate branch (name of the branch should briefly describe the issue)

4. Play around with your changes

```
stack run programm.mc
```

5. Run tests

```
stack test
```

6. If tests succeed, create a PR

Detailed explanation of [how to create a Pull Request](https://www.digitalocean.com/community/tutorials/how-to-create-a-pull-request-on-github).
