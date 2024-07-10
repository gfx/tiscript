# TiScript - Turing-Incomplete TypeScript as a Configuration Language

TiScript is a configuration language designed to be intuitive and easy for humans and machines. It draws inspiration from TypeScript and JSON, offering the benefits of both:

* **Readability and maintainability** for humans, similar to JSON.
* **Type safety and structure** for machines inspired by TypeScript.

TiScript is intentionally designed to be **Turing incomplete**. This means it focuses on defining configurations and is not intended for general programming tasks. However, since it's a subset of TypeScript, you can still leverage the TypeScript development toolkit for features like language services.

## Project Status

This is a work in progress. **The current implementation is a proof of concept**, and the feature is incomplete. The goal is to provide a simple and easy-to-use configuration language that can be used in various applications.

## Example

TiScript definition (it's also a TypeScript snippet):

```typescript
// editor_config.ts
const LF = "\x0A";

export const EditorConfig = {
    tabSize: 4,
    trimTrailingWhitespace: true,
    endOfLine: LF,
    encoding: "utf-8",
};
```

Currently, the only interface is a command called tiscript(1) (or `cargo run` on development).

```sh
$ cargo run ./editor_config.ts
```

the output is:

```json
{
    "editorConfig": {
        "tabSize": 4,
        "trimTrailingWhitespace": true,
        "endOfLine": "\n",
        "encoding": "utf-8"
    }
}
```

## Rust API

### Synopsis

TBD

<!--
```rust
use titys;

// integrated to Serde
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct EditorConfig {
    tabSize: i32,
    trimTrailingWhitespace: bool,
    endOfLine: String,
    encoding: String,
}

fn main() {
    let data = titys::load("./editor_config.ts").unwrap();
    let editorConfig = data.import("EditorConfig");

    println!("{:?}", editorConfig);
}
``` -->

### Description

TBD

## Features

This is a list of features in ECMA-262 that are planned or implemented:

* [x] shebang
* [x] line and block comments
* [x] [WIP] `export` (there are bugs. see `tests_fail` directory)
* [x] `export default`
* [x] `let`
* [x] `const`
* [x] `if` and `else`
* [x] `undefined` literals
* [x] `null` literals
* [x] `boolean` literals
* [x] `number` literals
* [ ] [WIP] `string` literals
* [ ] template `string` literals
* [ ] tagged template `string` literals
* [x] `bigint` literals (actually 64-bit int)
* [x] array literals
* [x] object literals
* [ ] [WIP] arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`)
* [x] comparison operators (`==`, `!=`, `===`, `!==`, `<`, `>`, `<=`, `>=`)
* [ ] increment and decrement operators (`++`, `--`)
* [ ] ternary operator (`cond ? t : f`)
* [ ] logical operators (`&&`, `||`, `!`)
* [ ] nullish coalescing operator (`??`)
* [ ] null-conditional operator (`?.`)
* [x] object spread syntax
* [x] array spread syntax
* [ ] `class` statement
* [ ] `for-of` loop
* [ ] exceptions (`Error`, `try-catch-finally`, `throw` and so on)
* [ ] temporal module
* [x] function call
* [ ] method call
* [ ] [WIP] function declaration with `function` keyword
* [ ] arrow function
* [ ] function as a first-class object
* [ ] generator function (`function*`)
* [ ] limited recursive calls of functions
* [ ] static `import`
* [ ] dynamic `import`
* [x] `Math` class methods
* [ ] `Math` class properties
* [ ] `String` class methods
* [ ] `String` instance methods
* [ ] `Number` class methods
* [ ] `Number` instance methods
* [ ] optional semicolons (ASI)

This is a list of features in TypeScript that are planned or implemented:

* [ ] `any`
* [ ] `unknown`
* [ ] `never`
* [ ] `as` type assertion
* [x] primitive type annotations
* [ ] literal type annotations
* [ ] union type annotations
* [ ] intersection type annotations
* [ ] tuple type annotations
* [ ] array type annotations
* [ ] object type annotations
* [ ] type guards
* [ ] `interface` statement
* [ ] `type` statement
* [ ] generics
* [ ] null-assertion operator (trailing `!`)

This is a list of features that won't be implemented:

* `var` declaration
* `eval` function
* `new Function()`
* `Date` class
* C-style `for` loop
* `for-in` loop
* `while` loop
* `do-while` loop
* `async` and `await`
* `symbol`
* true bigint
* `enum`
* `const enum`
* `namespace`
* unlimited recursion
* anything that meets Turing completeness


Note that **any features TiScript recognizes, but TypeScript compiler does not** are invalid, but not vice versa. This is because TiScript is a strict subset of TypeScript.

## Similar Works

* jsonnet https://jsonnet.org/
* tyson https://github.com/jetify-com/tyson

## Authors

FUJI, Goro (gfx).

This project is based on https://github.com/msakuta/ruscal, where much of the code comes from.

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
