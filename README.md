# TiScript - Turing-Incomplete TypeScript as a Configuration Language [![crates.io](https://img.shields.io/crates/v/tiscript.svg)](https://crates.io/crates/tiscript) [![CI](https://github.com/gfx/tiscript/actions/workflows/ci.yml/badge.svg)](https://github.com/gfx/tiscript/actions/workflows/ci.yml)

TiScript is a configuration language designed to be intuitive and easy for humans and machines. It draws inspiration from TypeScript and JSON, offering the benefits of both:

* **Readability and maintainability** for humans, similar to JSON.
* **Type safety and structure** for machines inspired by TypeScript.

TiScript is intentionally designed to be **Turing incomplete**. This means it focuses on defining configurations and is not intended for general programming tasks. However, since it's a subset of TypeScript, you can still leverage the TypeScript development toolkit for features like language services.

## Project Status

This is a work in progress. **The current implementation is MVP** and not intended to be used in production.

## Example

TiScript definition (it's strict subset of TypeScript):

```ts
// editor_config.ts
const LF = "\x0A";

export const tabSize = 4;
export const trimTrailingWhitespace = true;
export const endOfLine = LF;
export const encoding = "utf-8";
```

Currently, the only interface is a command called tiscript(1) (or `cargo run` on development).

```sh
$ cargo run ./editor_config.ts
```

the output is:

```json
{
    "tabSize": 4,
    "trimTrailingWhitespace": true,
    "endOfLine": "\n",
    "encoding": "utf-8"
}
```

## Rust API

This library implements [serde](https://serde.rs/)'s Deserializer.

### Synopsis

From a file:

```rust
use serde::{Deserialize, Serialize};

use tiscript::from_file;

// integrated to Serde
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct EditorConfig {
    tabSize: i32,
    trimTrailingWhitespace: bool,
    endOfLine: String,
    encoding: String,
}

fn main() {
    let editorConfig: EditorConfig = from_file("./editor_config.ts").unwrap();
    // or from_file_with_timeout(f, d) for untrusted code

    println!("{:?}", editorConfig);
}
```

From an inline code:

```rust
use serde::{Deserialize, Serialize};

use tiscript::from_str;

// integrated to Serde
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct EditorConfig {
    tabSize: i32,
    trimTrailingWhitespace: bool,
    endOfLine: String,
    encoding: String,
}

fn main() {
    let editorConfig: EditorConfig = from_str(r#"
        const LF = "\x0A";

        export const tabSize = 4;
        export const trimTrailingWhitespace = true;
        export const endOfLine = LF;
        export const encoding = "utf-8";
    "#).unwrap();
    // or from_str_with_timeout(f, d) for untrusted code

println!("{:?}", editorConfig);
}
```

### Description

TBD

## Features

This is a list of features in ECMA-262 that are planned or implemented ("[x]" does not necessarily mean it's 100% compatible with TypeScript and ECMA-262):

* [x] shebang
* [x] line and block comments
* [x] `export`
* [x] `export default`
* [x] `let`
* [x] `const`
* [x] `if` and `else`
* [x] `undefined` literals
* [x] `null` literals
* [x] `boolean` literals
* [x] `number` literals
* [x] `string` literals
* [x] template `string` literals
* [ ] tagged template `string` literals
* [x] `bigint` literals (actually 64-bit int)
* [x] array literals
* [x] object literals
* [ ] index access (`[0]` and `.["foo"]`)
* [ ] property access (`.foo`)
* [ ] `typeof` operator
* [x] arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`)
* [x] bitwise operators (`~`, `&`, `|`, `^`, `<<`, `>>`, `>>>`)
* [ ] assignment operators (`=`, `+=`, `-=`, `*=`, `/=`, `%=`, `**=`, `<<=`, `>>=`, `>>>=`, `&=`, `|=`, `^=`)
* [x] comparison operators (`==`, `!=`, `===`, `!==`, `<`, `>`, `<=`, `>=`)
* [ ] increment and decrement operators (`++`, `--`)
* [x] ternary operator (`cond ? t : f`)
* [ ] logical operators (`&&`, `||`, `!`)
* [ ] nullish coalescing operator (`??`)
* [ ] null-conditional operator (`?.`)
* [x] object spread syntax
* [x] array spread syntax
* [ ] `class` statement
* [ ] `for-of` loop
* [ ] C-style `for` (with restrictions)
* [ ] `while` loop (with restrictions)
* [ ] `do-while` loop (with restrictions)
* [ ] exceptions (`Error`, `try-catch-finally`, `throw` and so on)
* [ ] temporal module
* [x] function declaration
* [ ] arrow function
* [x] function call
* [ ] method call
* [x] function declaration with `function` keyword
* [ ] arrow function
* [ ] function as a first-class object
* [ ] generator function (`function*`)
* [ ] limited recursive calls of functions
* [ ] optional semicolons (ASI)
* [ ] static `import`
* [ ] dynamic `import`
* [x] `Math` class methods
* [ ] `Math` class properties
* [ ] `String` class methods
* [ ] `String` instance methods
* [ ] `Number` class methods
* [ ] `Number` instance methods
* [ ] `TextEncoder`
* [ ] `Intl` / ECMA-402
* [ ] `atob` and `btoa`

This is a list of features in TypeScript that are planned or implemented:

* [x] `import type` statement (but it does nothing so far)
* [ ] `any`
* [ ] `unknown`
* [ ] `never`
* [ ] `as` type assertion
* [x] `satisfies` type operator
* [x] primitive type annotations
* [ ] literal type annotations
* [ ] union type annotations
* [ ] intersection type annotations
* [ ] tuple type annotations
* [ ] array type annotations
* [ ] [WIP] object type annotations
* [ ] type guards
* [ ] `interface` type statement
* [x] `type` type statement
* [ ] `typeof` operator in type expressions
* [ ] generics
* [ ] null-assertion operator (trailing `!`)

This is a list of features that won't be implemented:

* `var` declaration
* `eval` function
* `new Function()`
* `RegExp` and regular expression operators
* Most of runtime (e.g. typed arrays)
* `for-in` loop
* `async` and `await`
* `symbol`
* true bigint
* decorators (just because the spec is too large)
* `enum`
* `const enum`
* `namespace`
* commonjs features
* no strict features
* unlimited recursion / loop
* anything that meets Turing completeness

Note that **any features TiScript recognizes, but TypeScript compiler does not** are invalid, but not vice versa. This is because TiScript is a strict subset of TypeScript.

## Development

If you'd like to develop this project, run `UPDATE=1 cargo test` to automatically generates `*.stdout` or `*.stderr` files in `spec/`.

## WebAssembly

This project is ensured to be built with `--target=wasm32-wasi` in CI. There's no test for WebAssembly though.

## Similar Works

* JSON https://www.json.org/
* Jsonnet https://jsonnet.org/
* TySON https://github.com/jetify-com/tyson

## Authors

FUJI, Goro (gfx).

This project a fork of https://github.com/msakuta/ruscal, and thus much of the code comes from it.

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
