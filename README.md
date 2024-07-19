# TiScript - Turing-Incomplete TypeScript as a Configuration Language [![CI](https://github.com/gfx/tiscript/actions/workflows/ci.yml/badge.svg)](https://github.com/gfx/tiscript/actions/workflows/ci.yml)

TiScript is a configuration language designed to be intuitive and easy for humans and machines. It draws inspiration from TypeScript and JSON, offering the benefits of both:

* **Readability and maintainability** for humans, similar to JSON.
* **Type safety and structure** for machines inspired by TypeScript.

TiScript is intentionally designed to be **Turing incomplete**. This means it focuses on defining configurations and is not intended for general programming tasks. However, since it's a subset of TypeScript, you can still leverage the TypeScript development toolkit for features like language services.

## Project Status

This is a work in progress. **The current implementation is MVP** and not intended to be used in production.

## Example

TiScript definition (it's 100% compatible with TypeScript):

```typescript
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

This library implements `serde`'s Deserializer.

### Synopsis

```rust
// from file
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
    println!("{:?}", editorConfig);
}
```

### Description

TBD

## Features

This is a list of features in ECMA-262 that are planned or implemented:

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
* [ ] `string` literals
* [ ] [WIP] template `string` literals
* [ ] tagged template `string` literals
* [x] `bigint` literals (actually 64-bit int)
* [x] array literals
* [x] object literals
* [ ] `typeof` operator
* [ ] [WIP] arithmetic operators (`+`, `-`, `*`, `/`, `%`, `**`)
* [x] bitwise operators (`~`, `&`, `|`, `^`, `<<`, `>>`, `>>>`)
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
* [ ] C-style `for` (with restrictions)
* [ ] `while` loop (with restrictions)
* [ ] `do-while` loop (with restrictions)
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
* [ ] `satisfies` specifier
* [x] primitive type annotations
* [ ] literal type annotations
* [ ] union type annotations
* [ ] intersection type annotations
* [ ] tuple type annotations
* [ ] array type annotations
* [ ] [WIP] object type annotations
* [ ] type guards
* [ ] `interface` statement
* [ ] `type` statement
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

## Similar Works

* jsonnet https://jsonnet.org/
* tyson https://github.com/jetify-com/tyson

## Authors

FUJI, Goro (gfx).

This project is based on https://github.com/msakuta/ruscal, where much of the code comes from.

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
