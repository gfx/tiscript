# TiTys - Turing-Incomplete TypeScript as a Configuration Language

TiTys is a configuration language designed to be intuitive and easy to work with for both humans and machines. It draws inspiration from TypeScript and JSON, offering the benefits of both:

* **Readability and maintainability** for humans, similar to JSON.
* **Type safety and structure** for machines, inspired by TypeScript.

TiTys is intentionally designed to be **Turing incomplete**. This means it's focused on defining configurations and not intended for general programming tasks. However, since it's a subset of TypeScript, you can still leverage the TypeScript development toolkit for features like language services.

## Project Status

This is a work in progress. **The current implementation is a proof of concept** and not yet feature complete. The goal is to provide a simple and easy-to-use configuration language that can be used in a variety of applications.

## Example

TiTys definition:

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

Right now, the only interface is a command called titis(1) (or `cargo run` on development).

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

TBD

<!--
Use the data in Rust:

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
```
-->

## Authors

FUJI, Goro (gfx).

This project is based on https://github.com/msakuta/ruscal, where much of code comes from.

## License

This project is licensed under the ISC License - see the [LICENSE](LICENSE) file for details.
