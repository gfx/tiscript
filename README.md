# TiTyS - Tiny Subset of TypeScript for Configuration

TiTyS is a configuration language that is designed to be easy to read and write for humans and easy to parse for machines. It is inspired by TypeScript and JSON. In fact, it is a subset of TypeScript and generates JSON-like data for configuration.

This is not a Turing-complete language. It has no loop in any ways, even if the language itself is a TypeScript variant and can use all of the development toolkit of TypeScript such as the language services.

## Example

TiTyS definition:

```ts
// edotor_config.ts
const LF = "\x0A";
const CR = "\x0D";
const CRLF = `${CR}${LF}`;

export EditorConfig = {
    tabSize: 4,
    trimTrailingWhitespace: true,
    endOfLine: LF,
    encoding: "utf-8",
};
```

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
