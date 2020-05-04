# json-rs

practice rust.

## Example

value which will be parsed from json is as followings.

```rust
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}
```

And, the followings is example.

```rust
use json_rs::{from_str, Value};

fn main() {
    let data = r#"
    {
        "name": "John Doe",
        "age": 43,
        "phones": [
            "+81 4444",
            "+81 4444"
        ]
    }
    "#;

    let v: Value = from_str(data.to_string()).unwrap_or(Value::Null);

    println!("Please call {} at the number {}", v["name"], v["phones"][0]);
}
```

## License

MIT

## Author

Ryo Kitagawa
