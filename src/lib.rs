use std::collections::HashMap;
use std::fmt;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Loc(usize, usize);

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}-{}", self.0, self.1)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Annot<T> {
    value: T,
    loc: Loc,
}

impl<T> Annot<T> {
    fn new(value: T, loc: Loc) -> Self {
        Self { value, loc }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ParseErrorType {
    UnexpectedString(String),
    UnexpectedChar(char),
    NotFound(char),
    EOF,
}

pub type ParseError = Annot<ParseErrorType>;

impl ParseError {
    fn unexpected_string(expected: String, loc: Loc) -> Self {
        ParseError::new(ParseErrorType::UnexpectedString(expected), loc)
    }

    fn unexpected_char(expected: char, loc: Loc) -> Self {
        ParseError::new(ParseErrorType::UnexpectedChar(expected), loc)
    }

    fn not_found(expected: char, loc: Loc) -> Self {
        ParseError::new(ParseErrorType::NotFound(expected), loc)
    }

    fn eof(loc: Loc) -> Self {
        ParseError::new(ParseErrorType::EOF, loc)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Number {
    Int(i128),
    Float(f64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Null,
    Bool(bool),
    Number(Number),
    String(String),
    Array(Vec<Value>),
    Object(HashMap<String, Value>),
}

pub fn from_str(data: String) -> Result<Value, ParseError> {
    let bytes = data.as_bytes();

    match read_single(bytes, 0) {
        Ok((v, _)) => Ok(v),
        Err(e) => Err(e),
    }
}

fn skip_space(data: &[u8], mut pos: usize) -> usize {
    while pos < data.len() && b" \n\t".contains(&data[pos]) {
        pos += 1;
    }
    pos
}

fn read_single(data: &[u8], pos: usize) -> Result<(Value, usize), ParseError> {
    match data[pos] {
        b'n' => read_constant(data, pos, "null".to_string(), Value::Null),
        b't' => read_constant(data, pos, "true".to_string(), Value::Bool(true)),
        b'f' => read_constant(data, pos, "false".to_string(), Value::Bool(false)),
        b'0'..=b'9' => read_number(data, pos),
        b'"' => read_string(data, pos),
        b'[' => read_array(data, pos),
        b'{' => read_obj(data, pos),
        c => Err(ParseError::unexpected_char(c.into(), Loc(pos, pos + 1))),
    }
}

fn read_constant(
    data: &[u8],
    pos: usize,
    constant: String,
    constant_value: Value,
) -> Result<(Value, usize), ParseError> {
    use std::str::from_utf8;

    if from_utf8(&data[pos..pos + constant.len()]).unwrap() == constant {
        return Ok((constant_value, pos + constant.len()));
    }
    let len = constant.len();
    Err(ParseError::unexpected_string(constant, Loc(pos, pos + len)))
}

fn read_string(data: &[u8], pos: usize) -> Result<(Value, usize), ParseError> {
    use std::str::from_utf8;

    if data[pos] != b'"' {
        return Err(ParseError::not_found('"', Loc(pos, pos + 1)));
    }

    let mut cur: usize = pos + 1;
    while data.len() > cur && !b"\"".contains(&data[cur]) {
        cur += 1;
    }

    if data.len() > cur {
        Ok((
            Value::String(from_utf8(&data[pos + 1..cur]).unwrap().to_string()),
            cur + 1,
        ))
    } else {
        Err(ParseError::not_found('"', Loc(pos, cur)))
    }
}

fn read_number(bytes: &[u8], pos: usize) -> Result<(Value, usize), ParseError> {
    use std::str::from_utf8;

    let mut last_position = pos;
    while last_position < bytes.len() && b"0123456789".contains(&bytes[last_position]) {
        last_position += 1;
    }

    if last_position < bytes.len() && b".".contains(&bytes[last_position]) {
        last_position += 1;
        while last_position < bytes.len() && b"0123456789".contains(&bytes[last_position]) {
            last_position += 1;
        }
        let f: f64 = from_utf8(&bytes[pos..last_position])
            .unwrap()
            .parse()
            .unwrap();
        Ok((Value::Number(Number::Float(f)), last_position))
    } else {
        let i: i128 = from_utf8(&bytes[pos..last_position])
            .unwrap()
            .parse()
            .unwrap();
        Ok((Value::Number(Number::Int(i)), last_position))
    }
}

fn read_array(data: &[u8], pos: usize) -> Result<(Value, usize), ParseError> {
    let mut contents: Vec<Value> = vec![];
    let mut cur = skip_space(data, pos + 1);
    if data[cur] == b']' {
        return Ok((Value::Array(contents), cur));
    }

    let (content, c) = read_single(data, cur)?;
    cur = c;
    contents.push(content);

    while data.len() > cur && data[cur] == b',' {
        cur = skip_space(data, cur + 1);
        let (content, c) = read_single(data, cur)?;
        cur = c;
        contents.push(content);
        cur = skip_space(data, cur);
    }

    if data.len() <= cur || data[cur] != b']' {
        return Err(ParseError::not_found(']', Loc(pos, cur)));
    }

    Ok((Value::Array(contents), cur + 1))
}

fn read_single_obj(data: &[u8], pos: usize) -> Result<(String, Value, usize), ParseError> {
    if let (Value::String(s), mut cur) = read_string(data, pos).unwrap() {
        if data[cur] != b':' {
            return Err(ParseError::unexpected_char(':', Loc(cur, cur + 1)));
        }
        cur = skip_space(data, cur + 1);
        let (obj, cur) = read_string(data, cur).unwrap();
        return Ok((s, obj, cur));
    }
    Err(ParseError::eof(Loc(pos, pos + 1)))
}

fn read_obj(data: &[u8], pos: usize) -> Result<(Value, usize), ParseError> {
    let mut hash_map = HashMap::new();
    if data[pos] != b'{' {
        return Err(ParseError::not_found('{', Loc(pos, pos + 1)));
    }

    let (s, v, mut cur) = read_single_obj(data, pos + 1).unwrap();
    hash_map.insert(s, v);
    cur = skip_space(data, cur);

    while data.len() > cur && data[cur] == b',' {
        let (s, v, c) = read_single_obj(data, cur + 1).unwrap();
        hash_map.insert(s, v);
        cur = skip_space(data, c);
    }

    if cur >= data.len() || data[cur] != b'}' {
        return Err(ParseError::not_found('}', Loc(pos, cur)));
    }

    Ok((Value::Object(hash_map), cur + 1))
}

#[cfg(test)]
mod tests {
    use super::{from_str, Loc, Number, ParseError, Value};

    #[test]
    fn from_str_test() {
        struct Test {
            input: String,
            output: Value,
        }

        let tests = vec![
            Test {
                input: "null".to_string(),
                output: Value::Null,
            },
            Test {
                input: "true".to_string(),
                output: Value::Bool(true),
            },
            Test {
                input: "false".to_string(),
                output: Value::Bool(false),
            },
            Test {
                input: "1".to_string(),
                output: Value::Number(Number::Int(1)),
            },
            Test {
                input: "1.".to_string(),
                output: Value::Number(Number::Float(1.)),
            },
            Test {
                input: "1.3".to_string(),
                output: Value::Number(Number::Float(1.3)),
            },
            Test {
                input: "\"hello\"".to_string(),
                output: Value::String("hello".to_string()),
            },
            Test {
                input: "[]".to_string(),
                output: Value::Array(vec![]),
            },
            Test {
                input: "[1]".to_string(),
                output: Value::Array(vec![Value::Number(Number::Int(1))]),
            },
            Test {
                input: "[1, 2., true, null, \"hello\"]".to_string(),
                output: Value::Array(vec![
                    Value::Number(Number::Int(1)),
                    Value::Number(Number::Float(2.)),
                    Value::Bool(true),
                    Value::Null,
                    Value::String("hello".to_string()),
                ]),
            },
            Test {
                input: "{\"hello\": \"world\"}".to_string(),
                output: Value::Object(
                    [("hello".to_string(), Value::String("world".to_string()))]
                        .iter()
                        .cloned()
                        .collect(),
                ),
            },
            Test {
                input: "[{\"hello\": \"world\"}, {\"konnichiwa\": \"sekai\"}]".to_string(),
                output: Value::Array(vec![
                    Value::Object(
                        [("hello".to_string(), Value::String("world".to_string()))]
                            .iter()
                            .cloned()
                            .collect(),
                    ),
                    Value::Object(
                        [("konnichiwa".to_string(), Value::String("sekai".to_string()))]
                            .iter()
                            .cloned()
                            .collect(),
                    ),
                ]),
            },
        ];

        for test in tests {
            assert_eq!(from_str(test.input), Ok(test.output))
        }
    }

    #[test]
    fn from_str_error_test() {
        struct Test {
            input: String,
            output: ParseError,
        }

        let tests = vec![
            Test {
                input: "none".to_string(),
                output: ParseError::unexpected_string("null".to_string(), Loc(0, 4)),
            },
            Test {
                input: "\"hello".to_string(),
                output: ParseError::not_found('"', Loc(0, 6)),
            },
            Test {
                input: "[\"hello\"".to_string(),
                output: ParseError::not_found(']', Loc(0, 8)),
            },
        ];

        for test in tests {
            assert_eq!(from_str(test.input), Err(test.output))
        }
    }
}
