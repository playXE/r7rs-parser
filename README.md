# r7rs-parser

Simple R7RS Scheme parser. 

# Example

```rust
use r7rs_parser::{ parser::Parser, expr::NoIntern};

const SOURCE: &'static str = r#"
#!fold-case


(Add 2 3+43i 4145125125153151351351353 3/4)
"#;

fn main() {
    let mut i = NoIntern;
    let mut parser = Parser::new(&mut i, &SOURCE, false);

    while !parser.finished() {
        match parser.parse(true) {
            Ok(expr) => {
                let s = expr.to_string(&NoIntern, false);

                println!("{}", s);
            }
            Err(e) => {
                println!("{}", e);
            }
        }
    }
}
```