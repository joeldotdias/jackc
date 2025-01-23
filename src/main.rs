use std::{env, fs, path::PathBuf};

use lexer::Lexer;
use parser::Parser;

pub mod lexer;
pub mod parser;

fn main() {
    let filename = env::args().nth(1).unwrap_or("test.jack".to_owned());
    let filepath = PathBuf::from(filename);
    assert!(
        filepath.extension().and_then(|ext| ext.to_str()) == Some("jack"),
        "Expected .jack file"
    );
    let source = fs::read_to_string(&filepath).unwrap();
    let lexer = Lexer::new(&source);

    /* lexer.for_each(|tok| match tok {
        Ok(t) => println!("{:?}", t),
        Err(e) => println!("Error: {}", e),
    }); */

    let mut parser = Parser::new(&source);
    let ast = parser.parse_class();
    match ast {
        Ok(tt) => println!("{:#?}", tt),
        Err(e) => println!("{}", e),
    }
}
