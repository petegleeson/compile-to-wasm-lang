use structopt::StructOpt;

mod lexer {
    #[derive(Debug, PartialEq)]
    struct Location {
        line: i32,
        col: i32,
    }

    #[derive(Debug)]
    struct LexFailure {
        loc: Location,
        message: String,
    }

    #[derive(Debug, PartialEq)]
    enum Token {
        Number { loc: Location, value: String },
        SemiColon { loc: Location },
    }

    fn lex(content: &str) -> Result<Vec<Token>, LexFailure> {
        Ok(Vec::new())
    }

    #[test]
    fn it_lexs_number() {
        match lex("1;") {
            Ok(tokens) => assert_eq!(
                tokens,
                [
                    Token::Number {
                        loc: Location { line: 1, col: 1 },
                        value: String::from("1")
                    },
                    Token::SemiColon {
                        loc: Location { line: 1, col: 2 }
                    }
                ]
            ),
            Err(failure) => panic!(failure.message),
        }
    }
}

#[derive(Debug, StructOpt)]
struct Cli {
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() {
    let args = Cli::from_args();
    println!("{:#?}", args);
    println!("Hello, world!");
}
