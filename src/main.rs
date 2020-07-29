use structopt::StructOpt;

mod lexer {
    type Location = ((i32, i32), (i32, i32));

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
        let mut tokens = Vec::new();
        let mut pending: Option<Token> = None;
        let mut line = 1;
        let mut col = 1;
        for c in content.chars() {
            // depending on pending and token
            // set pending and tokens
            match c {
                '0'..='9' => match pending {
                    Some(Token::Number {
                        loc: (start, _),
                        value,
                    }) => {
                        pending = Some(Token::Number {
                            value: format!("{}{}", value, c),
                            loc: (start, (line, col)),
                        })
                    }
                    Some(token) => {
                        pending = Some(Token::Number {
                            value: c.to_string(),
                            loc: ((line, col), (line, col)),
                        });
                        tokens.push(token)
                    }
                    None => {
                        pending = Some(Token::Number {
                            value: c.to_string(),
                            loc: ((line, col), (line, col)),
                        })
                    }
                },
                ';' => match pending {
                    Some(token) => {
                        tokens.push(token);
                        pending = None;
                        tokens.push(Token::SemiColon {
                            loc: ((line, col), (line, col)),
                        })
                    }
                    None => tokens.push(Token::SemiColon {
                        loc: ((line, col), (line, col)),
                    }),
                },
                _ => panic!("unexpected token"),
            }
            // adjust line and col
            match c {
                // @TODO - fix for windows line endings
                '\n' => {
                    line = line + 1;
                    col = 1;
                }
                _ => {
                    col = col + 1;
                }
            }
        }
        Ok(tokens)
    }

    #[test]
    fn it_lexs_number() {
        match lex("1;") {
            Ok(tokens) => assert_eq!(
                tokens,
                [
                    Token::Number {
                        loc: ((1, 1), (1, 1)),
                        value: String::from("1")
                    },
                    Token::SemiColon {
                        loc: ((1, 2), (1, 2))
                    }
                ]
            ),
            Err(failure) => panic!(failure.message),
        }
    }

    #[test]
    fn it_lexs_many_numbers() {
        match lex("420;") {
            Ok(tokens) => assert_eq!(
                tokens,
                [
                    Token::Number {
                        loc: ((1, 1), (1, 3)),
                        value: String::from("420")
                    },
                    Token::SemiColon {
                        loc: ((1, 4), (1, 4))
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
