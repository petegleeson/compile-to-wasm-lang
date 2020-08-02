use std::collections::HashMap;
use structopt::StructOpt;

type Line = i32;
type Column = i32;

type Location = ((Line, Column), (Line, Column));

#[derive(Debug, PartialEq)]
enum Scope<T> {
    Global {
        bindings: HashMap<String, T>,
    },
    Local {
        parent: Box<Scope<T>>,
        bindings: HashMap<String, T>,
    },
}

impl<T> Scope<T> {
    pub fn bindings(&self) -> &HashMap<String, T> {
        match &self {
            Scope::Global { bindings } => bindings,
            Scope::Local {
                bindings,
                parent: _,
            } => bindings,
        }
    }
}

mod lexer {
    use crate::Location;

    #[derive(Debug, PartialEq)]
    struct LexFailure {
        loc: Location,
        message: String,
    }

    #[derive(Debug, PartialEq)]
    pub enum Token {
        Identifier { loc: Location, value: String },
        Equals { loc: Location },
        Number { loc: Location, value: String },
        SemiColon { loc: Location },
    }

    impl Token {
        pub fn location(&self) -> Location {
            match &self {
                Token::Identifier { loc, value: _ } => *loc,
                Token::Equals { loc } => *loc,
                Token::Number { loc, value: _ } => *loc,
                Token::SemiColon { loc } => *loc,
            }
        }
    }

    fn lex(content: &str) -> Result<Vec<Token>, LexFailure> {
        let mut error: Option<LexFailure> = None;
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
                'A'..='Z' | 'a'..='z' => match pending {
                    Some(Token::Identifier {
                        loc: (start, _),
                        value,
                    }) => {
                        pending = Some(Token::Identifier {
                            value: format!("{}{}", value, c),
                            loc: (start, (line, col)),
                        })
                    }
                    Some(token) => {
                        pending = Some(Token::Identifier {
                            value: c.to_string(),
                            loc: ((line, col), (line, col)),
                        });
                        tokens.push(token)
                    }
                    None => {
                        pending = Some(Token::Identifier {
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
                '=' => match pending {
                    Some(token) => {
                        tokens.push(token);
                        pending = None;
                        tokens.push(Token::Equals {
                            loc: ((line, col), (line, col)),
                        })
                    }
                    None => tokens.push(Token::Equals {
                        loc: ((line, col), (line, col)),
                    }),
                },
                ' ' => match pending {
                    Some(token) => {
                        tokens.push(token);
                        pending = None;
                    }
                    None => {}
                },
                x => {
                    error = Some(LexFailure {
                        loc: ((line, col), (line, col)),
                        message: format!("Unexpected token '{}'", x),
                    })
                }
            }
            // adjust line and col
            match c {
                '\n' => {
                    line = line + 1;
                    col = 1;
                }
                _ => {
                    col = col + 1;
                }
            }
            // stop if there's a lexing error
            if let Some(_) = error {
                break;
            }
        }
        match error {
            Some(e) => Err(e),
            None => Ok(tokens),
        }
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

    #[test]
    fn it_lexs_characters() {
        match lex("aBc;") {
            Ok(tokens) => assert_eq!(
                tokens,
                [
                    Token::Identifier {
                        loc: ((1, 1), (1, 3)),
                        value: String::from("aBc")
                    },
                    Token::SemiColon {
                        loc: ((1, 4), (1, 4))
                    }
                ]
            ),
            Err(failure) => panic!(failure.message),
        }
    }

    #[test]
    fn it_lexs_statement() {
        match lex("let x = 12;") {
            Ok(tokens) => assert_eq!(
                tokens,
                [
                    Token::Identifier {
                        loc: ((1, 1), (1, 3)),
                        value: String::from("let")
                    },
                    Token::Identifier {
                        loc: ((1, 5), (1, 5)),
                        value: String::from("x")
                    },
                    Token::Equals {
                        loc: ((1, 7), (1, 7)),
                    },
                    Token::Number {
                        loc: ((1, 9), (1, 10)),
                        value: String::from("12")
                    },
                    Token::SemiColon {
                        loc: ((1, 11), (1, 11))
                    }
                ]
            ),
            Err(failure) => panic!(failure.message),
        }
    }

    #[test]
    fn it_errors_on_unexpected_token() {
        match lex("1^;") {
            Ok(_) => panic!("Expected failure"),
            Err(failure) => assert_eq!(
                failure,
                LexFailure {
                    loc: ((1, 2), (1, 2)),
                    message: String::from("Unexpected token '^'")
                }
            ),
        }
    }
}

/*
mod parser {
    use crate::lexer::Token;
    use crate::Location;
    use crate::Scope as S;
    use std::collections::HashMap;
    use std::iter::Iterator;
    /*
    <start> ::= <statement> ;

    <statement> ::= <declaration> ";" | <declaration> ";\n" <start> ;

    <declaration> ::= "let " <identifier> " = " <expression> ;

    <expression> ::= <identifier> | <number>;

    <identifier> ::= "a" | "b" | "c" | "d" ;

    <number> ::= "1" | "2" | "3" | "4" ;

    */

    // @idea NodeId<T> where T is reference type
    type NodeId = i32;
    type ScopeId = i32;

    type Scope = S<NodeId>;

    #[derive(Debug, PartialEq)]
    struct Number {
        uid: NodeId,
        loc: Location,
        value: String,
        scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    struct Identifier {
        uid: NodeId,
        loc: Location,
        value: String,
        scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    struct Program {
        uid: NodeId,
        loc: Location,
        statements: Vec<NodeId>,
        scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    struct Declaration {
        uid: NodeId,
        loc: Location,
        id: NodeId,
        value: NodeId,
        scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    enum Statement {
        Declaration(NodeId),
    }

    #[derive(Debug, PartialEq)]
    enum Expression {
        Number(NodeId),
        Identifier(NodeId),
    }

    #[derive(Debug, PartialEq)]
    struct ParseFailure {
        loc: Location,
        message: String,
    }

    struct Env {
        nextId: i32,
        scopes: HashMap<ScopeId, Scope>,
        numbers: HashMap<NodeId, Number>,
        identifiers: HashMap<NodeId, Identifier>,
        declarations: HashMap<NodeId, Declaration>,
        statements: HashMap<NodeId, Statement>,
        expressions: HashMap<NodeId, Expression>,
        program: Program,
    }

    impl Env {
        fn uid(&mut self) -> i32 {
            let id = self.nextId;
            self.nextId = id + 1;
            id
        }

        fn lookup(&self, value: &String) -> Option<ScopeId> {
            self.scopes
                .iter()
                .find_map(|(&k, &v)| match v.bindings().get(value) {
                    Some(_) => Some(k),
                    None => None,
                })
        }
    }

    fn parse_identifier<'a>(
        mut tokens: impl Iterator<Item = &'a Token>,
        mut env: Env,
    ) -> Result<NodeId, ParseFailure> {
        match tokens.next() {
            Some(Token::Identifier { loc, value }) => match env.lookup(value) {
                Some(scope) => {
                    let uid = env.uid();
                    let result = Identifier {
                        uid,
                        loc: *loc,
                        value: value.clone(),
                        scope,
                    };
                    env.identifiers.insert(uid, result);
                    Ok(uid)
                }
            },
        }
    }

    fn parse_declaration<'a>(
        mut tokens: impl Iterator<Item = &'a Token>,
        mut env: Env,
    ) -> Result<NodeId, ParseFailure> {
        let result = Declaration {
            loc: ((1, 1), (1, 1)),
            uid: env.uid(),
        };
        env.declarations.insert(result.uid, result);
        Ok(result.uid)
    }

    fn parse_statement<'a>(
        mut tokens: impl Iterator<Item = &'a Token>,
        mut env: Env,
    ) -> Result<Statement, ParseFailure> {
        match tokens.next() {
            Some(Token::Identifier { loc, value }) if value == "let" => {
                parse_declaration(tokens, env).map(|d| Statement::Declaration(d))
            }
            Some(token) => Err(ParseFailure {
                message: String::from("Syntax error"),
                loc: token.location(),
            }),
        }
    }

    fn parse(tokens: Vec<Token>) -> Result<Env, ParseFailure> {
        let mut scopes = HashMap::new();
        scopes.insert(
            1,
            Scope::Global {
                bindings: HashMap::new(),
            },
        );
        let mut env = Env {
            nextId: 1,
            scopes,
            numbers: HashMap::new(),
            identifiers: HashMap::new(),
            declarations: HashMap::new(),
            statements: HashMap::new(),
            expressions: HashMap::new(),
            // @Fix need to construct this after traversal
            program: Program {
                uid: 1,
                loc: ((1, 1), (1, 1)),
                statements: Vec::new(),
                scope: 1,
            },
        };
        let mut iter = tokens.iter();
        let stmt = parse_statement(
            iter.take_while(|t| match t {
                Token::SemiColon { loc: _ } => true,
                _ => false,
            }),
            env,
        );

        Ok(env)
    }

    #[test]
    fn it_parses_statement() {
        match parse(Vec::from([
            Token::Identifier {
                loc: ((1, 1), (1, 3)),
                value: String::from("let"),
            },
            Token::Identifier {
                loc: ((1, 5), (1, 5)),
                value: String::from("x"),
            },
            Token::Equals {
                loc: ((1, 7), (1, 7)),
            },
            Token::Number {
                loc: ((1, 9), (1, 10)),
                value: String::from("12"),
            },
            Token::SemiColon {
                loc: ((1, 11), (1, 11)),
            },
        ])) {
            Ok(program) => {
                let scope = Scope::Global {
                    bindings: HashMap::new(),
                };
                // let mut bindings = HashMap::new();
                // let y = scope.bindings();
                // .insert(String::from("x"), x);

                assert_eq!(
                    program,
                    Program {
                        loc: ((1, 1), (1, 11)),
                        scope: &scope,
                        statements: Vec::from([Statement::Declaration(Declaration {
                            loc: ((1, 1), (1, 12)),
                            id: Identifier {
                                loc: ((1, 5), (1, 5)),
                                value: String::from("x"),
                                scope: &scope,
                            },
                            value: Expression::Number(Number {
                                loc: ((1, 9), (1, 10)),
                                value: String::from("12"),
                                scope: &scope
                            }),
                            scope: &scope
                        })]),
                    }
                )
            }
            Err(failure) => panic!(failure.message),
        }
    }
}
*/

#[derive(Debug, StructOpt)]
struct Cli {
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

fn main() {
    // let args = Cli::from_args();
    // println!("{:#?}", args);
    println!("Hello, world!");
}
