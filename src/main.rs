use std::collections::HashMap;
use structopt::StructOpt;

type Line = i32;
type Column = i32;

type Location = ((Line, Column), (Line, Column));

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

        pub fn value(&self) -> String {
            match &self {
                Token::Identifier { loc: _, value } => value.clone(),
                Token::Equals { loc: _ } => String::from("="),
                Token::Number { loc: _, value } => value.clone(),
                Token::SemiColon { loc: _ } => String::from(";"),
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

mod parser {
    use crate::lexer::Token;
    use crate::Location;
    use std::collections::HashMap;
    /*
    <start> ::= <statement> ;

    <statement> ::= <declaration> ";" | <declaration> ";\n" <start> ;

    <declaration> ::= "let " <identifier> " = " <expression> ;

    <expression> ::= <identifier> | <number>;

    <identifier> ::= "a" | "b" | "c" | "d" ;

    <number> ::= "1" | "2" | "3" | "4" ;

    */

    type ScopeId = i32;

    #[derive(Debug, PartialEq)]
    enum Scope<T> {
        Global {
            bindings: HashMap<String, T>,
        },
        Local {
            parent: ScopeId,
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

    // @idea NodeId<T> where T is reference type
    type NodeId = i32;

    #[derive(Debug, PartialEq)]
    struct Number {
        uid: NodeId,
        loc: Location,
        value: String,
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
        statements: Vec<Statement>,
        scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    struct Declaration {
        uid: NodeId,
        loc: Location,
        id: NodeId,
        value: Expression,
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

    struct ParseResult {
        scopes: HashMap<ScopeId, Scope<NodeId>>,
        numbers: HashMap<NodeId, Number>,
        identifiers: HashMap<NodeId, Identifier>,
        declarations: HashMap<NodeId, Declaration>,
        program: Program,
    }

    struct Env {
        next_id: i32,
        location: Location,
        // current token index
        token: usize,
        // all tokens
        tokens: Vec<Token>,
        // current scope
        scope: ScopeId,
        // all scopes
        scopes: HashMap<ScopeId, Scope<NodeId>>,
        numbers: HashMap<NodeId, Number>,
        identifiers: HashMap<NodeId, Identifier>,
        declarations: HashMap<NodeId, Declaration>,
    }

    impl Env {
        fn new(tokens: Vec<Token>) -> Env {
            let mut scopes = HashMap::new();
            scopes.insert(
                1,
                Scope::Global {
                    bindings: HashMap::new(),
                },
            );
            Env {
                next_id: 2,
                token: 0,
                tokens,
                location: ((1, 1), (1, 1)),
                scope: 1,
                scopes,
                numbers: HashMap::new(),
                identifiers: HashMap::new(),
                declarations: HashMap::new(),
            }
        }

        fn uid(&mut self) -> i32 {
            let id = self.next_id;
            self.next_id = id + 1;
            id
        }

        fn next_token(&mut self) -> Option<&Token> {
            match self.tokens.get(self.token) {
                Some(t) => {
                    self.token = self.token + 1;
                    Some(t)
                }
                None => None,
            }
        }

        fn peek_token(&self) -> Option<&Token> {
            self.tokens.get(self.token)
        }

        fn location(&self) -> Location {
            self.location.clone()
        }

        fn get_scope(&self, id: ScopeId) -> &Scope<NodeId> {
            match self.scopes.get(&id) {
                Some(s) => s,
                None => panic!(format!("Internal error: cannot find scope with id {}", id)),
            }
        }

        fn lookup_scope(&mut self, value: &String) -> Option<ScopeId> {
            let mut current_scope_id = self.scope;
            loop {
                let current_scope = self.get_scope(current_scope_id);
                if let Some(_) = current_scope.bindings().get(value) {
                    break Some(current_scope_id);
                }
                match current_scope {
                    Scope::Global { bindings: _ } => break None,
                    Scope::Local {
                        parent,
                        bindings: _,
                    } => {
                        current_scope_id = *parent;
                    }
                }
            }
        }
    }

    fn parse_number(env: &mut Env) -> Result<NodeId, ParseFailure> {
        match env.next_token() {
            Some(Token::Number { loc, value }) => {
                let result = Number {
                    loc: *loc,
                    value: value.to_string(),
                    uid: env.uid(),
                };
                let uid = result.uid;
                env.numbers.insert(uid, result);
                Ok(uid)
            }
            Some(t) => Err(ParseFailure {
                loc: t.location(),
                message: format!("Expected a number but found '{}'", t.value()),
            }),
            None => Err(ParseFailure {
                loc: env.location(),
                message: format!("Expected a number after here"),
            }),
        }
    }

    fn parse_identifier(env: &mut Env) -> Result<NodeId, ParseFailure> {
        let res = match env.next_token() {
            Some(Token::Identifier { loc, value }) => Ok(Identifier {
                loc: *loc,
                value: value.to_string(),
                uid: env.uid(),
                scope: env.scope,
            }),
            Some(t) => Err(ParseFailure {
                loc: t.location(),
                message: format!("Expected an identifier but found '{}'", t.value()),
            }),
            None => Err(ParseFailure {
                loc: env.location(),
                message: format!("Expected an identifier after here"),
            }),
        };
        res.and_then(|id| match env.lookup_scope(&id.value) {
            Some(_) => {
                let uid = id.uid;
                env.identifiers.insert(uid, id);
                Ok(uid)
            }
            None => Err(ParseFailure {
                loc: env.location(),
                message: format!("Identifier '{}' must be declared before use", id.value),
            }),
        })
    }

    fn parse(tokens: Vec<Token>) -> Result<ParseResult, ParseFailure> {
        let mut env = Env::new(tokens);
        let scope = env.scope;
        let uid = env.uid();
        // parse everything
        match parse_number(&mut env) {
            Ok(node_id) => {
                let (_, end) = env.location();
                Ok(ParseResult {
                    scopes: env.scopes,
                    numbers: env.numbers,
                    identifiers: env.identifiers,
                    declarations: env.declarations,
                    program: Program {
                        uid,
                        scope,
                        loc: ((1, 1), end),
                        statements: vec![Statement::Declaration(node_id)],
                    },
                })
            }
            Err(e) => Err(e),
        }
    }
}

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
