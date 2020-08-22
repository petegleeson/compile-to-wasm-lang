use structopt::StructOpt;

type Line = i32;
type Column = i32;

type Location = ((Line, Column), (Line, Column));

trait Locatable {
    fn location(&self) -> Location;
}

#[derive(Debug, PartialEq)]
pub struct LexFailure {
    loc: Location,
    message: String,
}

#[derive(Debug, PartialEq)]
pub struct TypeCheckFailure {
    message: String,
}

#[derive(Debug, PartialEq)]
pub struct ParseFailure {
    loc: Location,
    message: String,
}

#[derive(Debug, PartialEq)]
pub struct CodeGenFailure {
    message: String,
}

#[derive(Debug, PartialEq)]
pub struct LinkFailure {
    message: String,
}

impl From<std::io::Error> for LinkFailure {
    fn from(e: std::io::Error) -> LinkFailure {
        LinkFailure {
            message: format!("Internal Link Error: {}", e),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Failure {
    Lex(LexFailure),
    Parse(ParseFailure),
    TypeCheck(TypeCheckFailure),
    CodeGen(CodeGenFailure),
    Link(LinkFailure),
}

impl Failure {
    pub fn message(&self) -> String {
        match self {
            Failure::Lex(f) => f.message.clone(),
            Failure::TypeCheck(f) => f.message.clone(),
            Failure::Parse(f) => f.message.clone(),
            Failure::CodeGen(f) => f.message.clone(),
            Failure::Link(f) => f.message.clone(),
        }
    }
}

mod lexer {
    use crate::Failure;
    use crate::LexFailure;
    use crate::Locatable;
    use crate::Location;

    #[derive(Debug, PartialEq)]
    pub enum Token {
        Identifier { loc: Location, value: String },
        Equals { loc: Location },
        Number { loc: Location, value: String },
        SemiColon { loc: Location },
        LParen { loc: Location },
        RParen { loc: Location },
        LCurly { loc: Location },
        RCurly { loc: Location },
        Comma { loc: Location },
    }

    impl Locatable for Token {
        fn location(&self) -> Location {
            match &self {
                Token::Identifier { loc, .. } => *loc,
                Token::Equals { loc } => *loc,
                Token::Number { loc, .. } => *loc,
                Token::SemiColon { loc } => *loc,
                Token::LCurly { loc } => *loc,
                Token::RCurly { loc } => *loc,
                Token::LParen { loc } => *loc,
                Token::RParen { loc } => *loc,
                Token::Comma { loc } => *loc,
            }
        }
    }

    impl Token {
        fn from(c: char, loc: Location) -> Token {
            match c {
                '=' => Token::Equals { loc },
                ';' => Token::SemiColon { loc },
                '(' => Token::LParen { loc },
                ')' => Token::RParen { loc },
                '{' => Token::LCurly { loc },
                '}' => Token::RCurly { loc },
                ',' => Token::Comma { loc },
                _ => panic!(println!("Internal error: cannot create token for {}", c)),
            }
        }
        pub fn value(&self) -> String {
            match &self {
                Token::Identifier { value, .. } => value.clone(),
                Token::Equals { .. } => String::from("="),
                Token::Number { value, .. } => value.clone(),
                Token::SemiColon { .. } => String::from(";"),
                Token::LParen { .. } => String::from("("),
                Token::RParen { .. } => String::from(")"),
                Token::LCurly { .. } => String::from("{"),
                Token::RCurly { .. } => String::from("}"),
                Token::Comma { .. } => String::from(","),
            }
        }
    }

    pub fn lex(content: &str) -> Result<Vec<Token>, Failure> {
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
                ';' | '=' | '(' | ')' | '{' | '}' | ',' => {
                    if let Some(token) = pending {
                        tokens.push(token);
                        pending = None;
                    };
                    tokens.push(Token::from(c, ((line, col), (line, col))));
                }
                ' ' | '\n' => match pending {
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
            Some(e) => Err(Failure::Lex(e)),
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
            Err(failure) => panic!(failure.message()),
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
            Err(failure) => panic!(failure.message()),
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
            Err(failure) => panic!(failure.message()),
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
            Err(failure) => panic!(failure.message()),
        }
    }

    #[test]
    fn it_errors_on_unexpected_token() {
        match lex("1^;") {
            Ok(_) => panic!("Expected failure"),
            Err(failure) => assert_eq!(
                failure,
                Failure::Lex(LexFailure {
                    loc: ((1, 2), (1, 2)),
                    message: String::from("Unexpected token '^'")
                })
            ),
        }
    }
}

mod parser {
    use crate::lexer::Token;
    use crate::Failure;
    use crate::Locatable;
    use crate::Location;
    use crate::ParseFailure;
    use std::collections::HashMap;

    /*
    <start> ::= <statement> ;

    <statement> ::= <declaration> ";" | <declaration> ";\n" <start> ;

    <declaration> ::= "let " <identifier> " = " <expression> ;

    <expression> ::= <identifier> | <number>;

    <identifier> ::= "a" | "b" | "c" | "d" ;

    <number> ::= "1" | "2" | "3" | "4" ;

    */

    pub type ScopeId = i32;

    #[derive(Debug, PartialEq)]
    pub enum Scope<T> {
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
                Scope::Local { bindings, .. } => bindings,
            }
        }

        pub fn add_binding(&mut self, id: String, v: T) -> Option<T> {
            match self {
                Scope::Global { bindings } => bindings.insert(id, v),
                Scope::Local { bindings, .. } => bindings.insert(id, v),
            }
        }
    }

    // @Idea NodeId<T> where T is reference type
    pub type NodeId = i32;

    pub trait Node {
        fn node_id(&self) -> NodeId;
    }

    #[derive(Debug, PartialEq)]
    pub struct Number {
        uid: NodeId,
        loc: Location,
        pub value: String,
    }

    impl Locatable for Number {
        fn location(&self) -> Location {
            self.loc
        }
    }

    impl Node for Number {
        fn node_id(&self) -> NodeId {
            self.uid
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Identifier {
        uid: NodeId,
        loc: Location,
        pub value: String,
        pub scope: ScopeId,
    }

    impl Locatable for Identifier {
        fn location(&self) -> Location {
            self.loc
        }
    }

    impl Node for Identifier {
        fn node_id(&self) -> NodeId {
            self.uid
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Function {
        uid: NodeId,
        loc: Location,
        pub scope: ScopeId,
        pub args: Vec<Identifier>,
        pub body: Vec<Statement>,
    }

    impl Locatable for Function {
        fn location(&self) -> Location {
            self.loc
        }
    }

    impl Node for Function {
        fn node_id(&self) -> NodeId {
            self.uid
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Program {
        uid: NodeId,
        loc: Location,
        pub statements: Vec<Statement>,
        pub scope: ScopeId,
    }

    impl Locatable for Program {
        fn location(&self) -> Location {
            self.loc
        }
    }

    impl Node for Program {
        fn node_id(&self) -> NodeId {
            self.uid
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Declaration {
        uid: NodeId,
        loc: Location,
        pub id: Identifier,
        pub value: Expression,
        pub scope: ScopeId,
    }

    impl Locatable for Declaration {
        fn location(&self) -> Location {
            self.loc
        }
    }

    impl Node for Declaration {
        fn node_id(&self) -> NodeId {
            self.uid
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Declaration(Declaration),
    }

    impl Locatable for Statement {
        fn location(&self) -> Location {
            match self {
                Statement::Declaration(d) => d.location(),
            }
        }
    }

    impl Node for Statement {
        fn node_id(&self) -> NodeId {
            match self {
                Statement::Declaration(d) => d.node_id(),
            }
        }
    }

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Number(Number),
        Identifier(Identifier),
        Function(Function),
    }

    impl Locatable for Expression {
        fn location(&self) -> Location {
            match self {
                Expression::Number(n) => n.location(),
                Expression::Identifier(id) => id.location(),
                Expression::Function(func) => func.location(),
            }
        }
    }

    impl Node for Expression {
        fn node_id(&self) -> NodeId {
            match self {
                Expression::Number(n) => n.node_id(),
                Expression::Identifier(id) => id.node_id(),
                Expression::Function(func) => func.node_id(),
            }
        }
    }

    pub type Scopes = HashMap<ScopeId, Scope<NodeId>>;

    #[derive(Debug, PartialEq)]
    pub struct ParseResult {
        pub program: Program,
        pub scopes: Scopes,
    }

    // collection of mutable data used during parse tree creation
    struct Env {
        // next uid
        next_id: i32,
        // current scope
        current_scope: ScopeId,
        // all scopes
        scopes: HashMap<ScopeId, Scope<NodeId>>,
    }

    impl Env {
        fn new(id: ScopeId, global: Scope<NodeId>) -> Env {
            Env {
                next_id: 2,
                current_scope: id,
                scopes: vec![(id, global)].into_iter().collect(),
            }
        }

        fn uid(&mut self) -> i32 {
            let id = self.next_id;
            self.next_id = id + 1;
            id
        }

        fn get_scope(&self, id: ScopeId) -> &Scope<NodeId> {
            match self.scopes.get(&id) {
                Some(s) => s,
                None => panic!(format!("Internal error: cannot find scope with id {}", id)),
            }
        }

        fn lookup_scope(&mut self, value: &String) -> Option<ScopeId> {
            let mut current_scope_id = self.current_scope;
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

        fn push_scope(&mut self) -> ScopeId {
            let scope_id = self.uid();
            self.scopes.insert(
                scope_id,
                Scope::Local {
                    parent: self.current_scope,
                    bindings: HashMap::new(),
                },
            );
            self.current_scope = scope_id;
            scope_id
        }

        fn pop_scope(&mut self) -> ScopeId {
            self.current_scope = match self.get_scope(self.current_scope) {
                Scope::Local { parent, .. } => *parent,
                _ => panic!("Internal error: tried to pop scope from global scope"),
            };
            self.current_scope
        }

        fn add_binding(&mut self, id: String, node_id: NodeId) -> Option<NodeId> {
            self.scopes
                .get_mut(&self.current_scope)
                .and_then(|scope| scope.add_binding(id, node_id))
        }
    }

    struct TokenIterator {
        // current token index
        token: usize,
        // all tokens
        tokens: Vec<Token>,
    }

    impl TokenIterator {
        fn new(tokens: Vec<Token>) -> TokenIterator {
            TokenIterator { token: 0, tokens }
        }

        fn location(&self) -> Location {
            match self.tokens.get(self.token) {
                Some(t) => t.location(),
                None => panic!("Internal error: cannot get location for token"),
            }
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
    }

    fn parse_number(tokens: &mut TokenIterator, env: &mut Env) -> Result<Number, ParseFailure> {
        let loc = tokens.location();
        match tokens.next_token() {
            Some(Token::Number { loc, value }) => Ok(Number {
                loc: *loc,
                value: value.to_string(),
                uid: env.uid(),
            }),
            Some(t) => Err(ParseFailure {
                loc,
                message: format!("Expected a number but found '{}'", t.value()),
            }),
            None => Err(ParseFailure {
                loc,
                message: format!("Expected a number after here"),
            }),
        }
    }

    fn parse_identifier(
        tokens: &mut TokenIterator,
        env: &mut Env,
    ) -> Result<Identifier, ParseFailure> {
        let loc = tokens.location();
        match tokens.next_token() {
            Some(Token::Identifier { loc, value }) => Ok(Identifier {
                loc: *loc,
                value: value.to_string(),
                uid: env.uid(),
                scope: 1,
            }),
            Some(t) => Err(ParseFailure {
                loc,
                message: format!("Expected an identifier but found '{}'", t.value()),
            }),
            None => Err(ParseFailure {
                loc,
                message: format!("Expected an identifier after here"),
            }),
        }
        .and_then(|id| match env.lookup_scope(&id.value) {
            Some(_) => Ok(id),
            None => Err(ParseFailure {
                loc: id.loc,
                message: format!("Identifier '{}' must be declared before use", id.value),
            }),
        })
    }

    fn parse_function(tokens: &mut TokenIterator, env: &mut Env) -> Result<Function, ParseFailure> {
        let (start, end) = tokens.location();
        let uid = env.uid();
        let scope = env.current_scope;
        env.push_scope();

        fn parse_args(
            function_id: i32,
            tokens: &mut TokenIterator,
            env: &mut Env,
        ) -> Result<Vec<Identifier>, ParseFailure> {
            let mut args = Vec::new();
            loop {
                match tokens.peek_token() {
                    Some(Token::LParen { .. }) => {
                        tokens.next_token();
                        continue;
                    }
                    Some(Token::RParen { .. }) => {
                        tokens.next_token();
                        break Ok(args);
                    }
                    Some(Token::Identifier { value, .. }) => {
                        env.add_binding(value.to_string(), function_id);
                        match parse_identifier(tokens, env) {
                            Ok(ident) => {
                                args.push(ident);
                                // check for more args
                                match tokens.peek_token() {
                                    Some(Token::Comma { .. }) => {
                                        tokens.next_token();
                                        continue;
                                    }
                                    _ => break Ok(args),
                                }
                            }
                            Err(e) => break Err(e),
                        }
                    }
                    _ => {
                        break Err(ParseFailure {
                            loc: tokens.location(),
                            message: format!(
                                "Expected function arguments to be comma separated Identifiers"
                            ),
                        });
                    }
                }
            }
        };

        fn parse_body(
            tokens: &mut TokenIterator,
            env: &mut Env,
        ) -> Result<Vec<Statement>, ParseFailure> {
            let mut statements = Vec::new();
            loop {
                match tokens.peek_token() {
                    Some(Token::RCurly { .. }) => {
                        tokens.next_token();
                        break Ok(statements);
                    }
                    Some(_) => match parse_statement(tokens, env) {
                        Ok(statement) => {
                            statements.push(statement);
                        }
                        Err(e) => break Err(e),
                    },
                    None => {
                        break Err(ParseFailure {
                            loc: tokens.location(),
                            message: String::from("Expected '}' at end of function expression"),
                        })
                    }
                }
            }
        };

        parse_args(uid, tokens, env)
            .and_then(|args| parse_body(tokens, env).map(|body| (args, body)))
            .map(|(args, body)| {
                env.pop_scope();
                Function {
                    loc: (start, end),
                    uid,
                    scope,
                    args,
                    body,
                }
            })
    }

    fn parse_expression(
        tokens: &mut TokenIterator,
        env: &mut Env,
    ) -> Result<Expression, ParseFailure> {
        let loc = tokens.location();
        match tokens.peek_token() {
            Some(Token::Identifier { .. }) => {
                parse_identifier(tokens, env).map(Expression::Identifier)
            }
            Some(Token::Number { .. }) => parse_number(tokens, env).map(Expression::Number),
            Some(Token::LParen { .. }) => parse_function(tokens, env).map(Expression::Function),
            _ => Err(ParseFailure {
                loc,
                message: String::from("Expected expression"),
            }),
        }
    }

    fn parse_declaration(
        tokens: &mut TokenIterator,
        env: &mut Env,
    ) -> Result<Declaration, ParseFailure> {
        let uid = env.uid();
        let (start, _) = tokens.location();
        match tokens.next_token() {
            Some(Token::Identifier { value, .. }) if value == "let" => Ok(()),
            _ => Err(ParseFailure {
                loc: tokens.location(),
                message: String::from("Expected declaration to start with 'let' keyword"),
            }),
        }
        .and_then(|_| match tokens.peek_token() {
            Some(Token::Identifier { value, .. }) => {
                let id_value = value.to_string();
                match env.lookup_scope(&id_value) {
                    None => {
                        env.add_binding(id_value, uid);
                        parse_identifier(tokens, env)
                    }
                    Some(_) => Err(ParseFailure {
                        loc: tokens.location(),
                        message: format!("Identifier '{}' is already declared", id_value),
                    }),
                }
            }
            _ => Err(ParseFailure {
                loc: tokens.location(),
                message: String::from("Expected identifier"),
            }),
        })
        .and_then(|identifier| match tokens.next_token() {
            Some(Token::Equals { .. }) => Ok(identifier),
            _ => Err(ParseFailure {
                loc: tokens.location(),
                message: String::from("Expected '='"),
            }),
        })
        .and_then(|identifier| parse_expression(tokens, env).map(|value| (identifier, value)))
        .map(|(id, value)| Declaration {
            id,
            value,
            uid,
            loc: (start, tokens.location().0),
            scope: 1,
        })
    }

    fn parse_statement(
        tokens: &mut TokenIterator,
        env: &mut Env,
    ) -> Result<Statement, ParseFailure> {
        match tokens.peek_token() {
            Some(Token::Identifier { value, .. }) if value == "let" => {
                parse_declaration(tokens, env).map(Statement::Declaration)
            }
            _ => Err(ParseFailure {
                loc: tokens.location(),
                message: String::from("Expected statement"),
            }),
        }
        .and_then(|stmt| match tokens.next_token() {
            Some(Token::SemiColon { .. }) => Ok(stmt),
            _ => Err(ParseFailure {
                loc: tokens.location(),
                message: String::from("Expected ';'"),
            }),
        })
    }

    pub fn parse(tokens: Vec<Token>) -> Result<ParseResult, Failure> {
        let mut token_iter = TokenIterator::new(tokens);
        let mut env = Env::new(
            1,
            Scope::Global {
                bindings: HashMap::new(),
            },
        );
        let uid = env.uid();
        let mut statements = Vec::new();
        let mut end = (1, 1);
        loop {
            match token_iter.peek_token() {
                Some(_) => match parse_statement(&mut token_iter, &mut env) {
                    Ok(statement) => {
                        end = statement.location().1;
                        statements.push(statement);
                    }
                    Err(e) => break Err(Failure::Parse(e)),
                },
                None => {
                    break Ok(ParseResult {
                        program: Program {
                            uid,
                            scope: 1,
                            loc: ((1, 1), end),
                            statements,
                        },
                        scopes: env.scopes,
                    })
                }
            }
        }
    }

    #[test]
    fn it_parses_tokens() {
        match parse(vec![
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
        ]) {
            Ok(parse_result) => {
                insta::assert_debug_snapshot!(parse_result);
            }
            Err(failure) => panic!(failure.message()),
        }
    }
}

mod ast {

    use crate::parser::NodeId;
    use crate::parser::ScopeId;
    use crate::typecheck::Type;
    use crate::Location;

    #[derive(Debug, PartialEq)]
    pub struct Number {
        pub uid: NodeId,
        pub loc: Location,
        pub value: i32,
        pub ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub struct Identifier {
        pub uid: NodeId,
        pub loc: Location,
        pub value: String,
        pub scope: ScopeId,
        pub ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub struct Program {
        pub uid: NodeId,
        pub loc: Location,
        pub statements: Vec<Statement>,
        pub scope: ScopeId,
    }

    #[derive(Debug, PartialEq)]
    pub struct Declaration {
        pub uid: NodeId,
        pub loc: Location,
        pub id: Identifier,
        pub value: Expression,
        pub scope: ScopeId,
        pub ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub struct Function {
        pub uid: NodeId,
        pub loc: Location,
        pub args: Vec<Identifier>,
        pub body: Vec<Statement>,
        pub scope: ScopeId,
        pub ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub enum Statement {
        Declaration(Declaration),
    }

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Number(Number),
        Identifier(Identifier),
        Function(Function),
    }
}

mod typecheck {
    use crate::ast;
    use crate::lexer::Token;
    use crate::parser;
    use crate::parser::Node;
    use crate::parser::NodeId;
    use crate::parser::Scope;
    use crate::parser::ScopeId;
    use crate::Failure;
    use crate::Locatable;
    use crate::TypeCheckFailure;
    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    pub enum Type {
        Var(i32),
        Function { args: Vec<Type>, ret: Box<Type> },
        Void,
        I32,
    }

    #[derive(Debug, PartialEq, Eq, Hash, Clone)]
    enum Constraint {
        Eq(Type, Type),
    }

    type Binding = (i32, Type);
    type Substitution = HashMap<i32, Type>;

    use Constraint::*;
    use Type::*;
    /*
    fn apply_substitution(subst: Substitution, ty: &Type) -> Type {
        match (subst, ty) {
            ((Type::Var(y), replacement), Type::Var(x)) if *x == y => replacement,
            (_, Type::I32) => *ty,
            (_, Type::Void) => *ty,
            (_, Type::Var(_)) => *ty,
        }
    }
    */

    fn extend_subst(
        id: i32,
        replacement: Type,
        mut subst: Vec<Binding>,
    ) -> Result<Vec<Binding>, TypeCheckFailure> {
        let mut extended_subst: Vec<Binding> = subst
            .into_iter()
            .map(|binding| match binding {
                // @FIX type errors happen here
                (lhs, Var(y)) if y == id => (lhs, replacement.clone()),
                _ => binding,
            })
            .collect();
        extended_subst.push((id, replacement));
        Ok(extended_subst)
    }

    fn update_constraints(
        id: i32,
        replacement: &Type,
        mut constraints: Vec<Constraint>,
    ) -> Result<Vec<Constraint>, TypeCheckFailure> {
        use Constraint::*;
        use Type::*;
        Ok(constraints
            .into_iter()
            .map(|constraint| match constraint {
                // @FIX type errors happen here
                Eq(lhs, Var(y)) if y == id => Eq(lhs, replacement.clone()),
                Eq(Var(y), rhs) if y == id => Eq(replacement.clone(), rhs),
                _ => constraint,
            })
            .collect())
    }

    fn unify_constraints(constraints: Vec<Constraint>) -> Result<Substitution, TypeCheckFailure> {
        fn unify(
            mut constraints: Vec<Constraint>,
            mut subst: Vec<Binding>,
        ) -> Result<Vec<Binding>, TypeCheckFailure> {
            match constraints.pop() {
                None => Ok(subst),
                // skip constraints that provide no extra information
                Some(Eq(I32, I32)) => unify(constraints, subst),
                Some(Eq(Void, Void)) => unify(constraints, subst),
                Some(Eq(x, y)) if x == y => unify(constraints, subst),
                // useful constraints
                Some(Eq(Var(x), ty)) => update_constraints(x, &ty, constraints)
                    .and_then(|updated_constraints| match extend_subst(x, ty, subst) {
                        Ok(extended_subst) => Ok((updated_constraints, extended_subst)),
                        Err(e) => Err(e),
                    })
                    .and_then(|(constraints, subst)| unify(constraints, subst)),
                Some(Eq(ty, Var(x))) => {
                    constraints.push(Eq(Var(x), ty));
                    unify(constraints, subst)
                }
                Some(Eq(t1, t2)) => Err(TypeCheckFailure {
                    message: format!("Types to not unify {:?}, {:?}", t1, t2),
                }),
            }
        }
        unify(constraints, Vec::new()).map(|subst| subst.into_iter().collect())
    }

    #[test]
    fn it_solves_substitutions() {
        assert_eq!(
            unify_constraints(vec![Eq(Var(2), Var(1)), Eq(Var(1), I32)],),
            {
                let mut res = HashMap::new();
                res.insert(1, Type::I32);
                res.insert(2, Type::I32);
                res
            }
        );
    }

    #[test]
    fn it_solves_void_substitutions() {
        assert_eq!(
            unify_constraints(vec![
                Eq(Type::Var(2), Type::Var(1)),
                Eq(Type::Var(1), Type::Void)
            ],),
            {
                let mut res = HashMap::new();
                res.insert(1, Type::Void);
                res.insert(2, Type::Void);
                res
            }
        );
    }

    #[test]
    fn it_solves_substitutions_in_any_order() {
        assert_eq!(
            unify_constraints(vec![
                Eq(Type::Var(1), Type::I32),
                Eq(Type::Var(2), Type::Var(1))
            ],),
            {
                let mut res = HashMap::new();
                res.insert(1, Type::I32);
                res.insert(2, Type::I32);
                res
            }
        );
    }

    #[test]
    fn it_skips_duplicate_substitutions() {
        assert_eq!(
            unify_constraints(vec![
                Eq(Type::Var(1), Type::I32),
                Eq(Type::Var(1), Type::I32)
            ],),
            {
                let mut res = HashMap::new();
                res.insert(1, Type::I32);
                res
            }
        );
    }

    fn generate_constraints_identifier(
        identifier: &parser::Identifier,
        scopes: &parser::Scopes,
    ) -> Vec<Constraint> {
        match scopes
            .get(&identifier.scope)
            .map(|scope| scope.bindings().get(&identifier.value))
            .flatten()
        {
            Some(parent_node_id) => vec![Eq(Var(identifier.node_id()), Var(*parent_node_id))],
            None => vec![],
        }
    }

    fn generate_constraints_number(number: &parser::Number) -> Vec<Constraint> {
        vec![Eq(Type::Var(number.node_id()), Type::I32)]
    }

    fn generate_constraints_function(
        function: &parser::Function,
        scopes: &parser::Scopes,
    ) -> Vec<Constraint> {
        let mut constraints = Vec::new();
        for arg in &function.args {
            constraints.append(&mut generate_constraints_identifier(arg, &scopes));
        }
        for statement in &function.body {
            constraints.append(&mut generate_constraints_statement(statement, &scopes));
        }
        constraints.push(Eq(
            Var(function.node_id()),
            Type::Function {
                args: function
                    .args
                    .iter()
                    .map(|id| Type::Var(id.node_id()))
                    .collect(),
                ret: Box::new(Var(function.body.last().unwrap().node_id())),
            },
        ));
        constraints
    }

    fn generate_constraints_expression(
        expression: &parser::Expression,
        scopes: &parser::Scopes,
    ) -> Vec<Constraint> {
        match expression {
            parser::Expression::Function(expr) => generate_constraints_function(expr, scopes),
            parser::Expression::Number(expr) => generate_constraints_number(expr),
            parser::Expression::Identifier(expr) => generate_constraints_identifier(expr, scopes),
        }
    }

    fn generate_constraints_declaration(
        declaration: &parser::Declaration,
        scopes: &parser::Scopes,
    ) -> Vec<Constraint> {
        let mut constraints = Vec::new();
        constraints.append(&mut generate_constraints_identifier(
            &declaration.id,
            scopes,
        ));
        constraints.append(&mut generate_constraints_expression(
            &declaration.value,
            scopes,
        ));
        constraints.push(Eq(Type::Var(declaration.node_id()), Type::Void));
        constraints.push(Eq(
            Type::Var(declaration.id.node_id()),
            Type::Var(declaration.value.node_id()),
        ));
        constraints
    }

    fn generate_constraints_statement(
        statement: &parser::Statement,
        scopes: &parser::Scopes,
    ) -> Vec<Constraint> {
        match statement {
            parser::Statement::Declaration(d) => generate_constraints_declaration(d, scopes),
        }
    }

    fn generate_constraints(result: &parser::ParseResult) -> Vec<Constraint> {
        result
            .program
            .statements
            .iter()
            .flat_map(|s| generate_constraints_statement(s, &result.scopes))
            .collect()
    }

    fn apply_subst_identifier(
        identifier: parser::Identifier,
        subst: &Substitution,
    ) -> ast::Identifier {
        ast::Identifier {
            loc: identifier.location(),
            scope: identifier.scope,
            uid: identifier.node_id(),
            ty: subst.get(&identifier.node_id()).unwrap().clone(),
            value: identifier.value,
        }
    }

    fn apply_subst_number(number: parser::Number, subst: &Substitution) -> ast::Number {
        ast::Number {
            loc: number.location(),
            uid: number.node_id(),
            ty: subst.get(&number.node_id()).unwrap().clone(),
            value: number.value.parse().unwrap(),
        }
    }

    fn apply_subst_function(function: parser::Function, subst: &Substitution) -> ast::Function {
        ast::Function {
            loc: function.location(),
            uid: function.node_id(),
            scope: function.scope,
            ty: subst.get(&function.node_id()).unwrap().clone(),
            args: function
                .args
                .into_iter()
                .map(|id| apply_subst_identifier(id, &subst))
                .collect(),
            body: function
                .body
                .into_iter()
                .map(|statement| apply_subst_statement(statement, &subst))
                .collect(),
        }
    }

    fn apply_subst_expression(
        expression: parser::Expression,
        subst: &Substitution,
    ) -> ast::Expression {
        match expression {
            parser::Expression::Function(function) => {
                ast::Expression::Function(apply_subst_function(function, subst))
            }
            parser::Expression::Identifier(id) => {
                ast::Expression::Identifier(apply_subst_identifier(id, subst))
            }
            parser::Expression::Number(n) => ast::Expression::Number(apply_subst_number(n, subst)),
        }
    }

    fn apply_subst_declaration(
        declaration: parser::Declaration,
        subst: &Substitution,
    ) -> ast::Declaration {
        ast::Declaration {
            loc: declaration.location(),
            uid: declaration.node_id(),
            scope: declaration.scope,
            ty: subst.get(&declaration.node_id()).unwrap().clone(),
            id: apply_subst_identifier(declaration.id, subst),
            value: apply_subst_expression(declaration.value, subst),
        }
    }

    fn apply_subst_statement(statement: parser::Statement, subst: &Substitution) -> ast::Statement {
        match statement {
            parser::Statement::Declaration(d) => {
                ast::Statement::Declaration(apply_subst_declaration(d, subst))
            }
        }
    }

    fn apply_subst(program: parser::Program, subst: &Substitution) -> ast::Program {
        ast::Program {
            loc: program.location(),
            uid: program.node_id(),
            scope: program.scope,
            statements: program
                .statements
                .into_iter()
                .map(|s| apply_subst_statement(s, subst))
                .collect(),
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct TypeCheckResult {
        pub program: ast::Program,
        pub scopes: HashMap<ScopeId, Scope<NodeId>>,
    }

    pub fn typecheck(result: parser::ParseResult) -> Result<TypeCheckResult, Failure> {
        let constraints = generate_constraints(&result);
        match unify_constraints(constraints) {
            Ok(subst) => Ok(TypeCheckResult {
                program: apply_subst(result.program, &subst),
                scopes: result.scopes,
            }),
            Err(e) => Err(Failure::TypeCheck(e)),
        }
    }

    #[test]
    fn it_typechecks_declaration() {
        match parser::parse(vec![
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
        ])
        .and_then(typecheck)
        {
            Ok(ast) => insta::assert_debug_snapshot!(ast),
            Err(_) => panic!("typecheck failed"),
        }
    }
}

mod codegen {

    extern crate libc;
    extern crate llvm_sys as llvm;

    use crate::ast::{Declaration, Expression, Identifier, Number, Program, Statement};
    use crate::parser::NodeId;
    use crate::parser::Scope;
    use crate::parser::ScopeId;
    use crate::{CodeGenFailure, Failure};

    use llvm::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
    use llvm::core::*;
    use llvm::prelude::*;
    use llvm::target::{
        LLVMInitializeWebAssemblyAsmParser, LLVMInitializeWebAssemblyAsmPrinter,
        LLVMInitializeWebAssemblyDisassembler, LLVMInitializeWebAssemblyTarget,
        LLVMInitializeWebAssemblyTargetInfo, LLVMInitializeWebAssemblyTargetMC,
    };
    use llvm::target_machine::LLVMCodeGenOptLevel::*;
    use llvm::target_machine::LLVMCodeModel::*;
    use llvm::target_machine::LLVMRelocMode::*;
    use llvm::target_machine::LLVMTargetMachineEmitToMemoryBuffer;
    use llvm::target_machine::*;
    use std::collections::HashMap;
    use std::ffi::CString;
    use std::mem::MaybeUninit;
    use std::slice;

    macro_rules! c_str {
        ($s:expr) => {
            concat!($s, "\0").as_ptr() as *const i8
        };
    }

    struct Types {
        int_32: LLVMTypeRef,
    }

    struct Env {
        module: LLVMModuleRef,
        context: LLVMContextRef,
        builder: LLVMBuilderRef,
        types: Types,
    }

    fn generate_number(env: &mut Env, number: &Number) -> Result<LLVMValueRef, CodeGenFailure> {
        unsafe { Ok(LLVMConstInt(env.types.int_32, number.value as u64, 0)) }
    }

    fn generate_declaration(
        env: &mut Env,
        declaration: &Declaration,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<(), CodeGenFailure> {
        let id_name = CString::new(declaration.id.value.clone()).unwrap();
        match &declaration.value {
            Expression::Number(n) => {
                generate_number(env, &n).and_then(|ll_num| match scopes.get(&declaration.scope) {
                    Some(Scope::Global { .. }) => unsafe {
                        let g = LLVMAddGlobal(env.module, env.types.int_32, id_name.as_ptr());
                        LLVMSetInitializer(g, ll_num);
                        Ok(())
                    },
                    _ => Err(CodeGenFailure {
                        message: String::from("Don't know how to codegen this"),
                    }),
                })
            }
            Expression::Identifier(rhs_id) => unsafe {
                let rhs_name = CString::new(rhs_id.value.clone()).unwrap();
                let rhs_value = LLVMGetNamedGlobal(env.module, rhs_name.as_ptr());
                // @Idea could also create this type from rhs_id.ty
                let rhs_type = LLVMGlobalGetValueType(rhs_value);
                let g = LLVMAddGlobal(env.module, LLVMPointerType(rhs_type, 0), id_name.as_ptr());
                LLVMSetInitializer(g, rhs_value);
                Ok(())
            },
        }
    }

    fn generate_statement(
        env: &mut Env,
        statement: &Statement,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<(), CodeGenFailure> {
        match statement {
            Statement::Declaration(d) => generate_declaration(env, d, scopes),
        }
    }

    fn generate_program(
        env: &mut Env,
        program: &Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<(), CodeGenFailure> {
        let mut result: Result<(), CodeGenFailure> = Ok(());
        let mut iter = program.statements.iter();
        while let Some(statement) = iter.next() {
            result = generate_statement(env, statement, scopes);
            if let Err(_) = result {
                break;
            }
        }
        result.and_then(|_| unsafe {
            {
                // verify module is valid
                let mut out_error = MaybeUninit::<*mut libc::c_char>::uninit();
                match LLVMVerifyModule(
                    env.module,
                    LLVMVerifierFailureAction::LLVMReturnStatusAction,
                    out_error.as_mut_ptr(),
                ) {
                    0 => Ok(()),
                    _ => Err(CodeGenFailure {
                        message: CString::from_raw(out_error.assume_init())
                            .to_str()
                            .unwrap()
                            .to_owned(),
                    }),
                }
            }
        })
    }

    fn try_generate<'a>(
        file_name: &str,
        program: &Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<&'a [u8], CodeGenFailure> {
        unsafe {
            let module =
                LLVMModuleCreateWithName(CString::new(file_name).expect("CString failed").as_ptr());
            let context = LLVMGetModuleContext(module);
            let builder = LLVMCreateBuilderInContext(context);
            let mut env = Env {
                module,
                context,
                builder,
                types: Types {
                    int_32: LLVMInt32TypeInContext(context),
                },
            };
            // setup
            let result = match generate_program(&mut env, program, scopes) {
                Err(e) => Err(e),
                Ok(_) => {
                    LLVMInitializeWebAssemblyTargetInfo();
                    LLVMInitializeWebAssemblyTarget();
                    LLVMInitializeWebAssemblyTargetMC();
                    LLVMInitializeWebAssemblyAsmPrinter();
                    LLVMInitializeWebAssemblyAsmParser();
                    LLVMInitializeWebAssemblyDisassembler();

                    let triple = CString::new("wasm32-unknown-unknown").unwrap();

                    {
                        let mut out_target = MaybeUninit::<LLVMTargetRef>::uninit();
                        let mut out_error = MaybeUninit::<*mut libc::c_char>::uninit();
                        match LLVMGetTargetFromTriple(
                            triple.as_ptr(),
                            out_target.as_mut_ptr(),
                            out_error.as_mut_ptr(),
                        ) {
                            0 => Ok(out_target.assume_init()),
                            _ => Err(CodeGenFailure {
                                message: CString::from_raw(out_error.assume_init())
                                    .to_str()
                                    .unwrap()
                                    .to_owned(),
                            }),
                        }
                    }
                    .and_then(|target| {
                        let tm = LLVMCreateTargetMachine(
                            target,
                            triple.as_ptr(),
                            b"\0".as_ptr() as *const _,
                            b"\0".as_ptr() as *const _,
                            LLVMCodeGenLevelDefault,
                            LLVMRelocDefault,
                            LLVMCodeModelDefault,
                        );

                        LLVMSetTarget(module, triple.as_ptr());
                        LLVMSetDataLayout(module, LLVMCreateTargetDataLayout(tm) as *const i8);

                        let mut out_membuf = MaybeUninit::<LLVMMemoryBufferRef>::uninit();
                        let mut out_error = MaybeUninit::<*mut libc::c_char>::uninit();
                        let result = match LLVMTargetMachineEmitToMemoryBuffer(
                            tm,
                            module,
                            LLVMCodeGenFileType::LLVMObjectFile,
                            out_error.as_mut_ptr(),
                            out_membuf.as_mut_ptr(),
                        ) {
                            0 => Ok(out_membuf.assume_init()),
                            _ => Err(CodeGenFailure {
                                message: CString::from_raw(out_error.assume_init())
                                    .to_str()
                                    .unwrap()
                                    .to_owned(),
                            }),
                        };

                        LLVMDisposeTargetMachine(tm);
                        result
                    })
                    .and_then(|mbuf| {
                        let p = LLVMGetBufferStart(mbuf);
                        let len = LLVMGetBufferSize(mbuf);
                        // @Fix memory leak - mbuf doesn't get free deallocd
                        // Doing this LLVMDisposeMemoryBuffer(mbuf) disposes slice memory
                        // likely need to copy memory into rust owned buffer
                        let result = slice::from_raw_parts(p as *const _, len as usize);
                        Ok(result)
                    })
                }
            };

            LLVMDisposeModule(env.module);
            LLVMContextDispose(env.context);
            LLVMDisposeBuilder(env.builder);
            result
        }
    }

    pub fn generate<'a>(
        file_name: &str,
        program: &Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<&'a [u8], Failure> {
        match try_generate(file_name, program, scopes) {
            Err(e) => Err(Failure::CodeGen(e)),
            Ok(buf) => Ok(buf),
        }
    }

    fn try_show(
        file_name: &str,
        program: &Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<(), CodeGenFailure> {
        unsafe {
            let module =
                LLVMModuleCreateWithName(CString::new(file_name).expect("CString failed").as_ptr());
            let context = LLVMGetModuleContext(module);
            let builder = LLVMCreateBuilderInContext(context);
            let mut env = Env {
                module,
                context,
                builder,
                types: Types {
                    int_32: LLVMInt32TypeInContext(context),
                },
            };
            // setup
            let result = match generate_program(&mut env, program, scopes) {
                Err(e) => Err(e),
                Ok(_) => {
                    LLVMDumpModule(env.module);
                    println!("");
                    Ok(())
                }
            };

            LLVMDisposeModule(env.module);
            LLVMContextDispose(env.context);
            LLVMDisposeBuilder(env.builder);
            result
        }
    }

    pub fn show(
        file_name: &str,
        program: &Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<(), Failure> {
        match try_show(file_name, program, scopes) {
            Err(e) => Err(Failure::CodeGen(e)),
            Ok(x) => Ok(x),
        }
    }
}

mod linker {
    use crate::{Failure, LinkFailure};
    use std::io::Write;
    use std::process::Command;
    use tempfile::NamedTempFile;

    fn try_link(file_name: &str, buffer: &[u8]) -> Result<(), LinkFailure> {
        let mut obj_file = NamedTempFile::new()?;
        obj_file.write_all(buffer)?;
        // @TODO make this either call a C method or bundle lld in out dir binary
        Command::new("./llvm-project/out/bin/lld")
            .args(&[
                "-flavor",
                "wasm",
                "--no-entry",
                "--export-dynamic",
                obj_file.path().to_str().unwrap(),
                "-o",
                &format!("{}.wasm", file_name),
            ])
            .status()?;
        Ok(())
    }

    pub fn link(file_name: &str, buffer: &[u8]) -> Result<(), Failure> {
        match try_link(file_name, buffer) {
            Err(e) => Err(Failure::Link(e)),
            Ok(_) => Ok(()),
        }
    }
}

#[derive(Debug, StructOpt)]
struct Cli {
    // outputs the intermediate representation
    #[structopt(long)]
    ir: bool,
    // input file
    #[structopt(parse(from_os_str))]
    path: std::path::PathBuf,
}

use std::fs;

fn main() {
    let args = Cli::from_args();
    // println!("{:#?}", args);
    let ir = args.ir;
    let input = fs::read_to_string(&args.path).expect("input file not found");
    let name = args.path.file_stem().map(|n| n.to_str()).flatten().unwrap();
    match lexer::lex(&input)
        .and_then(parser::parse)
        .and_then(typecheck::typecheck)
        .and_then(|res| {
            if ir {
                codegen::show(name, &res.program, &res.scopes)
            } else {
                codegen::generate(name, &res.program, &res.scopes)
                    .and_then(|buf| linker::link(name, buf))
            }
        }) {
        Ok(_) => println!("✅ Done"),
        Err(e) => println!("❌ {}", e.message()),
    }
}
