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
pub enum Failure {
    Lex(LexFailure),
    Parse(ParseFailure),
    TypeCheck(TypeCheckFailure),
}

impl Failure {
    pub fn message(&self) -> String {
        match self {
            Failure::Lex(f) => f.message.clone(),
            Failure::TypeCheck(f) => f.message.clone(),
            Failure::Parse(f) => f.message.clone(),
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
    }

    impl Locatable for Token {
        fn location(&self) -> Location {
            match &self {
                Token::Identifier { loc, .. } => *loc,
                Token::Equals { loc } => *loc,
                Token::Number { loc, .. } => *loc,
                Token::SemiColon { loc } => *loc,
            }
        }
    }

    impl Token {
        pub fn value(&self) -> String {
            match &self {
                Token::Identifier { value, .. } => value.clone(),
                Token::Equals { .. } => String::from("="),
                Token::Number { value, .. } => value.clone(),
                Token::SemiColon { .. } => String::from(";"),
            }
        }
    }

    fn lex(content: &str) -> Result<Vec<Token>, Failure> {
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
    }

    impl Locatable for Expression {
        fn location(&self) -> Location {
            match self {
                Expression::Number(n) => n.location(),
                Expression::Identifier(id) => id.location(),
            }
        }
    }

    impl Node for Expression {
        fn node_id(&self) -> NodeId {
            match self {
                Expression::Number(n) => n.node_id(),
                Expression::Identifier(id) => id.node_id(),
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
        pub value: String,
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
    pub enum Statement {
        Declaration(Declaration),
    }

    #[derive(Debug, PartialEq)]
    pub enum Expression {
        Number(Number),
        Identifier(Identifier),
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
    use std::collections::HashMap;

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    pub enum Type {
        Var(i32),
        Void,
        I32,
    }

    type Substitution = (Type, Type);
    type Constraints = Vec<Substitution>;
    type Solutions = HashMap<i32, Type>;

    fn apply_substitution(subst: Substitution, ty: Type) -> Type {
        match (subst, ty) {
            ((Type::Var(y), replacement), Type::Var(x)) if x == y => replacement,
            (_, Type::I32) => ty,
            (_, Type::Void) => ty,
            (_, Type::Var(_)) => ty,
        }
    }

    fn solve_constraint(mut constraints: Constraints, solutions: Solutions) -> Solutions {
        match constraints.pop() {
            None => solutions,
            Some(subst) => solve_constraint(
                constraints
                    .iter()
                    .filter_map(|(ty_left, ty_right)| {
                        match (
                            apply_substitution(subst, *ty_left),
                            apply_substitution(subst, *ty_right),
                        ) {
                            (left, right) if left == right => None,
                            x => Some(x),
                        }
                    })
                    .collect(),
                {
                    let mut next_solutions: Solutions = solutions
                        .iter()
                        .map(|(id, ty)| (*id, apply_substitution(subst, *ty)))
                        .collect();
                    if let (Type::Var(k), v) = subst {
                        next_solutions.insert(k, v);
                    }
                    next_solutions
                },
            ),
        }
    }

    // O(n^2)
    fn solve_constraints(constraints: Constraints) -> Solutions {
        solve_constraint(constraints, HashMap::new())
    }

    #[test]
    fn it_solves_substitutions() {
        assert_eq!(
            solve_constraints(vec![
                (Type::Var(2), Type::Var(1)),
                (Type::Var(1), Type::I32)
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
    fn it_solves_void_substitutions() {
        assert_eq!(
            solve_constraints(vec![
                (Type::Var(2), Type::Var(1)),
                (Type::Var(1), Type::Void)
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
            solve_constraints(vec![
                (Type::Var(1), Type::I32),
                (Type::Var(2), Type::Var(1))
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
            solve_constraints(vec![(Type::Var(1), Type::I32), (Type::Var(1), Type::I32)],),
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
    ) -> Constraints {
        match scopes
            .get(&identifier.scope)
            .map(|scope| scope.bindings().get(&identifier.value))
            .flatten()
        {
            Some(parent_node_id) => {
                vec![(Type::Var(identifier.node_id()), Type::Var(*parent_node_id))]
            }
            None => vec![],
        }
    }

    fn generate_constraints_number(number: &parser::Number) -> Constraints {
        vec![(Type::Var(number.node_id()), Type::I32)]
    }

    fn generate_constraints_expression(
        expression: &parser::Expression,
        scopes: &parser::Scopes,
    ) -> Constraints {
        match expression {
            parser::Expression::Number(expr) => generate_constraints_number(expr),
            parser::Expression::Identifier(expr) => generate_constraints_identifier(expr, scopes),
        }
    }

    fn generate_constraints_declaration(
        declaration: &parser::Declaration,
        scopes: &parser::Scopes,
    ) -> Constraints {
        let mut constraints = Vec::new();
        constraints.append(&mut generate_constraints_identifier(
            &declaration.id,
            scopes,
        ));
        constraints.append(&mut generate_constraints_expression(
            &declaration.value,
            scopes,
        ));
        constraints.push((Type::Var(declaration.node_id()), Type::Void));
        constraints.push((
            Type::Var(declaration.id.node_id()),
            Type::Var(declaration.value.node_id()),
        ));
        constraints
    }

    fn generate_constraints_statement(
        statement: &parser::Statement,
        scopes: &parser::Scopes,
    ) -> Constraints {
        match statement {
            parser::Statement::Declaration(d) => generate_constraints_declaration(d, scopes),
        }
    }

    fn generate_constraints(result: &parser::ParseResult) -> Constraints {
        result
            .program
            .statements
            .iter()
            .flat_map(|s| generate_constraints_statement(s, &result.scopes))
            .collect()
    }

    fn apply_constraints_identifier(
        identifier: parser::Identifier,
        solved: &Solutions,
    ) -> ast::Identifier {
        ast::Identifier {
            loc: identifier.location(),
            scope: identifier.scope,
            uid: identifier.node_id(),
            ty: *solved.get(&identifier.node_id()).unwrap(),
            value: identifier.value,
        }
    }

    fn apply_constraints_number(number: parser::Number, solved: &Solutions) -> ast::Number {
        ast::Number {
            loc: number.location(),
            uid: number.node_id(),
            ty: *solved.get(&number.node_id()).unwrap(),
            value: number.value,
        }
    }

    fn apply_constraints_expression(
        expression: parser::Expression,
        solved: &Solutions,
    ) -> ast::Expression {
        match expression {
            parser::Expression::Identifier(id) => {
                ast::Expression::Identifier(apply_constraints_identifier(id, solved))
            }
            parser::Expression::Number(n) => {
                ast::Expression::Number(apply_constraints_number(n, solved))
            }
        }
    }

    fn apply_constraints_declaration(
        declaration: parser::Declaration,
        solved: &Solutions,
    ) -> ast::Declaration {
        ast::Declaration {
            loc: declaration.location(),
            uid: declaration.node_id(),
            scope: declaration.scope,
            ty: *solved.get(&declaration.node_id()).unwrap(),
            id: apply_constraints_identifier(declaration.id, solved),
            value: apply_constraints_expression(declaration.value, solved),
        }
    }

    fn apply_constraints_statement(
        statement: parser::Statement,
        solved: &Solutions,
    ) -> ast::Statement {
        match statement {
            parser::Statement::Declaration(d) => {
                ast::Statement::Declaration(apply_constraints_declaration(d, solved))
            }
        }
    }

    fn apply_constraints(program: parser::Program, solved: &Solutions) -> ast::Program {
        ast::Program {
            loc: program.location(),
            uid: program.node_id(),
            scope: program.scope,
            statements: program
                .statements
                .into_iter()
                .map(|s| apply_constraints_statement(s, solved))
                .collect(),
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct TypeCheckResult {
        pub program: ast::Program,
        pub scopes: HashMap<ScopeId, Scope<NodeId>>,
    }

    fn typecheck(result: parser::ParseResult) -> Result<TypeCheckResult, Failure> {
        let constraints = generate_constraints(&result);
        let solutions = solve_constraints(constraints);
        let program = apply_constraints(result.program, &solutions);
        Ok(TypeCheckResult {
            program,
            scopes: result.scopes,
        })
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

    use llvm::analysis::{LLVMVerifierFailureAction, LLVMVerifyModule};
    use llvm::bit_writer::*;
    use llvm::core::*;
    use llvm::prelude::*;
    use llvm::target::{
        LLVMInitializeWebAssemblyAsmParser, LLVMInitializeWebAssemblyAsmPrinter,
        LLVMInitializeWebAssemblyDisassembler, LLVMInitializeWebAssemblyTarget,
        LLVMInitializeWebAssemblyTargetInfo, LLVMInitializeWebAssemblyTargetMC, LLVMTargetDataRef,
        LLVM_InitializeAllTargetInfos, LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets,
        LLVM_InitializeNativeAsmParser, LLVM_InitializeNativeAsmPrinter,
        LLVM_InitializeNativeTarget,
    };
    use llvm::target_machine::LLVMCodeGenOptLevel::*;
    use llvm::target_machine::LLVMCodeModel::*;
    use llvm::target_machine::LLVMGetDefaultTargetTriple;
    use llvm::target_machine::LLVMRelocMode::*;
    use llvm::target_machine::*;
    use llvm::target_machine::{LLVMTargetMachineEmitToFile, LLVMTargetMachineEmitToMemoryBuffer};

    use std::ffi::CString;
    use std::mem;
    use std::mem::MaybeUninit;
    use std::process::Command;
    use std::ptr;

    macro_rules! c_str {
        ($s:expr) => {
            concat!($s, "\0").as_ptr() as *const i8
        };
    }

    pub fn generate() {
        unsafe {
            LLVMInitializeWebAssemblyTargetInfo();
            LLVMInitializeWebAssemblyTarget();
            LLVMInitializeWebAssemblyTargetMC();
            LLVMInitializeWebAssemblyAsmPrinter();
            LLVMInitializeWebAssemblyAsmParser();
            LLVMInitializeWebAssemblyDisassembler();

            // setup
            let context = LLVMContextCreate();
            let module = LLVMModuleCreateWithName(c_str!("main"));
            let builder = LLVMCreateBuilderInContext(context);

            // common types
            let void_type = LLVMVoidTypeInContext(context);
            let i8_type = LLVMIntTypeInContext(context, 8);
            let i8_pointer_type = LLVMPointerType(i8_type, 0);

            LLVMAddGlobal(module, i8_type, c_str!("log"));

            // declare that there's a `void log(i8*)` function in the environment
            // but don't provide a block (aka body) so that it in the wasm module
            // it'll be imported
            // let log_func_type = LLVMFunctionType(void_type, i8_type as *mut _, 1, 0);
            // let log_func = LLVMAddFunction(module, c_str!("log"), log_func_type);
            // // let bb_ref = LLVMCreateBasicBlockInContext(context, c_str!("entry"));
            // LLVMAppendBasicBlockInContext(context, log_func, c_str!("entry"));

            // // our "main" function which we'll need to call explicitly from JavaScript
            // // after we've instantiated the WebAssembly.Instance
            // let main_func_type = LLVMFunctionType(void_type, ptr::null_mut(), 0, 0);
            // let main_func = LLVMAddFunction(module, c_str!("main"), main_func_type);
            // let main_block = LLVMAppendBasicBlockInContext(context, main_func, c_str!("main"));
            // LLVMPositionBuilderAtEnd(builder, main_block);

            // // main's function body
            // let hello_world_str =
            //     LLVMBuildGlobalStringPtr(builder, c_str!("hello, world."), c_str!(""));
            // let log_args = [hello_world_str].as_ptr() as *mut _;
            // // calling `log("hello, world.")`
            // LLVMBuildCall(builder, log_func, log_args, 1, c_str!(""));
            // LLVMBuildRetVoid(builder);

            // write our bitcode file
            LLVMSetTarget(module, c_str!("wasm32-unknown-unknown-elf"));
            // LLVMWriteBitcodeToFile(module, c_str!("main.bc"));

            LLVMVerifyModule(
                module,
                LLVMVerifierFailureAction::LLVMPrintMessageAction,
                ptr::null_mut(),
            );

            // generate code
            let triple = CString::new("wasm32-unknown-unknown").unwrap();
            let mut target = MaybeUninit::<LLVMTargetRef>::uninit();
            let mut error = MaybeUninit::<*mut libc::c_char>::uninit();

            let r =
                LLVMGetTargetFromTriple(triple.as_ptr(), target.as_mut_ptr(), error.as_mut_ptr());
            // println!(
            //     "result {}, error {}",
            //     r,
            //     CString::from_raw(error.assume_init()).to_str().unwrap()
            // );
            // println!("target: {:?}", *LLVMGetFirstTarget());
            let tm = LLVMCreateTargetMachine(
                target.assume_init(),
                triple.as_ptr(),
                b"\0".as_ptr() as *const _,
                b"\0".as_ptr() as *const _,
                LLVMCodeGenLevelDefault,
                LLVMRelocDefault,
                LLVMCodeModelDefault,
            );

            LLVMSetDataLayout(module, LLVMCreateTargetDataLayout(tm) as *const i8);

            let out_file = CString::new("main.o").unwrap();

            let mut emit_error = MaybeUninit::<*mut libc::c_char>::uninit();

            let res = LLVMTargetMachineEmitToFile(
                tm,
                module,
                out_file.into_raw(),
                LLVMCodeGenFileType::LLVMObjectFile,
                emit_error.as_mut_ptr(),
            );

            if res != 0 {
                println!(
                    "result {}, error {}",
                    res,
                    CString::from_raw(emit_error.assume_init())
                        .to_str()
                        .unwrap()
                );
            } else {
                println!("code generated");
            }

            let link_res = Command::new("./llvm-project/out/bin/lld")
                .args(&[
                    "-flavor",
                    "wasm",
                    "--allow-undefined",
                    "--no-entry",
                    "main.o",
                    "-o",
                    "main.wasm",
                ])
                .status()
                .expect("Linking should work");

            LLVMDisposeTargetMachine(tm);
            LLVMDisposeBuilder(builder);
            LLVMDisposeModule(module);
            LLVMContextDispose(context);
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
    codegen::generate();
    println!("Hello, world!");
}
