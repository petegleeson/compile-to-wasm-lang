use structopt::StructOpt;

type Line = i32;
type Column = i32;

type Location = ((Line, Column), (Line, Column));

trait Locatable {
    fn location(&self) -> Location;
}

mod lexer {
    use crate::Locatable;
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
    use crate::Locatable;
    use crate::Location;
    use insta::assert_debug_snapshot;
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

    #[derive(Debug, PartialEq)]
    pub struct Number {
        uid: NodeId,
        loc: Location,
        value: String,
    }

    impl Locatable for Number {
        fn location(&self) -> Location {
            self.loc
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Identifier {
        pub uid: NodeId,
        pub loc: Location,
        pub value: String,
        pub scope: ScopeId,
    }

    impl Locatable for Identifier {
        fn location(&self) -> Location {
            self.loc
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Program {
        pub uid: NodeId,
        pub loc: Location,
        pub statements: Vec<Statement>,
        pub scope: ScopeId,
    }

    impl Locatable for Program {
        fn location(&self) -> Location {
            self.loc
        }
    }

    #[derive(Debug, PartialEq)]
    pub struct Declaration {
        pub uid: NodeId,
        pub loc: Location,
        pub id: Identifier,
        pub value: Expression,
        pub scope: ScopeId,
    }

    impl Locatable for Declaration {
        fn location(&self) -> Location {
            self.loc
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

    #[derive(Debug, PartialEq)]
    struct ParseFailure {
        loc: Location,
        message: String,
    }

    #[derive(Debug, PartialEq)]
    pub struct ParseResult {
        pub program: Program,
        pub scopes: HashMap<ScopeId, Scope<NodeId>>,
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

    fn parse(tokens: Vec<Token>) -> Result<ParseResult, ParseFailure> {
        let mut token_iter = TokenIterator::new(tokens);
        let mut env = Env::new(
            1,
            Scope::Global {
                bindings: HashMap::new(),
            },
        );
        // let global_scope = env.scope;
        let uid = env.uid();
        let mut statements = Vec::new();
        let mut end = (1, 1);
        loop {
            match token_iter.peek_token() {
                Some(_) => match parse_statement(&mut token_iter, &mut env) {
                    Ok(statment) => {
                        statements.push(statment);
                    }
                    Err(e) => break Err(e),
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
                assert_debug_snapshot!(parse_result);
            }
            Err(failure) => panic!(failure.message),
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
        uid: NodeId,
        loc: Location,
        value: String,
        ty: Type,
    }

    #[derive(Debug, PartialEq)]
    pub struct Identifier {
        uid: NodeId,
        loc: Location,
        value: String,
        scope: ScopeId,
        ty: Type,
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
        uid: NodeId,
        loc: Location,
        id: Identifier,
        value: Expression,
        scope: ScopeId,
        ty: Type,
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
    use crate::parser;
    use crate::parser::NodeId;
    use crate::parser::Scope;
    use crate::parser::ScopeId;
    use crate::Locatable;
    use crate::Location;
    use std::collections::HashMap;
    use std::ptr::eq;

    #[derive(Debug, PartialEq, Eq, Hash, Clone, Copy)]
    pub enum Type {
        Var(i32),
        I32,
    }

    type Substitution = (Type, Type);
    type Constraints = Vec<Substitution>;
    type Solutions = Vec<Substitution>;

    fn apply_substitution(subst: Substitution, constraint: Substitution) -> Substitution {
        match (subst, constraint) {
            ((Type::Var(x), replacement), (left, Type::Var(y))) if x == y => (left, replacement),
            ((Type::Var(x), replacement), (Type::Var(y), right)) if x == y => (replacement, right),
            (_, _) => constraint,
        }
    }

    fn solve_constraint(constraints: Constraints, index: usize) -> Solutions {
        match constraints.get(index) {
            None => constraints,
            Some(subst) => solve_constraint(
                constraints
                    .iter()
                    .filter_map(|constraint| {
                        // skip the constraint we are substituting
                        if eq(subst, constraint) {
                            Some(*constraint)
                        } else {
                            match apply_substitution(*subst, *constraint) {
                                (left, right) if left == right => None,
                                x => Some(x),
                            }
                        }
                    })
                    .collect(),
                index + 1,
            ),
        }
    }

    // O(n^2)
    fn solve_constraints(constraints: Constraints) -> Solutions {
        solve_constraint(constraints, 0)
    }

    #[test]
    fn it_solves_substitutions() {
        assert_eq!(
            solve_constraints(vec![
                (Type::Var(2), Type::Var(1)),
                (Type::Var(1), Type::I32)
            ],),
            vec![(Type::Var(2), Type::I32), (Type::Var(1), Type::I32)]
        );
    }

    #[test]
    fn it_solves_substitutions_in_any_order() {
        assert_eq!(
            solve_constraints(vec![
                (Type::Var(1), Type::I32),
                (Type::Var(2), Type::Var(1))
            ],),
            vec![(Type::Var(1), Type::I32), (Type::Var(2), Type::I32)]
        );
    }

    #[test]
    fn it_skips_duplicate_substitutions() {
        assert_eq!(
            solve_constraints(vec![(Type::Var(1), Type::I32), (Type::Var(1), Type::I32)],),
            vec![(Type::Var(1), Type::I32)]
        );
    }

    #[derive(Debug, PartialEq)]
    pub struct TypeCheckFailure {
        loc: Location,
        message: String,
    }

    #[derive(Debug, PartialEq)]
    pub struct TypeCheckResult {
        pub program: ast::Program,
        pub scopes: HashMap<ScopeId, Scope<NodeId>>,
    }

    /*

    pub fn typecheck_identifier(
        node: &parser::Identifier,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<ast::Identifier, TypeCheckFailure> {
        let scope = scopes
            .get(&node.scope)
            .and_then(|scope| scope.bindings().get(&node.value));
        Err(TypeCheckFailure {
            loc: node.location(),
            message: String::from("Program does not typecheck"),
        })
    }

    pub fn typecheck_declaration(
        node: &parser::Declaration,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<ast::Declaration, TypeCheckFailure> {
        typecheck_identifier(&node.id, scopes)
    }

    pub fn typecheck_statement(
        node: &parser::Statement,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<ast::Statement, TypeCheckFailure> {
        match node {
            parser::Statement::Declaration(d) => {
                typecheck_declaration(d, scopes).map(|typed_d| ast::Statement::Declaration(typed_d))
            }
        }
    }

    pub fn typecheck_program(
        node: &parser::Program,
        scopes: &HashMap<ScopeId, Scope<NodeId>>,
    ) -> Result<ast::Program, TypeCheckFailure> {
        (&node.statements)
            .into_iter()
            .map(|s| typecheck_statement(&s, scopes))
            .collect::<Result<Vec<ast::Statement>, TypeCheckFailure>>()
            .map(|ss| ast::Program {
                statements: ss,
                uid: node.uid,
                loc: node.location(),
                scope: node.scope,
            })
    }

    pub fn typecheck(res: parser::ParseResult) -> Result<TypeCheckResult, TypeCheckFailure> {
        typecheck_program(&res.program, &res.scopes).map(|program| TypeCheckResult {
            program,
            scopes: res.scopes,
        })
    }

    // fn gen_con_declaration(node: &Declaration) -> Constraint {
    //     result
    //         .program
    //         .statements
    //         .into_iter()
    //         .map(|s| match s {})
    //         .collect()
    // }
    */

    /*
    fn generate_constraints(result: &ParseResult) -> Vec<Constraint> {
        result
            .program
            .statements
            .into_iter()
            .map(|s| match s {
                Statement::Declaration(Declaration { id, value, .. }) => Constraint::Binding(),
            })
            .collect()
    }

    fn unify(a: Type, b: Type) -> Substitution {
        HashMap::new()
    }

    #[test]
    fn it_unifies_binding() {
        use Constraint::*;
        use Type::*;
        assert_eq!(
            unify(vec![Constant(I32)]),
            vec![(Var(1), I32)].into_iter().collect()
        )
    }
    */
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
