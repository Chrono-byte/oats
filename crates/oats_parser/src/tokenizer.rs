//! Tokenizer for Oats source code
//!
//! This module provides tokenization functionality to expose tokens
//! for testing and tooling purposes. The parser uses chumsky which
//! works directly on characters, but we need token-level access for
//! some features.

use std::ops::Range;

/// Represents a token in the Oats language
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    /// The token kind
    pub kind: TokenKind,
    /// The source span (byte range)
    pub span: Range<usize>,
    /// The actual text of the token
    pub text: String,
}

/// Token kinds in the Oats language
#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    // Keywords
    Let,
    Const,
    Mut,
    Function,
    Class,
    Interface,
    Enum,
    Export,
    Import,
    Async,
    Await,
    Return,
    If,
    Else,
    For,
    While,
    Do,
    Switch,
    Case,
    Default,
    Break,
    Continue,
    Try,
    Catch,
    Finally,
    Throw,
    New,
    This,
    Super,
    Extends,
    Implements,
    Static,
    Public,
    Private,
    Protected,
    Abstract,
    Declare,
    Debugger,
    Typeof,
    Instanceof,
    In,
    Of,
    As,
    Is,
    Void,
    Null,
    True,
    False,
    Undefined,

    // Identifiers
    Ident(String),

    // Literals
    String(String),
    Number(f64),
    Boolean(bool),

    // Operators
    Plus,             // +
    Minus,            // -
    Star,             // *
    Slash,            // /
    Percent,          // %
    PlusPlus,         // ++
    MinusMinus,       // --
    PlusEq,           // +=
    MinusEq,          // -=
    StarEq,           // *=
    SlashEq,          // /=
    PercentEq,        // %=
    Eq,               // =
    EqEq,             // ==
    NotEq,            // !=
    EqEqEq,           // ===
    NotEqEq,          // !==
    Lt,               // <
    Gt,               // >
    LtEq,             // <=
    GtEq,             // >=
    LtLt,             // <<
    GtGt,             // >>
    GtGtGt,           // >>>
    LtLtEq,           // <<=
    GtGtEq,           // >>=
    GtGtGtEq,         // >>>=
    And,              // &
    Or,               // |
    Xor,              // ^
    AndEq,            // &=
    OrEq,             // |=
    XorEq,            // ^=
    AndAnd,           // &&
    OrOr,             // ||
    Not,              // !
    Tilde,            // ~
    Question,         // ?
    Colon,            // :
    QuestionDot,      // ?.
    DoubleQuestion,   // ??
    DoubleQuestionEq, // ??=
    StarStar,         // **
    StarStarEq,       // **=

    // Punctuation
    Semicolon,    // ;
    Comma,        // ,
    Dot,          // .
    DotDotDot,    // ...
    LeftParen,    // (
    RightParen,   // )
    LeftBracket,  // [
    RightBracket, // ]
    LeftBrace,    // {
    RightBrace,   // }
    Arrow,        // =>
    At,           // @

    // Comments and whitespace (usually filtered out)
    LineComment(String),
    BlockComment(String),
    Whitespace,

    // Other
    Unknown,
    Eof,
}

/// Tokenize a source string into a vector of tokens
pub fn tokenize(source: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = source.char_indices().peekable();

    while let Some((start, ch)) = chars.next() {
        let pos = start;

        match ch {
            // Whitespace
            ' ' | '\t' | '\r' | '\n' => {
                let mut end = pos + ch.len_utf8();
                while let Some((next_pos, next_ch)) = chars.peek().copied() {
                    if matches!(next_ch, ' ' | '\t' | '\r' | '\n') {
                        chars.next();
                        end = next_pos + next_ch.len_utf8();
                    } else {
                        break;
                    }
                }
                tokens.push(Token {
                    kind: TokenKind::Whitespace,
                    span: pos..end,
                    text: source[pos..end].to_string(),
                });
            }

            // Line comment
            '/' if matches!(chars.peek(), Some((_, '/'))) => {
                chars.next(); // consume second '/'
                let mut end = pos + 2;
                let mut comment = String::new();
                for (next_pos, next_ch) in chars.by_ref() {
                    end = next_pos + next_ch.len_utf8();
                    if next_ch == '\n' {
                        break;
                    }
                    comment.push(next_ch);
                }
                tokens.push(Token {
                    kind: TokenKind::LineComment(comment),
                    span: pos..end,
                    text: source[pos..end].to_string(),
                });
            }

            // Block comment
            '/' if matches!(chars.peek(), Some((_, '*'))) => {
                chars.next(); // consume '*'
                let mut end = pos + 2;
                let mut comment = String::new();
                let mut prev = '/';
                for (next_pos, next_ch) in chars.by_ref() {
                    end = next_pos + next_ch.len_utf8();
                    if prev == '*' && next_ch == '/' {
                        break;
                    }
                    comment.push(next_ch);
                    prev = next_ch;
                }
                tokens.push(Token {
                    kind: TokenKind::BlockComment(comment),
                    span: pos..end,
                    text: source[pos..end].to_string(),
                });
            }

            // String literals
            '"' | '\'' => {
                let quote = ch;
                let mut end = pos + 1;
                let mut value = String::new();
                let mut escaped = false;

                for (next_pos, next_ch) in chars.by_ref() {
                    end = next_pos + next_ch.len_utf8();
                    if escaped {
                        value.push(next_ch);
                        escaped = false;
                    } else if next_ch == '\\' {
                        escaped = true;
                    } else if next_ch == quote {
                        break;
                    } else {
                        value.push(next_ch);
                    }
                }

                tokens.push(Token {
                    kind: TokenKind::String(value),
                    span: pos..end,
                    text: source[pos..end].to_string(),
                });
            }

            // Numbers
            '0'..='9' => {
                let mut end = pos + ch.len_utf8();
                let mut num_str = String::from(ch);
                let mut has_dot = false;

                while let Some((next_pos, next_ch)) = chars.peek().copied() {
                    match next_ch {
                        '0'..='9' => {
                            chars.next();
                            end = next_pos + next_ch.len_utf8();
                            num_str.push(next_ch);
                        }
                        '.' if !has_dot => {
                            chars.next();
                            end = next_pos + 1;
                            num_str.push('.');
                            has_dot = true;
                        }
                        _ => break,
                    }
                }

                let num = num_str.parse::<f64>().unwrap_or(0.0);
                tokens.push(Token {
                    kind: TokenKind::Number(num),
                    span: pos..end,
                    text: num_str,
                });
            }

            // Operators and punctuation
            '+' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '+'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::PlusPlus,
                        span: pos..pos + 2,
                        text: "++".to_string(),
                    });
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::PlusEq,
                        span: pos..pos + 2,
                        text: "+=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Plus,
                        span: pos..end,
                        text: "+".to_string(),
                    });
                }
            }

            '-' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '>'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::Arrow,
                        span: pos..pos + 2,
                        text: "=>".to_string(),
                    });
                } else if matches!(chars.peek(), Some((_, '-'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::MinusMinus,
                        span: pos..pos + 2,
                        text: "--".to_string(),
                    });
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::MinusEq,
                        span: pos..pos + 2,
                        text: "-=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Minus,
                        span: pos..end,
                        text: "-".to_string(),
                    });
                }
            }

            '*' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '*'))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::StarStarEq,
                            span: pos..pos + 3,
                            text: "**=".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::StarStar,
                            span: pos..pos + 2,
                            text: "**".to_string(),
                        });
                    }
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::StarEq,
                        span: pos..pos + 2,
                        text: "*=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Star,
                        span: pos..end,
                        text: "*".to_string(),
                    });
                }
            }

            '/' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::SlashEq,
                        span: pos..pos + 2,
                        text: "/=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Slash,
                        span: pos..end,
                        text: "/".to_string(),
                    });
                }
            }

            '%' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::PercentEq,
                        span: pos..pos + 2,
                        text: "%=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Percent,
                        span: pos..end,
                        text: "%".to_string(),
                    });
                }
            }

            '=' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::EqEqEq,
                            span: pos..pos + 3,
                            text: "===".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::EqEq,
                            span: pos..pos + 2,
                            text: "==".to_string(),
                        });
                    }
                } else if matches!(chars.peek(), Some((_, '>'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::Arrow,
                        span: pos..pos + 2,
                        text: "=>".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Eq,
                        span: pos..end,
                        text: "=".to_string(),
                    });
                }
            }

            '!' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::NotEqEq,
                            span: pos..pos + 3,
                            text: "!==".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::NotEq,
                            span: pos..pos + 2,
                            text: "!=".to_string(),
                        });
                    }
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Not,
                        span: pos..end,
                        text: "!".to_string(),
                    });
                }
            }

            '<' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '<'))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::LtLtEq,
                            span: pos..pos + 3,
                            text: "<<=".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::LtLt,
                            span: pos..pos + 2,
                            text: "<<".to_string(),
                        });
                    }
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::LtEq,
                        span: pos..pos + 2,
                        text: "<=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Lt,
                        span: pos..end,
                        text: "<".to_string(),
                    });
                }
            }

            '>' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '>'))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '>'))) {
                        chars.next();
                        if matches!(chars.peek(), Some((_, '='))) {
                            chars.next();
                            tokens.push(Token {
                                kind: TokenKind::GtGtGtEq,
                                span: pos..pos + 4,
                                text: ">>>=".to_string(),
                            });
                        } else {
                            tokens.push(Token {
                                kind: TokenKind::GtGtGt,
                                span: pos..pos + 3,
                                text: ">>>".to_string(),
                            });
                        }
                    } else if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::GtGtEq,
                            span: pos..pos + 3,
                            text: ">>=".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::GtGt,
                            span: pos..pos + 2,
                            text: ">>".to_string(),
                        });
                    }
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::GtEq,
                        span: pos..pos + 2,
                        text: ">=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Gt,
                        span: pos..end,
                        text: ">".to_string(),
                    });
                }
            }

            '&' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '&'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::AndAnd,
                        span: pos..pos + 2,
                        text: "&&".to_string(),
                    });
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::AndEq,
                        span: pos..pos + 2,
                        text: "&=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::And,
                        span: pos..end,
                        text: "&".to_string(),
                    });
                }
            }

            '|' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '|'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::OrOr,
                        span: pos..pos + 2,
                        text: "||".to_string(),
                    });
                } else if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::OrEq,
                        span: pos..pos + 2,
                        text: "|=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Or,
                        span: pos..end,
                        text: "|".to_string(),
                    });
                }
            }

            '^' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '='))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::XorEq,
                        span: pos..pos + 2,
                        text: "^=".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Xor,
                        span: pos..end,
                        text: "^".to_string(),
                    });
                }
            }

            '?' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '?'))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '='))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::DoubleQuestionEq,
                            span: pos..pos + 3,
                            text: "??=".to_string(),
                        });
                    } else {
                        tokens.push(Token {
                            kind: TokenKind::DoubleQuestion,
                            span: pos..pos + 2,
                            text: "??".to_string(),
                        });
                    }
                } else if matches!(chars.peek(), Some((_, '.'))) {
                    chars.next();
                    tokens.push(Token {
                        kind: TokenKind::QuestionDot,
                        span: pos..pos + 2,
                        text: "?.".to_string(),
                    });
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Question,
                        span: pos..end,
                        text: "?".to_string(),
                    });
                }
            }

            ':' => {
                tokens.push(Token {
                    kind: TokenKind::Colon,
                    span: pos..pos + 1,
                    text: ":".to_string(),
                });
            }

            ';' => {
                tokens.push(Token {
                    kind: TokenKind::Semicolon,
                    span: pos..pos + 1,
                    text: ";".to_string(),
                });
            }

            ',' => {
                tokens.push(Token {
                    kind: TokenKind::Comma,
                    span: pos..pos + 1,
                    text: ",".to_string(),
                });
            }

            '.' => {
                let end = pos + 1;
                if matches!(chars.peek(), Some((_, '.'))) {
                    chars.next();
                    if matches!(chars.peek(), Some((_, '.'))) {
                        chars.next();
                        tokens.push(Token {
                            kind: TokenKind::DotDotDot,
                            span: pos..pos + 3,
                            text: "...".to_string(),
                        });
                    } else {
                        // Invalid, but tokenize as two dots
                        tokens.push(Token {
                            kind: TokenKind::Dot,
                            span: pos..end,
                            text: ".".to_string(),
                        });
                    }
                } else {
                    tokens.push(Token {
                        kind: TokenKind::Dot,
                        span: pos..end,
                        text: ".".to_string(),
                    });
                }
            }

            '(' => {
                tokens.push(Token {
                    kind: TokenKind::LeftParen,
                    span: pos..pos + 1,
                    text: "(".to_string(),
                });
            }

            ')' => {
                tokens.push(Token {
                    kind: TokenKind::RightParen,
                    span: pos..pos + 1,
                    text: ")".to_string(),
                });
            }

            '[' => {
                tokens.push(Token {
                    kind: TokenKind::LeftBracket,
                    span: pos..pos + 1,
                    text: "[".to_string(),
                });
            }

            ']' => {
                tokens.push(Token {
                    kind: TokenKind::RightBracket,
                    span: pos..pos + 1,
                    text: "]".to_string(),
                });
            }

            '{' => {
                tokens.push(Token {
                    kind: TokenKind::LeftBrace,
                    span: pos..pos + 1,
                    text: "{".to_string(),
                });
            }

            '}' => {
                tokens.push(Token {
                    kind: TokenKind::RightBrace,
                    span: pos..pos + 1,
                    text: "}".to_string(),
                });
            }

            '@' => {
                tokens.push(Token {
                    kind: TokenKind::At,
                    span: pos..pos + 1,
                    text: "@".to_string(),
                });
            }

            '~' => {
                tokens.push(Token {
                    kind: TokenKind::Tilde,
                    span: pos..pos + 1,
                    text: "~".to_string(),
                });
            }

            // Identifiers and keywords
            c if c.is_alphabetic() || c == '_' || c == '$' => {
                let mut end = pos + c.len_utf8();
                let mut ident = String::from(c);

                while let Some((next_pos, next_ch)) = chars.peek().copied() {
                    if next_ch.is_alphanumeric() || next_ch == '_' || next_ch == '$' {
                        chars.next();
                        end = next_pos + next_ch.len_utf8();
                        ident.push(next_ch);
                    } else {
                        break;
                    }
                }

                let kind = match ident.as_str() {
                    "let" => TokenKind::Let,
                    "const" => TokenKind::Const,
                    "mut" => TokenKind::Mut,
                    "function" => TokenKind::Function,
                    "class" => TokenKind::Class,
                    "interface" => TokenKind::Interface,
                    "enum" => TokenKind::Enum,
                    "export" => TokenKind::Export,
                    "import" => TokenKind::Import,
                    "async" => TokenKind::Async,
                    "await" => TokenKind::Await,
                    "return" => TokenKind::Return,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "for" => TokenKind::For,
                    "while" => TokenKind::While,
                    "do" => TokenKind::Do,
                    "switch" => TokenKind::Switch,
                    "case" => TokenKind::Case,
                    "default" => TokenKind::Default,
                    "break" => TokenKind::Break,
                    "continue" => TokenKind::Continue,
                    "try" => TokenKind::Try,
                    "catch" => TokenKind::Catch,
                    "finally" => TokenKind::Finally,
                    "throw" => TokenKind::Throw,
                    "new" => TokenKind::New,
                    "this" => TokenKind::This,
                    "super" => TokenKind::Super,
                    "extends" => TokenKind::Extends,
                    "implements" => TokenKind::Implements,
                    "static" => TokenKind::Static,
                    "public" => TokenKind::Public,
                    "private" => TokenKind::Private,
                    "protected" => TokenKind::Protected,
                    "abstract" => TokenKind::Abstract,
                    "declare" => TokenKind::Declare,
                    "debugger" => TokenKind::Debugger,
                    "typeof" => TokenKind::Typeof,
                    "instanceof" => TokenKind::Instanceof,
                    "in" => TokenKind::In,
                    "of" => TokenKind::Of,
                    "as" => TokenKind::As,
                    "is" => TokenKind::Is,
                    "void" => TokenKind::Void,
                    "null" => TokenKind::Null,
                    "true" => TokenKind::True,
                    "false" => TokenKind::False,
                    "undefined" => TokenKind::Undefined,
                    _ => TokenKind::Ident(ident.clone()),
                };

                tokens.push(Token {
                    kind,
                    span: pos..end,
                    text: ident,
                });
            }

            _ => {
                tokens.push(Token {
                    kind: TokenKind::Unknown,
                    span: pos..pos + ch.len_utf8(),
                    text: ch.to_string(),
                });
            }
        }
    }

    // Add EOF token
    tokens.push(Token {
        kind: TokenKind::Eof,
        span: source.len()..source.len(),
        text: String::new(),
    });

    tokens
}

/// Filter out whitespace and comments from a token stream
pub fn filter_whitespace_and_comments(tokens: Vec<Token>) -> Vec<Token> {
    tokens
        .into_iter()
        .filter(|t| {
            !matches!(
                t.kind,
                TokenKind::Whitespace | TokenKind::LineComment(_) | TokenKind::BlockComment(_)
            )
        })
        .collect()
}
