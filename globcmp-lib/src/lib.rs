use multipeek::{multipeek, MultiPeek};
use std::{fmt, str::FromStr};

#[cfg(feature = "stack-safe")]
const STACK_RED_ZONE: usize = 32 * 1024;
#[cfg(feature = "stack-safe")]
const STACK_GROW_SIZE: usize = 1024 * 1024;

#[derive(Debug, PartialEq, Eq, Clone)]
enum Token {
    Slash,
    Char(char),
    CharClass(Vec<char>),
    AnyChar,
    AnyChars,
    AnyRecur,
}

#[derive(Debug, PartialEq, Eq)]
pub enum PatternError {
    UnmatchedOpenBracket,
    UnmatchedCloseBracket,
    EmptyCharClass,
    InvalidCharInClass,
    InvalidRecursive,
}

impl fmt::Display for PatternError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        use PatternError::*;

        match self {
            UnmatchedOpenBracket => "unmatched opening bracket",
            UnmatchedCloseBracket => "unmatched closing bracket",
            EmptyCharClass => "empty character class",
            InvalidCharInClass => {
                "invalid character in character class"
            }
            InvalidRecursive => "invalid recursive pattern",
        }
        .fmt(f)
    }
}

impl std::error::Error for PatternError {}

#[derive(Debug)]
struct PatternIterator<T>
where
    T: Iterator<Item = Token> + Clone,
{
    tokens: MultiPeek<T>,
}

impl<T> PatternIterator<T>
where
    T: Iterator<Item = Token> + Clone,
{
    /// This method is private in order to preserve the invariant that
    /// `TokenizedPattern` represents a valid pattern.
    ///
    /// Use [`TokenizedPattern::from_str`] instead to create a
    /// `TokenizedPattern` from a string.
    fn from(tokens: MultiPeek<T>) -> Self {
        Self { tokens }
    }

    fn is_more_specific_than(self, other: Self) -> bool {
        let mut a_iter = self.tokens;
        let mut b_iter = other.tokens;

        fn recur_moving_a<T>(
            a_iter: &MultiPeek<T>,
            b_iter: &MultiPeek<T>,
        ) -> bool
        where
            T: Iterator<Item = Token> + Clone,
        {
            let mut a_clone = a_iter.clone();
            let b_clone = b_iter.clone();

            a_clone.next();

            PatternIterator::from(a_clone)
                .is_more_specific_than(PatternIterator::from(b_clone))
        }

        fn recur_moving_b<T>(
            a_iter: &MultiPeek<T>,
            b_iter: &MultiPeek<T>,
        ) -> bool
        where
            T: Iterator<Item = Token> + Clone,
        {
            let a_clone = a_iter.clone();
            let mut b_clone = b_iter.clone();

            b_clone.next();

            PatternIterator::from(a_clone)
                .is_more_specific_than(PatternIterator::from(b_clone))
        }

        fn recur_moving_both<T>(
            a_iter: &MultiPeek<T>,
            b_iter: &MultiPeek<T>,
        ) -> bool
        where
            T: Iterator<Item = Token> + Clone,
        {
            let mut a_clone = a_iter.clone();
            let mut b_clone = b_iter.clone();

            a_clone.next();
            b_clone.next();

            PatternIterator::from(a_clone)
                .is_more_specific_than(PatternIterator::from(b_clone))
        }

        loop {
            use Token::*;

            let a_token = a_iter.peek();
            let b_token = b_iter.peek();

            match (a_token, b_token) {
                (None, None) => return true,
                (None, Some(AnyChars)) => {
                    b_iter.next();
                }
                (None, Some(_)) | (Some(_), None) => return false,
                (Some(AnyChars), Some(AnyChars))
                | (Some(AnyRecur), Some(AnyRecur)) => {
                    #[cfg(feature = "stack-safe")]
                    return stacker::maybe_grow(
                        STACK_RED_ZONE,
                        STACK_GROW_SIZE,
                        || {
                            recur_moving_a(&a_iter, &b_iter)
                                || recur_moving_b(&a_iter, &b_iter)
                                || recur_moving_both(&a_iter, &b_iter)
                        },
                    );
                    #[cfg(not(feature = "stack-safe"))]
                    return recur_moving_a(&a_iter, &b_iter)
                        || recur_moving_b(&a_iter, &b_iter)
                        || recur_moving_both(&a_iter, &b_iter);
                }
                (Some(Slash), Some(Slash)) => {
                    let a_next = a_iter.peek_nth(1);
                    let b_next = b_iter.peek_nth(1);

                    match (a_next, b_next) {
                        (Some(AnyRecur), Some(AnyRecur)) => {
                            #[cfg(feature = "stack-safe")]
                            return stacker::maybe_grow(
                                STACK_RED_ZONE,
                                STACK_GROW_SIZE,
                                || {
                                    recur_moving_a(&a_iter, &b_iter)
                                        || recur_moving_b(
                                            &a_iter, &b_iter,
                                        )
                                        || recur_moving_both(
                                            &a_iter, &b_iter,
                                        )
                                },
                            );
                            #[cfg(not(feature = "stack-safe"))]
                            return recur_moving_a(&a_iter, &b_iter)
                                || recur_moving_b(&a_iter, &b_iter)
                                || recur_moving_both(&a_iter, &b_iter);
                        }
                        (Some(AnyRecur), _) => {
                            a_iter.next();
                        }
                        (_, Some(AnyRecur)) => {
                            b_iter.next();
                        }
                        _ => {
                            a_iter.next();
                            b_iter.next();
                        }
                    }
                }
                (Some(a), Some(b)) if a == b => {
                    a_iter.next();
                    b_iter.next();
                }
                (Some(AnyChar), Some(CharClass(_)))
                | (Some(AnyChar), Some(Char(_)))
                | (Some(CharClass(_)), Some(Char(_))) => return false,
                (Some(CharClass(_)), Some(AnyChar))
                | (Some(Char(_)), Some(AnyChar)) => {
                    a_iter.next();
                    b_iter.next();
                }
                (Some(Char(a)), Some(CharClass(bs))) => {
                    if !bs.contains(a) {
                        return false;
                    }

                    a_iter.next();
                    b_iter.next();
                }
                (Some(AnyChars), Some(_))
                | (Some(AnyRecur), Some(_)) => return false,
                (Some(Slash), Some(AnyChars)) => {
                    b_iter.next();
                }
                (Some(_), Some(AnyChars)) => {
                    #[cfg(feature = "stack-safe")]
                    return stacker::maybe_grow(
                        STACK_RED_ZONE,
                        STACK_GROW_SIZE,
                        || {
                            recur_moving_a(&a_iter, &b_iter)
                                || recur_moving_b(&a_iter, &b_iter)
                                || recur_moving_both(&a_iter, &b_iter)
                        },
                    );
                    #[cfg(not(feature = "stack-safe"))]
                    return recur_moving_a(&a_iter, &b_iter)
                        || recur_moving_b(&a_iter, &b_iter)
                        || recur_moving_both(&a_iter, &b_iter);
                }
                (Some(Slash), Some(AnyRecur)) => {
                    #[cfg(feature = "stack-safe")]
                    return stacker::maybe_grow(
                        STACK_RED_ZONE,
                        STACK_GROW_SIZE,
                        || {
                            recur_moving_a(&a_iter, &b_iter)
                                || recur_moving_b(&a_iter, &b_iter)
                        },
                    );
                    #[cfg(not(feature = "stack-safe"))]
                    return recur_moving_a(&a_iter, &b_iter)
                        || recur_moving_b(&a_iter, &b_iter);
                }
                (Some(_), Some(AnyRecur)) => {
                    a_iter.next();
                }
                (Some(_), Some(_)) => return false,
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Pattern {
    original: String,
    tokens: Vec<Token>,
}

impl Pattern {
    /// `self` is weakly more specific than `other`.
    ///
    /// If `self` and `other` are not comparable, then this method
    /// returns false both ways.  If `self` and `other` are the same,
    /// then it returns true both ways.
    ///
    /// # Examples
    ///
    /// TODO
    pub fn is_more_specific_than(&self, other: &Self) -> bool {
        let self_tokens = self.tokens.to_vec();
        let other_tokens = other.tokens.to_vec();

        PatternIterator::from(multipeek(self_tokens))
            .is_more_specific_than(PatternIterator::from(multipeek(
                other_tokens,
            )))
    }

    pub fn as_str(&self) -> &str {
        self.original.as_str()
    }
}

impl FromStr for Pattern {
    type Err = PatternError;

    /// Tries to convert a pattern into a valid `Pattern`.
    ///
    /// # Invariants
    ///
    /// TODO
    ///
    /// # Examples
    ///
    /// TODO with assert!(...is_ok()) and assert!(...is_err()).
    fn from_str(value: &str) -> Result<Self, Self::Err> {
        let mut tokens = Vec::new();
        let mut class_buf = Vec::new();
        let mut in_char_class = false;
        let mut pending_star = false;

        for char in value.chars() {
            if char == '*' {
                if !pending_star {
                    pending_star = true;
                } else {
                    tokens.push(Token::AnyRecur);
                    pending_star = false;
                }
                continue;
            } else if pending_star {
                tokens.push(Token::AnyChars);
                pending_star = false;
            }

            let token = match char {
                '/' => Token::Slash,
                '?' => Token::AnyChar,
                '[' => {
                    if in_char_class {
                        return Err(Self::Err::InvalidCharInClass);
                    } else {
                        in_char_class = true;
                        continue;
                    }
                }
                ']' => {
                    if !in_char_class {
                        return Err(Self::Err::UnmatchedCloseBracket);
                    } else if class_buf.is_empty() {
                        return Err(Self::Err::EmptyCharClass);
                    } else {
                        in_char_class = false;
                        Token::CharClass(std::mem::take(&mut class_buf))
                    }
                }
                _ => {
                    if in_char_class {
                        class_buf.push(char);
                        continue;
                    } else {
                        Token::Char(char)
                    }
                }
            };

            tokens.push(token);
        }

        if in_char_class {
            return Err(Self::Err::UnmatchedOpenBracket);
        }

        if pending_star {
            tokens.push(Token::AnyChars);
        }

        // Validate the usage of `**`.  It should never be preceded or
        // succeeded by any token other than `/`.
        //

        let mut prev_token: Option<&Token> = None;

        for token in tokens.iter() {
            if *token == Token::AnyRecur
                && let Some(p) = prev_token
                && *p != Token::Slash
            {
                return Err(Self::Err::InvalidRecursive);
            } else if prev_token == Some(&Token::AnyRecur)
                && *token != Token::Slash
            {
                return Err(Self::Err::InvalidRecursive);
            }

            prev_token = Some(token);
        }

        Ok(Self {
            original: value.to_string(),
            tokens,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_empty_str() {
        assert_eq!(
            Pattern::from_str(""),
            Ok(Pattern {
                original: String::new(),
                tokens: Vec::new()
            })
        );
    }

    #[test]
    fn tokenize_exact() {
        assert_eq!(
            Pattern::from_str("foo/bar"),
            Ok(Pattern {
                original: "foo/bar".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::Char('b'),
                    Token::Char('a'),
                    Token::Char('r'),
                ]
            })
        );
    }

    #[test]
    fn tokenize_any_char() {
        assert_eq!(
            Pattern::from_str("foo/b?r"),
            Ok(Pattern {
                original: "foo/b?r".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::Char('b'),
                    Token::AnyChar,
                    Token::Char('r'),
                ]
            })
        );
    }

    #[test]
    fn tokenize_any_chars() {
        assert_eq!(
            Pattern::from_str("foo/b*r"),
            Ok(Pattern {
                original: "foo/b*r".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::Char('b'),
                    Token::AnyChars,
                    Token::Char('r'),
                ]
            })
        );

        assert_eq!(
            Pattern::from_str("foo/*b*r*"),
            Ok(Pattern {
                original: "foo/*b*r*".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::AnyChars,
                    Token::Char('b'),
                    Token::AnyChars,
                    Token::Char('r'),
                    Token::AnyChars,
                ]
            })
        );
    }

    #[test]
    fn tokenize_valid_char_class() {
        assert_eq!(
            Pattern::from_str("foo/ba[rz]"),
            Ok(Pattern {
                original: "foo/ba[rz]".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::Char('b'),
                    Token::Char('a'),
                    Token::CharClass(vec!['r', 'z']),
                ]
            })
        );
    }

    #[test]
    fn tokenize_empty_char_class() {
        assert_eq!(
            Pattern::from_str("foo/ba[]"),
            Err(PatternError::EmptyCharClass)
        );
    }

    #[test]
    fn tokenize_unclosed_char_class() {
        assert_eq!(
            Pattern::from_str("foo/ba[rz"),
            Err(PatternError::UnmatchedOpenBracket)
        );
    }

    #[test]
    fn tokenize_unopened_char_class() {
        assert_eq!(
            Pattern::from_str("foo/barz]"),
            Err(PatternError::UnmatchedCloseBracket)
        );
    }

    #[test]
    fn tokenize_invalid_char_class() {
        assert_eq!(
            Pattern::from_str("foo/ba[[rz]"),
            Err(PatternError::InvalidCharInClass)
        );
    }

    #[test]
    fn tokenize_valid_recursive() {
        assert_eq!(
            Pattern::from_str("foo/**/baz"),
            Ok(Pattern {
                original: "foo/**/baz".to_string(),
                tokens: vec![
                    Token::Char('f'),
                    Token::Char('o'),
                    Token::Char('o'),
                    Token::Slash,
                    Token::AnyRecur,
                    Token::Slash,
                    Token::Char('b'),
                    Token::Char('a'),
                    Token::Char('z'),
                ]
            })
        );
    }

    #[test]
    fn tokenize_invalid_recursive() {
        for pattern in [
            "x**/bar/baz",
            "**x/bar/baz",
            "***/bar/baz",
            "foo/**x/baz",
            "foo/x**/baz",
            "foo/***/baz",
            "foo/bar/x**",
            "foo/bar/**x",
            "foo/bar/***",
        ] {
            assert_eq!(
                Pattern::from_str(dbg!(pattern)),
                Err(PatternError::InvalidRecursive)
            );
        }
    }

    macro_rules! same_specificity {
        ($a:expr, $b:expr) => {
            let a_str = $a;
            let a_pattern = Pattern::from_str(a_str)
                .expect("first pattern should be valid");

            let b_str = $b;
            let b_pattern = Pattern::from_str(b_str)
                .expect("second pattern should be valid");

            assert!(
                a_pattern.is_more_specific_than(&b_pattern),
                "{a_str:?} should be more specific than {b_str:?}"
            );
            assert!(
                b_pattern.is_more_specific_than(&a_pattern),
                "{b_str:?} should be more specific than {a_str:?}"
            );
        };
    }

    macro_rules! more_specific {
        ($a:expr, $b:expr) => {
            let a_str = $a;
            let a_pattern = Pattern::from_str(a_str)
                .expect("first pattern should be valid");

            let b_str = $b;
            let b_pattern = Pattern::from_str(b_str)
                .expect("second pattern should be valid");

            assert!(
                a_pattern.is_more_specific_than(&b_pattern),
                "{a_str:?} should be more specific than {b_str:?}"
            );
            assert!(
                !b_pattern.is_more_specific_than(&a_pattern),
                "{b_str:?} should not be more specific than {a_str:?}"
            );
        };
    }

    macro_rules! unknown_specificity {
        ($a:expr, $b:expr) => {
            let a_str = $a;
            let a_pattern = Pattern::from_str(a_str)
                .expect("first pattern should be valid");

            let b_str = $b;
            let b_pattern = Pattern::from_str(b_str)
                .expect("second pattern should be valid");

            assert!(
                !a_pattern.is_more_specific_than(&b_pattern),
                "{a_str:?} should not be more specific than {b_str:?}"
            );
            assert!(
                !b_pattern.is_more_specific_than(&a_pattern),
                "{b_str:?} should not be more specific than {a_str:?}"
            );
        };
    }

    #[test]
    fn pattern_specificity() {
        same_specificity!("foo", "foo");
        same_specificity!("foo/bar", "foo/bar");

        // Any character.
        for pattern in [
            "?oo/bar", "??o/bar", "???/bar", "f?o/bar", "fo?/bar",
            "foo/?ar", "foo/??r", "foo/???", "foo/b?r", "foo/ba?",
        ] {
            // Versus exact path.
            more_specific!("foo/bar", dbg!(pattern));

            // Versus same any-character pattern.
            same_specificity!(pattern, pattern);
        }

        // Wildcard.
        for pattern in [
            // Superfluous wildcard.
            "*foo/bar", "f*oo/bar", "foo*/bar", "foo/*bar", "foo/b*ar",
            "foo/bar*",
            // Wildcard matching a single character.
            "*oo/bar", "*o*/bar", "f*o/bar", "fo*/bar", "foo/*ar",
            "foo/*a*", "foo/b*r", "foo/ba*",
            // Wildcard matching multiple characters.
            "*/bar", "foo/*",
        ] {
            more_specific!("foo/bar", dbg!(pattern));
            same_specificity!(pattern, pattern);
        }

        // Recursive wildcard.
        for pattern in ["**/bar", "foo/**/bar"] {
            more_specific!("foo/bar", dbg!(pattern));
            same_specificity!(pattern, pattern);
        }

        more_specific!("foo/**/baz/lorem", "foo/**/lorem");
        more_specific!("foo/bar/**/lorem", "foo/**/lorem");
        more_specific!("foo/bar/**/lorem", "**/lorem");
        more_specific!("foo/**/bar/baz", "**/bar/baz");

        // Wildcard at shallower vs. deeper level.
        more_specific!("foo/*/*", "*/*/*");
        more_specific!("foo/bar/*", "*/*/*");
        more_specific!("foo/bar/*", "foo/*/*");
        more_specific!("foo/bar/baz", "*/*/*");
        more_specific!("foo/bar/baz", "foo/*/*");
        more_specific!("foo/bar/baz", "foo/bar/*");
        more_specific!("foo/bar/baz", "foo/bar/b*");
        more_specific!("foo/bar/baz", "foo/bar/ba*");
        more_specific!("foo/bar/baz", "foo/bar/baz*");
        more_specific!("foo/bar/baz", "foo/bar/*z");
        more_specific!("foo/bar/baz", "foo/bar/*az");
        more_specific!("foo/bar/baz", "foo/bar/*baz");
        more_specific!("foo/bar/baz", "foo/bar/b*az");
        more_specific!("foo/bar/baz", "foo/bar/ba*z");
        more_specific!("foo/bar/baz", "foo/bar/b*a*z");
        more_specific!("foo/bar/baz", "foo/bar/*b*a*z*");
        more_specific!("foo/bar/baz", "foo/b?r/baz");
        more_specific!("foo/bar/baz", "foo/b??/???");
        more_specific!("foo/bar/???", "foo/b??/???");
        more_specific!("foo/b??/baz", "foo/b??/???");
        more_specific!("foo/ba?/???", "foo/b??/???");

        // NOTE We could argue that "**/bar" matches at a lower level
        // than "foo/**/*", and that it is thus more specific.  We don't
        // do this though.
        unknown_specificity!("**/bar", "foo/*");
        unknown_specificity!("**/bar", "foo/**/*");
        unknown_specificity!("**/bar/baz", "foo/**/baz");

        // NOTE If we could compare the patterns against a concrete
        // path, e.g., "lorem/foo/bar/baz", then we could argue that
        // "**/foo/**/*" matches at a higher level than "**/bar/**/*",
        // and thus it is less specific.  But not having a concrete path
        // to compare against, we cannot say anything about specificity.
        unknown_specificity!("**/foo/**/*", "**/bar/**/*");

        more_specific!("foo/b*r", "foo/*r");
        more_specific!("foo/b*r", "foo/*");
        more_specific!("foo/b?r", "foo/b*r");

        // Character classes.
        more_specific!("foo/bar", "foo/ba[rz]");
        more_specific!("foo/ba[rz]", "foo/ba?");
        more_specific!("foo/ba[rz]", "foo/ba*");
        more_specific!("foo/ba?", "foo/ba*");

        // Wildcard placement in the filename.
        more_specific!("foo/bar.*", "foo/*");
        more_specific!("foo/*.c", "foo/*");
        unknown_specificity!("foo/bar.*", "foo/*.c");
        more_specific!("**/bar.*", "**/*");
        more_specific!("**/*.c", "**/*");
        unknown_specificity!("**/bar.*", "**/*.c");
        unknown_specificity!("foo/**/baz.*", "**/bar/*.c");

        // Patterns that don't match the same paths.
        unknown_specificity!("foo/bar", "lorem/ipsum");
        unknown_specificity!("foo/ba[rz]", "foo/bax");
        unknown_specificity!("foo/ba?", "foo/baxx");
        unknown_specificity!("foo/*", "foo/bar/*");
    }
}
