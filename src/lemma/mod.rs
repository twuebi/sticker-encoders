use std::fmt;

use conllx::graph::{Node, Sentence};
use edit_tree::{Apply, EditTree as EditTreeInner};
use failure::{Error, Fail};
use serde_derive::{Deserialize, Serialize};

use super::{EncodingProb, SentenceDecoder, SentenceEncoder};

/// Lemma encoding error.
#[derive(Clone, Debug, Eq, Fail, PartialEq)]
pub enum EncodeError {
    /// The token does not have a lemma.
    #[fail(display = "token without a lemma: '{}'", form)]
    MissingLemma { form: String },
}

/// Encoding of a lemmatization as an edit tree.
#[derive(Clone, Deserialize, Debug, Eq, Hash, PartialEq, Serialize)]
pub struct EditTree {
    inner: EditTreeInner<char>,
}

impl EditTree {
    /// Pretty print an edit tree.
    ///
    /// This is a pretty printer for edit trees that converts them to
    /// an S-expr. It is not optimized for efficieny and does a lot of
    /// string allocations.
    fn pretty_print(node: &EditTreeInner<char>) -> String {
        match node {
            EditTreeInner::MatchNode {
                pre,
                suf,
                left,
                right,
            } => {
                let left_str = left
                    .as_ref()
                    .map(|left| Self::pretty_print(left))
                    .unwrap_or_else(|| "()".to_string());
                let right_str = right
                    .as_ref()
                    .map(|right| Self::pretty_print(right))
                    .unwrap_or_else(|| "()".to_string());

                format!("(match {} {} {} {})", pre, suf, left_str, right_str)
            }
            EditTreeInner::ReplaceNode {
                replacee,
                replacement,
            } => format!(
                "(replace \"{}\" \"{}\")",
                replacee.iter().collect::<String>(),
                replacement.iter().collect::<String>(),
            ),
        }
    }
}

impl fmt::Display for EditTree {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", Self::pretty_print(&self.inner))
    }
}

/// Back-off strategy.
///
/// This is the strategy that will be used when an edit tree
/// could not be applied.
pub enum BackoffStrategy {
    Nothing,
    Form,
}

/// Edit tree-based lemma encoder.
///
/// This encoder encodes a lemma as an edit tree that is applied to an
/// unlemmatized form.
pub struct EditTreeEncoder {
    backoff_strategy: BackoffStrategy,
}

impl EditTreeEncoder {
    pub fn new(backoff_strategy: BackoffStrategy) -> Self {
        EditTreeEncoder { backoff_strategy }
    }
}

impl SentenceDecoder for EditTreeEncoder {
    type Encoding = EditTree;

    fn decode<S>(&self, labels: &[S], sentence: &mut Sentence) -> Result<(), Error>
    where
        S: AsRef<[EncodingProb<Self::Encoding>]>,
    {
        assert_eq!(
            labels.len(),
            sentence.len() - 1,
            "Labels and sentence length mismatch"
        );

        for (token, token_labels) in sentence
            .iter_mut()
            .filter_map(Node::token_mut)
            .zip(labels.iter())
        {
            if let Some(label) = token_labels.as_ref().get(0) {
                let form = token.form().chars().collect::<Vec<_>>();

                if let Some(lemma) = label.encoding().inner.apply(&form) {
                    // If the edit script can be applied, use the
                    // resulting lemma...
                    let lemma = lemma.into_iter().collect::<String>();
                    token.set_lemma(Some(lemma));
                } else if let BackoffStrategy::Form = self.backoff_strategy {
                    // .. if the edit script failed and the back-off
                    // strategy is to set the form as the lemma,
                    // do so.
                    token.set_lemma(Some(token.form().to_owned()));
                }
            }
        }

        Ok(())
    }
}

impl SentenceEncoder for EditTreeEncoder {
    type Encoding = EditTree;

    fn encode(&self, sentence: &Sentence) -> Result<Vec<Self::Encoding>, Error> {
        let mut encoding = Vec::with_capacity(sentence.len() - 1);

        for token in sentence.iter().filter_map(Node::token) {
            let lemma = token.lemma().ok_or_else(|| EncodeError::MissingLemma {
                form: token.form().to_owned(),
            })?;

            let tree = EditTreeInner::create_tree(
                &token.form().chars().collect::<Vec<_>>(),
                &lemma.chars().collect::<Vec<_>>(),
            );

            encoding.push(EditTree { inner: tree });
        }

        Ok(encoding)
    }
}

#[cfg(test)]
mod tests {
    use conllx::graph::{Node, Sentence};
    use conllx::token::{Token, TokenBuilder};
    use edit_tree::EditTree as EditTreeInner;

    use super::{BackoffStrategy, EditTree, EditTreeEncoder};
    use crate::{EncodingProb, SentenceDecoder, SentenceEncoder};

    fn encode_and_wrap(
        encoder: &EditTreeEncoder,
        sent: &Sentence,
    ) -> Vec<Vec<EncodingProb<EditTree>>> {
        encoder
            .encode(&sent)
            .unwrap()
            .into_iter()
            .map(|encoding| vec![EncodingProb::new(encoding, 1.0)])
            .collect::<Vec<_>>()
    }

    fn sentence_from_forms(tokens: &[&str]) -> Sentence {
        tokens.iter().map(|t| Token::new(*t)).collect()
    }

    fn sentence_from_pairs(token_lemmas: &[(&str, &str)]) -> Sentence {
        token_lemmas
            .iter()
            .map(|(t, l)| TokenBuilder::new(*t).lemma(*l).into())
            .collect()
    }

    #[test]
    fn display_edit_tree() {
        let tree = EditTree {
            inner: EditTreeInner::create_tree(
                &['l', 'o', 'o', 'p', 't'],
                &['l', 'o', 'p', 'e', 'n'],
            ),
        };

        assert_eq!(
            tree.to_string(),
            "(match 0 3 () (match 1 1 (replace \"o\" \"\") (replace \"t\" \"en\")))"
        );
    }

    #[test]
    fn encoder_decoder_roundtrip() {
        let sent_encode =
            sentence_from_pairs(&[("hij", "hij"), ("heeft", "hebben"), ("gefietst", "fietsen")]);

        let encoder = EditTreeEncoder::new(BackoffStrategy::Nothing);
        let labels = encode_and_wrap(&encoder, &sent_encode);

        let mut sent_decode = sentence_from_forms(&["hij", "heeft", "gefietst"]);
        encoder.decode(&labels, &mut sent_decode).unwrap();

        assert_eq!(sent_encode, sent_decode);
    }

    #[test]
    fn decoder_backoff_nothing() {
        let sent_encode = sentence_from_pairs(&[
            ("kinderen", "kind"),
            ("hadden", "hebben"),
            ("gefietst", "fietsen"),
        ]);
        let encoder = EditTreeEncoder::new(BackoffStrategy::Nothing);
        let labels = encode_and_wrap(&encoder, &sent_encode);

        let mut sent_decode = sentence_from_forms(&["het", "is", "anders"]);
        encoder.decode(&labels, &mut sent_decode).unwrap();

        assert!(sent_decode
            .iter()
            .filter_map(Node::token)
            .map(Token::lemma)
            .all(|lemma| lemma.is_none()));
    }

    #[test]
    fn decoder_backoff_form() {
        let sent_encode = sentence_from_pairs(&[
            ("kinderen", "kind"),
            ("hadden", "hebben"),
            ("gefietst", "fietsen"),
        ]);
        let encoder = EditTreeEncoder::new(BackoffStrategy::Form);
        let labels = encode_and_wrap(&encoder, &sent_encode);

        let mut sent_decode = sentence_from_forms(&["het", "is", "anders"]);
        encoder.decode(&labels, &mut sent_decode).unwrap();

        for token in sent_decode.iter().filter_map(Node::token) {
            assert_eq!(token.lemma(), Some(token.form()));
        }
    }
}
