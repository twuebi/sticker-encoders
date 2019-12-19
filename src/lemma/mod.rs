use conllx::graph::{Node, Sentence};
use edit_tree::{Apply, EditTree};
use failure::{Error, Fail};
use serde::{Deserialize, Serialize};

use super::{EncodingProb, SentenceDecoder, SentenceEncoder};

/// Lemma encoding error.
#[derive(Clone, Debug, Eq, Fail, PartialEq)]
pub enum EncodeError {
    /// The token does not have a lemma.
    #[fail(display = "token without a lemma: '{}'", form)]
    MissingLemma { form: String },
}

/// Back-off strategy.
///
/// This is the strategy that will be used when an edit tree
/// could not be applied.
#[serde(rename_all = "lowercase")]
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum BackoffStrategy {
    Nothing,
    Form,
}

/// Edit tree-based lemma encoder.
///
/// This encoder encodes a lemma as an edit tree that is applied to an
/// unlemmatized form.
#[derive(Clone, Copy, Debug, Deserialize, Serialize)]
pub struct EditTreeEncoder {
    backoff_strategy: BackoffStrategy,
}

impl EditTreeEncoder {
    pub fn new(backoff_strategy: BackoffStrategy) -> Self {
        EditTreeEncoder { backoff_strategy }
    }
}

impl SentenceDecoder for EditTreeEncoder {
    type Encoding = EditTree<char>;

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

                if let Some(lemma) = label.encoding().apply(&form) {
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
    type Encoding = EditTree<char>;

    fn encode(&self, sentence: &Sentence) -> Result<Vec<Self::Encoding>, Error> {
        let mut encoding = Vec::with_capacity(sentence.len() - 1);

        for token in sentence.iter().filter_map(Node::token) {
            let lemma = token.lemma().ok_or_else(|| EncodeError::MissingLemma {
                form: token.form().to_owned(),
            })?;

            let edit_tree = EditTree::create_tree(
                &token.form().chars().collect::<Vec<_>>(),
                &lemma.chars().collect::<Vec<_>>(),
            );

            encoding.push(edit_tree);
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
    ) -> Vec<Vec<EncodingProb<EditTree<char>>>> {
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
        let tree =
            EditTreeInner::create_tree(&['l', 'o', 'o', 'p', 't'], &['l', 'o', 'p', 'e', 'n']);

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
