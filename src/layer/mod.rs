//! CoNLL-X layer encoder.

use conllu::graph::{Node, Sentence};
use conllu::token::Token;
use failure::Error;
use serde_derive::{Deserialize, Serialize};

use super::{EncodingProb, SentenceDecoder, SentenceEncoder};

mod error;
use self::error::*;

/// Tagging layer.
#[serde(rename_all = "lowercase")]
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub enum Layer {
    UPos,
    XPos,
    Feature(String),
    Misc(String),
}

/// Layer values.
pub trait LayerValue {
    /// Set a layer value.
    fn set_value(&mut self, layer: &Layer, value: impl Into<String>);

    /// Get a layer value.
    fn value(&self, layer: &Layer) -> Option<&str>;
}

impl LayerValue for Token {
    /// Set the layer to the given value.
    fn set_value(&mut self, layer: &Layer, value: impl Into<String>) {
        let value = value.into();

        match layer {
            Layer::UPos => {
                self.set_upos(Some(value));
            }
            Layer::XPos => {
                self.set_xpos(Some(value));
            }
            Layer::Feature(ref feature) => {
                self.features_mut().insert(feature.clone(), value);
            }
            Layer::Misc(ref misc) => {
                self.misc_mut().insert(misc.clone(), Some(value));
            }
        };
    }

    /// Look up the layer value in a token.
    fn value(&self, layer: &Layer) -> Option<&str> {
        match layer {
            Layer::UPos => self.upos(),
            Layer::XPos => self.xpos(),
            Layer::Feature(ref feature) => self.features().get(feature).map(String::as_str),
            Layer::Misc(ref misc) => self.misc().get(misc)?.as_ref().map(String::as_str),
        }
    }
}

/// Encode sentences using a CoNLL-X layer.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
pub struct LayerEncoder {
    layer: Layer,
}

impl LayerEncoder {
    /// Construct a new layer encoder of the given layer.
    pub fn new(layer: Layer) -> Self {
        LayerEncoder { layer }
    }
}

impl SentenceDecoder for LayerEncoder {
    type Encoding = String;

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
                token.set_value(&self.layer, label.encoding().as_str());
            }
        }

        Ok(())
    }
}

impl SentenceEncoder for LayerEncoder {
    type Encoding = String;

    fn encode(&self, sentence: &Sentence) -> Result<Vec<Self::Encoding>, Error> {
        let mut encoding = Vec::with_capacity(sentence.len() - 1);
        for token in sentence.iter().filter_map(Node::token) {
            let label = token
                .value(&self.layer)
                .ok_or_else(|| EncodeError::MissingLabel {
                    form: token.form().to_owned(),
                })?;
            encoding.push(label.to_owned());
        }

        Ok(encoding)
    }
}

#[cfg(test)]
mod tests {
    use std::convert::TryFrom;

    use conllu::token::{Features, Misc, Token, TokenBuilder};

    use crate::layer::{Layer, LayerValue};

    #[test]
    fn layer() {
        let token: Token = TokenBuilder::new("test")
            .upos("CP")
            .xpos("P")
            .features(Features::try_from("a=b|c=d").unwrap())
            .misc(Misc::from("u=v|x=y"))
            .into();

        assert_eq!(token.value(&Layer::UPos), Some("CP"));
        assert_eq!(token.value(&Layer::XPos), Some("P"));
        assert_eq!(token.value(&Layer::Feature("a".to_owned())), Some("b"));
        assert_eq!(token.value(&Layer::Feature("c".to_owned())), Some("d"));
        assert_eq!(token.value(&Layer::Feature("e".to_owned())), None);
        assert_eq!(token.value(&Layer::Misc("u".to_owned())), Some("v"));
        assert_eq!(token.value(&Layer::Misc("x".to_owned())), Some("y"));
        assert_eq!(token.value(&Layer::Misc("z".to_owned())), None);
    }

    #[test]
    fn set_layer() {
        let mut token: Token = TokenBuilder::new("test").into();
        token.set_value(&Layer::UPos, "CP");
        token.set_value(&Layer::XPos, "P");
        token.set_value(&Layer::Feature("a".to_owned()), "b");
        token.set_value(&Layer::Misc("u".to_owned()), "v");

        assert_eq!(token.value(&Layer::UPos), Some("CP"));
        assert_eq!(token.value(&Layer::XPos), Some("P"));
        assert_eq!(token.value(&Layer::Feature("a".to_owned())), Some("b"));
        assert_eq!(token.value(&Layer::Feature("c".to_owned())), None);
        assert_eq!(token.value(&Layer::Misc("u".to_owned())), Some("v"));
        assert_eq!(token.value(&Layer::Misc("x".to_owned())), None);
    }
}
