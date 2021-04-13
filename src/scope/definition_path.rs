use rnix::{
    types::{Dynamic, Ident, Key, ParsedType, Str, TokenWrapper, TypedNode},
    value::StrPart,
    SyntaxNode, TextRange,
};
use std::convert::TryFrom;

#[derive(Clone)]
pub(crate) enum DefinitionPathSegment {
    Ident(Ident),
    Str(Str),
    InterpolatedString(Str),
    Dynamic(Dynamic),
}

impl DefinitionPathSegment {
    pub fn is_static(&self) -> bool {
        match self {
            Self::InterpolatedString(_) => false,
            Self::Dynamic(_) => false,
            _ => true,
        }
    }

    pub fn name(&self) -> Option<String> {
        match self {
            Self::Ident(i) => Some(i.as_str().to_owned()),
            Self::Str(s) => match &s.parts()[0] {
                StrPart::Literal(s) => Some(s.clone()),
                _ => None,
            },
            Self::InterpolatedString(_) | Self::Dynamic(_) => None,
        }
    }

    pub fn node(&self) -> &SyntaxNode {
        match self {
            Self::Ident(i) => i.node(),
            Self::Str(s) => s.node(),
            Self::InterpolatedString(s) => s.node(),
            Self::Dynamic(d) => d.node(),
        }
    }

    pub fn cast(node: SyntaxNode) -> Option<Self> {
        ParsedType::try_from(node).ok().and_then(|node| match node {
            ParsedType::Ident(ident) => Some(Self::Ident(ident)),
            ParsedType::Str(s) => {
                let parts = s.parts();
                let str_part = parts.get(0);
                if parts.len() == 1 {
                    if let Some(StrPart::Literal(_)) = str_part {
                        return Some(Self::Str(s));
                    }
                }
                Some(Self::InterpolatedString(s))
            }
            ParsedType::Dynamic(dynamic) => Some(Self::Dynamic(dynamic)),
            _ => None,
        })
    }
}

impl PartialEq for DefinitionPathSegment {
    fn eq(&self, other: &Self) -> bool {
        if let (Self::InterpolatedString(_), Self::InterpolatedString(_)) = (self, other) {
            false
        } else {
            self.name() == other.name()
        }
    }
}

#[derive(PartialEq, Clone)]
pub(crate) struct DefinitionPath(Vec<DefinitionPathSegment>);

impl DefinitionPath {
    pub fn first(&self) -> Option<&DefinitionPathSegment> {
        self.0.first()
    }

    pub fn last(&self) -> Option<&DefinitionPathSegment> {
        self.0.last()
    }

    pub fn iter(&self) -> impl Iterator<Item = &DefinitionPathSegment> {
        self.0.iter()
    }

    pub fn parent(&self) -> Option<Self> {
        let parent: Vec<_> = self
            .0
            .iter()
            .cloned()
            .take(self.0.len().saturating_sub(1))
            .collect();
        if parent.is_empty() {
            None
        } else {
            Some(Self(parent))
        }
    }

    pub fn join(&self, other: &Self) -> Self {
        let mut joined = self.0.clone();
        joined.append(&mut other.0.clone());
        Self(joined)
    }

    pub fn empty() -> Self {
        DefinitionPath(vec![])
    }

    pub fn is_static(&self) -> bool {
        self.0.iter().all(|segment| segment.is_static())
    }

    pub fn as_string(&self) -> String {
        self.0
            .iter()
            .map(|v| v.name().unwrap_or_else(|| "<dynamic>".to_owned()))
            .collect::<Vec<String>>()
            .join(".")
    }

    pub fn from_ident(key: Ident) -> Self {
        Self(vec![DefinitionPathSegment::Ident(key)])
    }

    pub fn from_key(key: Key) -> Option<Self> {
        let values: Vec<_> = key.path().filter_map(DefinitionPathSegment::cast).collect();
        if values.is_empty() {
            None
        } else {
            Some(Self(values))
        }
    }

    pub fn text_range(&self) -> Option<TextRange> {
        let from = self.first()?;
        let from = from.node().text_range().start();
        let to = self.last()?;
        let to = to.node().text_range().end();
        Some(TextRange::new(from, to))
    }
}
