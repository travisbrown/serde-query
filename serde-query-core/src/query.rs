use proc_macro2::TokenStream;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Optionality {
    None,
    PostOptional,
    Optional,
}

impl Optionality {
    pub const fn is_optional(self) -> bool {
        matches!(self, Self::Optional)
    }

    pub const fn is_post_optional(self) -> bool {
        matches!(self, Self::PostOptional)
    }

    pub const fn question_mark(self) -> &'static str {
        if self.is_optional() {
            "?"
        } else {
            ""
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum QueryFragment {
    Accept,
    /// '.' <name> [.<rest>]
    Field {
        name: String,
        quoted: bool,
        optional: Optionality,
        rest: Box<QueryFragment>,
    },
    /// '.' '[' <n> ']' [.<rest>]
    IndexArray {
        index: usize,
        optional: Optionality,
        rest: Box<QueryFragment>,
    },
    /// '.[]' [.<rest>]
    CollectArray {
        rest: Box<QueryFragment>,
    },
}

impl QueryFragment {
    pub(crate) const fn accept() -> Self {
        Self::Accept
    }

    pub(crate) fn field(name: String, quoted: bool, optional: Optionality, rest: Self) -> Self {
        Self::Field {
            name,
            quoted,
            optional,
            rest: rest.into(),
        }
    }

    pub(crate) fn index_array(index: usize, optional: Optionality, rest: Self) -> Self {
        Self::IndexArray {
            index,
            optional,
            rest: rest.into(),
        }
    }

    pub(crate) fn collect_array(rest: Self) -> Self {
        Self::CollectArray { rest: rest.into() }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct QueryId(syn::Ident);

impl QueryId {
    pub(crate) const fn new(identifier: syn::Ident) -> Self {
        Self(identifier)
    }

    pub(crate) const fn ident(&self) -> &syn::Ident {
        &self.0
    }
}

#[derive(Debug)]
pub struct Query {
    pub(crate) id: QueryId,
    pub(crate) fragment: QueryFragment,
    pub(crate) ty: TokenStream,
}

impl Query {
    pub(crate) const fn new(id: QueryId, fragment: QueryFragment, ty: TokenStream) -> Self {
        Self { id, fragment, ty }
    }
}
