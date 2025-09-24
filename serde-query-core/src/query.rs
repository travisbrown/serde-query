use proc_macro2::TokenStream;

#[derive(Debug, PartialEq, Eq)]
pub enum QueryFragment {
    Accept,
    /// '.' <name> [.<rest>]
    Field {
        name: String,
        quoted: bool,
        rest: Box<QueryFragment>,
    },
    /// '.' '[' <n> ']' [.<rest>]
    IndexArray {
        index: usize,
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

    pub(crate) fn field(name: String, quoted: bool, rest: Self) -> Self {
        Self::Field {
            name,
            quoted,
            rest: rest.into(),
        }
    }

    pub(crate) fn index_array(index: usize, rest: Self) -> Self {
        Self::IndexArray {
            index,
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
