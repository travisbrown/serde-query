#[cfg(test)]
mod tests {
    use serde_query::{DeserializeQuery, Query};

    #[derive(DeserializeQuery)]
    struct Data {
        #[query(".qux.[0]")]
        foo: bool,
        #[query(".qux.[1]?")]
        bar: Option<bool>,
    }

    #[test]
    fn test_optional() {
        const INPUT: &str = r#"{ "qux": [false] }"#;
        let data: Data = serde_json::from_str::<Query<Data>>(INPUT).unwrap().into();
        assert_eq!(data.foo, false);
        assert_eq!(data.bar, None);
    }
}
