#[cfg(test)]
mod tests {
    use serde_query::{DeserializeQuery, Query};

    #[derive(DeserializeQuery)]
    struct Data {
        #[query(".data.[1].bar?.a?.b?.c")]
        foo: Option<bool>,
        #[query(".data.[0].[1]?.[0]")]
        bar: Option<String>,
    }

    #[test]
    fn test_optional() {
        const INPUT: &str =
            r#"{ "data": [ [ [ "foo" ] ], { "bar": { "a": { "b": { "c": true } } } } ] }"#;
        let data: Data = serde_json::from_str::<Query<Data>>(INPUT).unwrap().into();
        assert_eq!(data.foo, Some(true));
        assert_eq!(data.bar, None);
    }
}
