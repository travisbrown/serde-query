#[derive(serde_query::Deserialize)]
struct Message {
    #[query(".commit.message")]
    message: String,
}

fn main() {
    let messages = ureq::get("https://api.github.com/repos/pandaman64/serde-query/commits")
        .call()
        .unwrap()
        .body_mut()
        .read_json::<Vec<Message>>()
        .unwrap();

    for message in messages.into_iter() {
        println!("{}", message.message);
    }
}
