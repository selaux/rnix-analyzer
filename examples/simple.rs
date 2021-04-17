use rnix::{parse, types::TypedNode};

pub fn main() {
    let ast1 = parse("{} ? a");

    println!("Ast1\n======\n{}", ast1.root().dump());
}
