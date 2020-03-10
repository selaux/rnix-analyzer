# rnix-analyzer

Allows to analyze nix code and query the result. Intended for use in a language server.

## Features

### Scope Analysis

rnix-analyzer analyzes scopes and attribute sets that you define within code. This allows to determine duplicate
definitions of attributes and variables. This includes nested definitions.

Examples:

```nix
let
    a = 1;
    a = 1; # This will result in an error because a has already been defined
in
a + a
```

```nix
{
    a = 1;
    a = 1; # This will result in an error because a has already been defined
}
```

```nix
{
    a = { b = 1; };
    a.b = 1; # This will result in an error because a.b has already been defined
    b = { b = 1; };
    b.a = 1; # This is ok
}
```

### Reference Analysis

rnix-analyzer analyzes references to `let in` definitions, function arguments and recursive attrsets. This allows
to determine undefined references and query for definitions and occurences of variables in code. The major drawback
right now is that it is not possible to determine what happens in `with a;` scopes as we cannot introspect `a`.

Examples:

```nix
a: a + b # This will result in an error as b is not defined
```

### Querying the Result

Example:

```rust
use rnix_analyzer::*;

let ast = rnix::parse("let foo = 1; in foo + foo");
let analysis = AnalysisResult::from(&ast);
let foo = analysis.variables_at(TextRange::from_to(
    TextUnit::from(16),
    TextUnit::from(17)
)).next().unwrap();
let foo_def = analysis.definition_of(&foo).unwrap();
let foo_occurences: Vec<_> = analysis.variables_for(&foo_def).map(|v| (v.name.clone(), v.text_range)).collect();

assert_eq!(foo_occurences, vec![
    ("foo".to_string(), TextRange::from_to(
        TextUnit::from(4),
        TextUnit::from(7)
    )),
    ("foo".to_string(), TextRange::from_to(
        TextUnit::from(16),
        TextUnit::from(19)
    )),
    ("foo".to_string(), TextRange::from_to(
        TextUnit::from(22),
        TextUnit::from(25)
    )),
]);
```
