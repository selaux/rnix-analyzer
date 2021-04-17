# rnix-analyzer

Allows to analyze nix code and query the result. Intended for use in a language server.

This is currently in very early development. It still finds some issues with current `nixpkgs`, which should not be there.

## Features

### Scope Analysis

rnix-analyzer analyzes scopes and attribute sets that you define within code. This allows to determine duplicate
definitions of attributes and variables. This includes nested definitions.

Examples:

Duplicate definition in let in:

```rust,skt-rnix-analyzer
let ast = rnix::parse(r#"
    let
        a = 1;
        a = 1; # This will result in an error because a has already been defined
    in
    a + a
"#);
let analysis = AnalysisResult::from(&ast);
let errors: Vec<&AnalysisError> = analysis.errors().collect();

assert_eq!(errors, vec![
    &AnalysisError::Scope(
        ScopeAnalysisError::AlreadyDefined(
            ScopeKind::LetIn,
            "a".to_owned(),
            TextRange::new(TextSize::from(32), TextSize::from(33)),
            TextRange::new(TextSize::from(17), TextSize::from(18))
        )
    ),
]);
```

Duplicate definition in attrset:

```rust,skt-rnix-analyzer
let ast = rnix::parse(r#"
    {
        a = 1;
        a = 1; # This will result in an error because a has already been defined
    }
"#);
let analysis = AnalysisResult::from(&ast);
let errors: Vec<&AnalysisError> = analysis.errors().collect();

assert_eq!(errors, vec![
    &AnalysisError::Scope(
        ScopeAnalysisError::AlreadyDefined(
            ScopeKind::AttrSet,
            "a".to_owned(),
            TextRange::new(TextSize::from(30), TextSize::from(31)),
            TextRange::new(TextSize::from(15), TextSize::from(16))
        )
    ),
]);
```

Another duplicate defintion in attrset:

```rust,skt-rnix-analyzer
let ast = rnix::parse(r#"
    {
        a = { b = 1; };
        a.b = 1; # This will result in an error because a.b has already been defined
        b = { b = 1; };
        b.a = 1; # This is ok
    }
"#);
let analysis = AnalysisResult::from(&ast);
let errors: Vec<&AnalysisError> = analysis.errors().collect();

assert_eq!(errors, vec![
    &AnalysisError::Scope(
        ScopeAnalysisError::AlreadyDefined(
            ScopeKind::AttrSet,
            "a.b".to_owned(),
            TextRange::new(TextSize::from(39), TextSize::from(42)),
            TextRange::new(TextSize::from(21), TextSize::from(22))
        )
    ),
]);
```

### Reference Analysis

rnix-analyzer analyzes references to `let in` definitions, function arguments and recursive attrsets. This allows
to determine undefined references and query for definitions and occurences of variables in code. The major drawback
right now is that it is not possible to determine what happens in `with a;` scopes as we cannot introspect `a`.

Undefined variable:

```nix
a: a + b # This will result in an error as b is not defined
```

```rust,skt-rnix-analyzer
let ast = rnix::parse(r#"
    a: a + b # This will result in an error as b is not defined
"#);
let analysis = AnalysisResult::from(&ast);
let errors: Vec<&AnalysisError> = analysis.errors().collect();

assert_eq!(errors, vec![
    &AnalysisError::Reference(
        ReferenceError::NotFoundInScope(
            "b".to_owned(),
            TextRange::new(TextSize::from(12), TextSize::from(13))
        )
    ),
]);
```

Querying references to definitions:

```rust,skt-rnix-analyzer
let ast = rnix::parse("let foo = 1; in foo + foo");
let analysis = AnalysisResult::from(&ast);
let foo = analysis.variables_at(TextRange::new(
    TextSize::from(16),
    TextSize::from(17)
)).next().unwrap();
let foo_def = analysis.definition_of(&foo).unwrap();
let foo_occurences: Vec<_> = analysis.variables_for(&foo_def).map(|v| (v.name.clone(), v.text_range)).collect();

assert_eq!(foo_occurences, vec![
    ("foo".to_string(), TextRange::new(
        TextSize::from(4),
        TextSize::from(7)
    )),
    ("foo".to_string(), TextRange::new(
        TextSize::from(16),
        TextSize::from(19)
    )),
    ("foo".to_string(), TextRange::new(
        TextSize::from(22),
        TextSize::from(25)
    )),
]);
```

## Example

An example for analysis can be found in the `examples/` directory. For examples for querying please look
into the rust docs (as there is currently no version released, you need to build it yourself).

## Performance

You can run the included benchmarks. Currently takes around 80ms for `all-packages.nix` on my machine and around 6 seconds for the whole `nixpkgs` repo.
