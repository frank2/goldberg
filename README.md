# Goldberg
**Goldberg** is a Rust procedural macro library for obfuscating Rust code. Its obfuscation techniques are designed to survive both compilation as well as optimization. While not intended to be a source code obfuscator, it *can* be used as such. It is named after Rube Goldberg machines.

Currently, the following types of obfuscation are supported:

* code-flow obfuscation
* string literal encryption
* integer literal obfuscation

The documentation can be found [here](https://docs.rs/goldberg). For usage examples, read [the tests file](https://github.com/frank2/goldberg/blob/main/tests/tests.rs). The changelog history can be found [here](https://github.com/frank2/goldberg/blob/main/CHANGELOG.md).
