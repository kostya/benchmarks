crystal test.cr --release -o json_cr
crystal test_pull.cr --release -o json_pull_cr
crystal test_schema.cr --release -o json_schema_cr
rustc --opt-level 3 test.rs -o json_rs
