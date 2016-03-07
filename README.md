This is a work in progress. In time it should become a real rangeset 
implementation.

# Goals:

* As little data as possible (e.g. singleton type)
* No unnecessary cloning of values – should work without T: Clone
* Good performance on all relevant operations

# License

Licensed under either of

 * Apache License, Version 2.0, ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
 * MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted 
for inclusion in the work by you, as defined in the Apache-2.0 license, shall 
be dual licensed as above, without any additional terms or conditions.

# TODOs:

* [_] Write the Cut and Range implementation (misses some methods)
* [X] `impl<T> Ord for Range<T>`
* [_] Write the actual RangeSet
* [_] Tests
* [_] `#[deny(missing_docs)`
* [_] rustfmt
* [_] write a proper README
