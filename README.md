# globcmp

globcmp assesses whether one glob pattern matches a file path more closely than another glob pattern does.

globcmp can be used either as a standalone executable or as a dependency for other crates.
If you want to use it as a dependency, see the [globcmp-lib](https://github.com/gn0/globcmp/blob/main/globcmp-lib) crate.

## Installation

### Instructions

Install via [crates.io](https://crates.io/crates/globcmp):

```sh
cargo install --locked globcmp
```

Or from the GitHub repository:

```sh
cargo install --locked --git https://github.com/gn0/globcmp.git
```

If `$HOME/.cargo/bin` is not in your `PATH` environment variable, then you also need to run:

```sh
export PATH=$HOME/.cargo/bin:$PATH
```

To make this setting permanent:

```sh
echo 'export PATH=$HOME/.cargo/bin:$PATH' >> $HOME/.bashrc  # If using bash.
echo 'export PATH=$HOME/.cargo/bin:$PATH' >> $HOME/.zshrc   # If using zsh.
```

### Feature flag: stack safety

This crate has one feature flag, `stack-safe` (off by default).
globcmp's pattern matching algorithms rely heavily on recursion, so they can in principle overflow the stack and panic.
If compiled with the `stack-safe` feature flag turned on, globcmp uses the [stacker](https://crates.io/crates/stacker/) crate to allocate memory on the heap if the stack space is about to run out.

There is a performance cost to checking the available stack space each time the algorithm recurs.
For the executable provided by the globcmp crate, you most likely won't need to turn on `stack-safe`.
If you use the [globcmp-lib](https://github.com/gn0/globcmp/tree/main/globcmp-lib) crate as a dependency, you might.

## Usage

### Basics

```sh
globcmp 'foo/bar' 'foo/bar'  # Prints: same
globcmp 'foo/bar' 'foo/b?r'  # Prints: pattern_a
globcmp 'foo/b*r' 'foo/b?r'  # Prints: pattern_b
globcmp 'foo/b*r' 'foo/asd'  # Prints: unknown
```

### Supported syntax

| Syntax  | Meaning | Example |
|---------|---------|---------|
| `?`     | Matches exactly one character within file and directory names. | `foo/b?r` |
| `*`     | Matches zero or more characters within file and directory names. | `foo/*` |
| `**`    | Matches zero or more directory levels within the file path. Cannot be preceded or succeeded by any character, except for `/`. | `foo/**/bar` |
| `[abc]` | Matches exactly one of the characters listed in the square brackets. | `foo/b[abc]r` |
| `[a-c]` | Matches exactly one of the characters in the interval (inclusive) listed in the square brackets. | `foo/b[a-c]r` |

## License

globcmp is distributed under the GNU Lesser General Public License (LGPL), version 3.
See the file [LICENSE](./LICENSE) for more information.

