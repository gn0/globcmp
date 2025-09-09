use clap::Parser;

use globcmp_lib::Pattern;

#[derive(Debug, Parser)]
#[command(version, about)]
struct Args {
    pattern_a: Pattern,
    pattern_b: Pattern,
}

fn main() {
    let Args {
        pattern_a,
        pattern_b,
    } = Args::parse();

    match (
        pattern_a.is_more_specific_than(&pattern_b),
        pattern_b.is_more_specific_than(&pattern_a),
    ) {
        (true, true) => println!("same"),
        (true, false) => println!("pattern_a"),
        (false, true) => println!("pattern_b"),
        (false, false) => println!("unknown"),
    }
}
