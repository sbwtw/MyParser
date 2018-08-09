
use std::env;
use std::path::Path;
use std::fs::OpenOptions;
use std::io;
use std::io::*;

fn gen_lexer(out_file: &Path) -> io::Result<()> {

    let mut out = OpenOptions::new()
                    .write(true)
                    .create(true)
                    .truncate(true)
                    .open(&out_file)?;

    out.write_all(b"pub struct GenLexer;")?;

    Ok(())
}

fn main() {
    let out_dir = env::var("OUT_DIR").unwrap();
    let out_file = Path::new(&out_dir).join("gen_lexer.rs");

    gen_lexer(&out_file).unwrap();
}