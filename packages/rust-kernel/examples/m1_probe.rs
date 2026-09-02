use std::fs;
use std::io::{self, Write};
use std::path::PathBuf;

use ichiran_kernel::{Kernel, Route};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let directory = std::env::args_os()
        .nth(1)
        .map(PathBuf::from)
        .ok_or("usage: m1_probe <release-directory>")?;
    let hot = fs::read(directory.join("hot.bin"))?;
    let mut kernel = Kernel::open(hot)?;
    eprintln!(
        "sections={} generated_blocks={} generated_decoded_bytes={}",
        kernel.manifest().sections.len(),
        kernel.generated_block_count(),
        kernel.generated_decoded_bytes()
    );
    if std::env::args().nth(2).as_deref() == Some("--fixtures") {
        let fixtures = [
            ("direct", "猫".encode_utf16().collect::<Vec<_>>()),
            ("morphology", "食べた".encode_utf16().collect()),
            ("generated", "忘れた".encode_utf16().collect()),
            ("astral", "😀".encode_utf16().collect()),
            ("high-surrogate", vec![0xd83d]),
            ("low-surrogate", vec![0xde00]),
        ];
        let mut stdout = io::stdout().lock();
        stdout.write_all(b"[")?;
        for (index, (name, input)) in fixtures.iter().enumerate() {
            if index > 0 {
                stdout.write_all(b",")?;
            }
            write!(
                stdout,
                "{{\"name\":{},\"result\":",
                serde_json::to_string(name)?
            )?;
            stdout.write_all(&kernel.analyze_json(input, 1)?)?;
            stdout.write_all(b"}")?;
        }
        stdout.write_all(b"]\n")?;
        return Ok(());
    }
    let requested = std::env::args().skip(2).collect::<Vec<_>>();
    let inputs = if requested.is_empty() {
        vec!["猫".to_owned(), "食べた".to_owned(), "忘れた".to_owned()]
    } else {
        requested
    };
    for text in inputs {
        println!("{}", serde_json::to_string(&kernel.analyze_str(&text, 1)?)?);
    }
    eprintln!(
        "generated={}",
        serde_json::to_string(
            &kernel.generated_lookup(&"忘れた".encode_utf16().collect::<Vec<_>>(), Route::Kanji,)?
        )?
    );
    Ok(())
}
