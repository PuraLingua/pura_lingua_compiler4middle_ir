use compiler_base::AnyResult;

use super::*;

#[test]
fn lex() -> AnyResult<()> {
    let f = std::fs::read_to_string("../__test_data/test.pura")?;
    let mut ts = BorrowedTokenStream::new(&f, tokenize(&f));
    ts.rerender_tokens();

    let output = format!("{ts}");
    std::fs::write("../__test_data/test.tokens", output)?;

    Ok(())
}
