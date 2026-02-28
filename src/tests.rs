use std::io::Write as _;

use crate::CompileContext;

#[test]
fn parse() -> pura_lingua::global::Result<()> {
    let f = parser::parse(
        "Test".to_owned(),
        false,
        r#"
class[0 public] "Test::Test": [!]"System::Object"
{
	field[0 public] A: [!]"System::UInt64";

	method[1 public override] "ToString"() -> [!]"System::String"
	{
		locals (
			a: [Test]"Test::Test"
			b: [!]"System::UInt64"
			c: [!]"System::String"
		)

		Load this -> a;
		LoadField a 0 -> b;
		StaticCall [!]"System::UInt64" 1(b) -> c;
		ReturnVal c;
	}
}
"#,
    )?;
    dbg!(f);

    Ok(())
}

#[test]
fn runtime_test_normal_f() -> pura_lingua::global::Result<()> {
    let mut ctx = CompileContext::new();
    ctx.add_core()?;

    ctx.add_file(
        "./TestData/runtime_test_normal_f.pl_middle_ir",
        "TestNormalF".to_owned(),
        false,
    )?;

    let compiled = ctx.compile()?;

    let mut answer = String::new();
    std::io::stdout().write_all("Should emit to file[Y/N]:".as_bytes())?;
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut answer)?;
    answer.make_ascii_lowercase();
    if answer.contains("y") {
        for (name, content) in compiled {
            let mut file = std::fs::File::create(format!("./TestData/{name}.plb"))?;
            content.write_to(&mut file)?;
        }
    }

    Ok(())
}

#[test]
fn runtime_gtest_fn() -> pura_lingua::global::Result<()> {
    let mut ctx = CompileContext::new();
    ctx.add_core()?;

    ctx.add_file(
        "./TestData/runtime_gtest_fn.pl_middle_ir",
        "Test".to_owned(),
        false,
    )?;

    let compiled = ctx.compile()?;

    let mut answer = String::new();
    std::io::stdout().write_all("Should emit to file[Y/N]:".as_bytes())?;
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut answer)?;
    answer.make_ascii_lowercase();
    if answer.contains("y") {
        for (name, content) in compiled {
            let mut file = std::fs::File::create(format!("./TestData/{name}.plb"))?;
            content.write_to(&mut file)?;
        }
    }

    Ok(())
}

#[test]
fn runtime_gtest_msgbox() -> pura_lingua::global::Result<()> {
    let mut ctx = CompileContext::new();
    ctx.add_core()?;

    ctx.add_file(
        "./TestData/runtime_gtest_msgbox.pl_middle_ir",
        "MsgboxTest".to_owned(),
        false,
    )?;

    let compiled = ctx.compile()?;

    let mut answer = String::new();
    std::io::stdout().write_all("Should emit to file[Y/N]:".as_bytes())?;
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut answer)?;
    answer.make_ascii_lowercase();
    if answer.contains("y") {
        for (name, content) in compiled {
            let mut file = std::fs::File::create(format!("./TestData/{name}.plb"))?;
            content.write_to(&mut file)?;
        }
    }

    Ok(())
}

#[test]
fn simple_console() -> pura_lingua::global::Result<()> {
    let mut ctx = CompileContext::new();
    ctx.add_core()?;

    ctx.add_language_specific_dependency("SimpleConsole")?;

    let compiled = ctx.compile()?;

    let mut answer = String::new();
    std::io::stdout().write_all("Should emit to file[Y/N]:".as_bytes())?;
    std::io::stdout().flush()?;
    std::io::stdin().read_line(&mut answer)?;
    answer.make_ascii_lowercase();
    if answer.contains("y") {
        for (name, content) in compiled {
            let mut file =
                std::fs::File::create(format!("./TestData/{}.plb", name.replace("::", "_")))?;
            content.write_to(&mut file)?;
        }
    }

    Ok(())
}
