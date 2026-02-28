//! Command: PLICompiler-MiddleIR
#![feature(decl_macro)]

use std::{io::Write, str::FromStr};

use Compiler_MiddleIR::CompileContext;
use line_ending::LineEnding;

#[derive(derive_more::FromStr)]
#[from_str(rename_all = "SCREAMING-KEBAB-CASE")]
pub enum Command {
    Quit,
    Exit,
    SetAssemblyName,
    AddSingleFile,
    AddSingleHeader,
    AddGlobFiles,
    AddGlobHeaders,
    AddCore,
    AddLanguageSpecificDependency,

    /// It will exit after finish
    BuildTo,
}

fn main() -> pura_lingua::global::Result<()> {
    let mut compiler_ctx = CompileContext::new();
    let mut stdout = std::io::stdout();
    let stdin = std::io::stdin();

    let pause_after_exit = std::env::args().any(|x| x == "--pause-after-exit");

    let mut assembly_name = None;

    loop {
        write!(stdout, ">> ")?;
        stdout.flush()?;

        let mut command = String::new();
        stdin.read_line(&mut command)?;
        // cSpell:disable-next-line
        if let Some(x) = command.rfind(LineEnding::from_current_platform().as_str()) {
            command.truncate(x);
        }

        if command.starts_with(':') && command.len() >= 1 {
            let last_space = command.find(' ').unwrap_or(command.len());
            let cmd = match Command::from_str(&command[1..last_space]) {
                Ok(x) => x,
                Err(_) => {
                    writeln!(stdout, "INVALID COMMAND: `{}`", &command[1..last_space])?;
                    continue;
                }
            };

            match cmd {
                Command::Quit | Command::Exit => break,
                Command::SetAssemblyName => {
                    let Some(input_assembly_name) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `SET-ASSEMBLY-NAME` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    assembly_name = Some(input_assembly_name.to_owned());
                }
                Command::AddSingleFile => {
                    let Some(file_path) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `ADD-SINGLE-FILE` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    let Some(assembly_name) = &assembly_name else {
                        writeln!(
                            stdout,
                            "COMMAND `ADD-SINGLE-FILE` REQUIRE ASSEMBLY-NAME TO BE SET"
                        )?;
                        continue;
                    };
                    if let Err(e) =
                        compiler_ctx.add_file(file_path, assembly_name.to_owned(), false)
                    {
                        writeln!(stdout, "COMMAND `ADD-SINGLE-FILE` FAILED WITH:\n{e}")?;
                    }
                }
                Command::AddSingleHeader => {
                    let Some(file_path) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `ADD-SINGLE-HEADER` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    let Some(assembly_name) = &assembly_name else {
                        writeln!(
                            stdout,
                            "COMMAND `ADD-SINGLE-HEADER` REQUIRE ASSEMBLY-NAME TO BE SET"
                        )?;
                        continue;
                    };
                    if let Err(e) = compiler_ctx.add_file(file_path, assembly_name.to_owned(), true)
                    {
                        writeln!(stdout)?;
                        writeln!(stdout, "COMMAND `ADD-SINGLE-HEADER` FAILED WITH:\n{e}")?;
                    }
                }
                Command::AddGlobFiles => {
                    let Some(pattern) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `ADD-GLOB-FILES` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    let Some(assembly_name) = &assembly_name else {
                        writeln!(
                            stdout,
                            "COMMAND `ADD-GLOB-FILES` REQUIRE ASSEMBLY-NAME TO BE SET"
                        )?;
                        continue;
                    };
                    if let Err(e) = compiler_ctx.add_glob(pattern, assembly_name.to_owned(), false)
                    {
                        writeln!(stdout, "COMMAND `ADD-GLOB-FILES` FAILED WITH:\n{e}")?;
                    }
                }
                Command::AddGlobHeaders => {
                    let Some(pattern) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `ADD-GLOB-HEADERS` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    let Some(assembly_name) = &assembly_name else {
                        writeln!(
                            stdout,
                            "COMMAND `ADD-GLOB-HEADERS` REQUIRE ASSEMBLY-NAME TO BE SET"
                        )?;
                        continue;
                    };
                    if let Err(e) = compiler_ctx.add_glob(pattern, assembly_name.to_owned(), true) {
                        writeln!(stdout, "COMMAND `ADD-GLOB-HEADERS` FAILED WITH:\n{e}")?;
                    }
                }
                Command::AddCore => {
                    if let Err(e) = compiler_ctx.add_core() {
                        writeln!(stdout, "COMMAND `ADD-CORE` FAILED WITH:\n{e}")?;
                    }
                }
                Command::AddLanguageSpecificDependency => {
                    let Some(dep) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `ADD-GLOB-HEADERS` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };
                    if let Err(e) = compiler_ctx.add_language_specific_dependency(dep) {
                        writeln!(
                            stdout,
                            "COMMAND `ADD-LANGUAGE-SPECIFIC-DEPENDENCY` FAILED WITH:\n{e}"
                        )?;
                    }
                }

                Command::BuildTo => {
                    macro before_exit() {
                        if pause_after_exit {
                            let _ = stdin.read_line(&mut String::new());
                        }
                    }
                    let Some(target) = command.get((last_space + 1)..) else {
                        writeln!(stdout, "COMMAND `BUILD-TO` REQUIRE ONE ARGUMENT")?;
                        continue;
                    };

                    let compiled = match compiler_ctx.compile() {
                        Ok(x) => x,
                        Err(err) => {
                            writeln!(stdout, "COMMAND `BUILD-TO` FAILED TO COMPILE WITH:\n{err}")?;
                            before_exit!();
                            break;
                        }
                    };

                    if let Err(err) = std::fs::create_dir_all(target) {
                        writeln!(
                            stdout,
                            "COMMAND `BUILD-TO` FAILED TO CREATE DIRECTORY BECAUSE {err}"
                        )?;
                        before_exit!();
                        break;
                    }

                    for (name, assembly) in compiled {
                        let file_path = format!("{target}/{}.plb", name.replace("::", "_"));
                        let mut file = match std::fs::File::create(&file_path) {
                            Ok(x) => x,
                            Err(err) => {
                                writeln!(
                                    stdout,
                                    "COMMAND `BUILD-TO` FAILED TO CREATE FILE {file_path} BECAUSE {err}"
                                )?;
                                before_exit!();
                                break;
                            }
                        };
                        if let Err(err) = assembly.write_to(&mut file) {
                            writeln!(
                                stdout,
                                "COMMAND `BUILD-TO` FAILED TO WRITE FILE {file_path} BECAUSE {err}"
                            )?;
                            before_exit!();
                            break;
                        }
                    }

                    break;
                }
            }
        } else {
            writeln!(stdout, "ECHO: {}", command)?;
        }
    }

    Ok(())
}
