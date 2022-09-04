use std::{fmt::Write, fs, path::PathBuf};

use color_eyre::eyre::{self, bail};

use clap::Parser;

#[derive(Parser, Clone, Debug)]
pub struct Args {
    /// The source file. This will be overwritten with the new data between
    /// delimiters.
    #[clap(required = true)]
    #[clap(parse(from_str))]
    file: PathBuf,

    /// The elements file. This is a list of functions to turn into element segments.
    #[clap(required = true)]
    #[clap(parse(from_str))]
    elems: PathBuf,

    #[clap(parse(from_str))]
    files: Vec<PathBuf>,
}

fn main() -> eyre::Result<()> {
    color_eyre::install()?;

    let args = Args::parse();

    let mut data = Vec::new();
    for path in args.files {
        if !path.exists() {
            bail!("The path {} does not exist.", path.display());
        }
        if !path.is_file() {
            bail!("The path {} must be a file.", path.display());
        }
        let file_ext = path.extension().unwrap().to_str().unwrap().to_string();
        let buf = fs::read(path)?;
        data.push((
            file_ext,
            buf.len(),
            buf.into_iter()
                .map(|x| match x {
                    b'\t' => String::from("\\t"),
                    b'\r' => String::from("\\r"),
                    b'\n' => String::from("\\n"),
                    b'\\' => String::from("\\\\"),
                    b'\'' => String::from("\\\'"),
                    b'"' => String::from("\\\""),
                    b'\x20'..=b'\x7e' => format!("{}", x as char),
                    _ => format!("\\{:<02x}", x),
                })
                .collect::<String>(),
        ));
    }

    let mut offset: u32 = 0;
    let mut new_data_string = String::new();
    writeln!(new_data_string, ";;DATA BEGIN;;")?;
    for (name, size, data) in data {
        writeln!(new_data_string, "    ;; name: {}", &name)?;
        writeln!(new_data_string, "    ;; size: 0x{:<02x}", size)?;
        writeln!(
            new_data_string,
            "    (global $data_{}_offset i32 (i32.const 0x{:<02x}))",
            &name, offset
        )?;
        writeln!(
            new_data_string,
            "    (global $data_{}_size i32 (i32.const 0x{:<02x}))",
            &name, size
        )?;
        writeln!(
            new_data_string,
            "    (data (i32.const 0x{:<02x}) \"{}\")",
            offset, data
        )?;
        writeln!(new_data_string)?;
        offset += size as u32;
        // align to multiples of 4
        if offset % 4 != 0 {
            offset = (offset + 3) & (!4 + 1);
        }
    }
    writeln!(
        new_data_string,
        "    (global $data_end i32 (i32.const 0x{:<02x}))",
        offset
    )?;

    writeln!(new_data_string)?;
    let elems = fs::read_to_string(args.elems)?;
    let mut idx = 0;
    let mut funcs = vec![];
    for line in elems.lines() {
        if line.is_empty() || line.starts_with(";;") {
            continue;
        }
        writeln!(
            new_data_string,
            "    (global $elem_{} i32 (i32.const {}))",
            line, idx
        )?;
        idx += 1;
        funcs.push(line);
    }
    writeln!(
        new_data_string,
        "    (elem (i32.const 0) {})",
        funcs
            .into_iter()
            .map(|x| format!("${}", x))
            .collect::<Vec<_>>()
            .join(" ")
    )?;
    writeln!(new_data_string, ";;DATA END;;")?;

    let mut file_buf = fs::read_to_string(&args.file)?;
    let data_begin_offset = file_buf
        .find(";;DATA BEGIN;;")
        .expect("begin delimiter in source file");
    let data_end_offset = file_buf
        .find(";;DATA END;;\n")
        .expect("begin delimiter in source file")
        + ";;DATA END;;\n".len();

    file_buf.replace_range(data_begin_offset..data_end_offset, &new_data_string);

    fs::write(args.file, file_buf)?;

    Ok(())
}
