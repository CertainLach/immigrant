use std::{fs, path::PathBuf};

fn main() {
	use std::fmt::Write;
	let files = fs::read_dir("examples").expect("list examples");
	let mut out_dir = PathBuf::from(std::env::var("OUT_DIR").unwrap());
	out_dir.push("example_tests.rs");
	let mut tests = String::new();
	for file in files {
		let file = file.expect("entry");
		let fname = file.file_name();
		let fname = fname.to_str().expect("file has utf-8 name");
		let test_name = fname.to_owned().replace(['.', '-'], "_");
		writeln!(
			tests,
			r#"
			#[traced_test]
			#[test]
			fn test_example_{test_name}() {{
				test_example("examples/{fname}");
			}}
		"#
		)
		.unwrap()
	}
	fs::write(out_dir, tests).expect("write");
}
