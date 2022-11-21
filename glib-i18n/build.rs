// Take a look at the license at the top of the repository in the LICENSE file.

use std::{
    env,
    ffi::{OsStr, OsString},
    io::Write,
    path::PathBuf,
};

fn main() {
    let manifest_dir = PathBuf::from(std::env::var_os("CARGO_MANIFEST_DIR").unwrap());
    let mut gettext_test = std::fs::read_to_string(manifest_dir.join("src/libintl.rs")).unwrap();
    gettext_test.push_str(r#"unsafe { gettext("test\0".as_ptr()); }"#);

    let compiler = Compiler::new();

    if compiler.try_compile(&gettext_test, [] as [&str; 0]) {
        println!("cargo:rustc-cfg=system_libintl");
    } else if compiler.try_compile(&gettext_test, ["-l", "intl"]) {
        println!("cargo:rustc-cfg=system_libintl");
        println!("cargo:rustc-link-lib=intl");
    }
}

struct Compiler {
    rustc: OsString,
    tmp_bin: PathBuf,
}

impl Compiler {
    fn new() -> Self {
        Self {
            rustc: env::var_os("RUSTC").unwrap(),
            tmp_bin: PathBuf::from(std::env::var_os("OUT_DIR").unwrap()).join("tmp.bin"),
        }
    }
    fn try_compile(
        &self,
        src: &str,
        extra_args: impl IntoIterator<Item = impl AsRef<OsStr>>,
    ) -> bool {
        let mut handle = std::process::Command::new(&self.rustc)
            .args(extra_args)
            .args::<_, &OsStr>(["-o".as_ref(), self.tmp_bin.as_ref(), "-".as_ref()])
            .stdin(std::process::Stdio::piped())
            .spawn()
            .unwrap();
        let mut stdin = handle.stdin.take().unwrap();
        stdin.write_all(b"pub fn main() {\n").unwrap();
        stdin.write_all(src.as_bytes()).unwrap();
        stdin.write_all(b"}").unwrap();
        handle.wait().unwrap().success()
    }
}
