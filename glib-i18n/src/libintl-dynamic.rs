// Take a look at the license at the top of the repository in the LICENSE file.

use once_cell::sync::Lazy;
use std::ffi::{c_char, c_int, c_ulong};
use std::{
    ptr::null_mut,
    sync::atomic::{AtomicPtr, Ordering},
};

static LIBRARY: Lazy<Option<libloading::Library>> = Lazy::new(|| {
    #[cfg(all(target_os = "windows", target_arch = "x86_64"))]
    let path = "libintl-8.dll";
    #[cfg(all(target_os = "windows", not(target_arch = "x86_64")))]
    let path = "intl.dll";
    #[cfg(target_os = "macos")]
    let path = "libintl.dylib";
    #[cfg(all(not(target_os = "windows"), not(target_os = "macos")))]
    let path = "libintl.so";
    unsafe { libloading::Library::new(path).ok() }
});

macro_rules! dynamic_fn {
    ($name:ident ( $($arg:ident : $arg_ty:ty),* $(,)? ) $(-> $ret:ty)?, $fallback_name:ident { $($fallback:tt)* }) => {
        pub unsafe extern "C" fn $name ($($arg: $arg_ty),*) $(-> $ret)? {
            #[allow(unused_variables)]
            unsafe extern "C" fn $fallback_name ($($arg: $arg_ty),*) $(-> $ret)? {
                $($fallback)*
            }
            static CACHED: AtomicPtr<()> = AtomicPtr::new(null_mut());
            let ptr = CACHED.load(Ordering::Relaxed);
            let ptr: unsafe extern "C" fn ($($arg_ty),*) $(-> $ret)? = if !ptr.is_null() {
                std::mem::transmute(ptr)
            } else {
                let ptr = LIBRARY
                    .as_ref()
                    .and_then(|lib| lib.get(concat!(stringify!($name), '\0').as_bytes()).map(|s| *s).ok())
                    .unwrap_or($fallback_name);
                let _ = CACHED.compare_exchange(
                    null_mut(),
                    ptr as *mut (),
                    Ordering::Release,
                    Ordering::Relaxed,
                );
                ptr
            };
            ptr($($arg),*)
        }
    };
}

dynamic_fn!(
    gettext(msgid: *const c_char) -> *mut c_char,
    fallback_gettext {
        msgid as *mut _
    }
);

dynamic_fn!(
    dgettext(domainname: *const c_char, msgid: *const c_char) -> *mut c_char,
    fallback_dgettext {
        msgid as *mut _
    }
);

dynamic_fn!(
    dcgettext(domainname: *const c_char, msgid: *const c_char, category: c_int) -> *mut c_char,
    fallback_dcgettext {
        msgid as *mut _
    }
);

dynamic_fn!(
    ngettext(msgid1: *const c_char, msgid2: *const c_char, n: c_ulong) -> *mut c_char,
    fallback_ngettext {
        (if n == 1 { msgid1 } else { msgid2 }) as *mut _
    }
);

dynamic_fn!(
    dngettext(
        domainname: *const c_char,
        msgid1: *const c_char,
        msgid2: *const c_char,
        n: c_ulong,
    ) -> *mut c_char,
    fallback_dngettext {
        (if n == 1 { msgid1 } else { msgid2 }) as *mut _
    }
);

dynamic_fn!(
    dcngettext(
        domainname: *const c_char,
        msgid1: *const c_char,
        msgid2: *const c_char,
        n: c_ulong,
        category: c_int,
    ) -> *mut c_char,
    fallback_dcngettext {
        (if n == 1 { msgid1 } else { msgid2 }) as *mut _
    }
);

dynamic_fn!(
    textdomain(domainname: *const c_char) -> *mut c_char,
    fallback_textdomain {
        static MESSAGES: &[u8] = b"messages\0";
        static DOMAIN: AtomicPtr<c_char> = AtomicPtr::new(MESSAGES.as_ptr() as *mut _);
        if domainname.is_null() {
            DOMAIN.load(Ordering::Acquire)
        } else {
            let domainname = if domainname.read() == 0
                || libc::strcmp(domainname, MESSAGES.as_ptr() as *const _) == 0
            {
                MESSAGES.as_ptr() as *mut _
            } else {
                libc::strdup(domainname)
            };
            let old = DOMAIN.swap(domainname, Ordering::SeqCst);
            if old != MESSAGES.as_ptr() as *mut _ {
                libc::free(old as *mut _);
            }
            domainname
        }
    }
);

dynamic_fn!(
    bindtextdomain(domainname: *const c_char, dirname: *const c_char) -> *mut c_char,
    fallback_bindtextdomain {
        b"/dummy\0".as_ptr() as *mut _
    }
);

dynamic_fn!(
    bind_textdomain_codeset(domainname: *const c_char, codeset: *const c_char) -> *mut c_char,
    fallback_bind_textdomain_codeset {
        codeset as *mut _
    }
);
