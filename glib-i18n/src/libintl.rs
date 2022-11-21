// Take a look at the license at the top of the repository in the LICENSE file.

use std::os::raw::{c_char, c_int, c_ulong};

extern "C" {
    pub fn gettext(msgid: *const c_char) -> *mut c_char;
    pub fn dgettext(domainname: *const c_char, msgid: *const c_char) -> *mut c_char;
    pub fn dcgettext(
        domainname: *const c_char,
        msgid: *const c_char,
        category: c_int,
    ) -> *mut c_char;
    pub fn ngettext(msgid1: *const c_char, msgid2: *const c_char, n: c_ulong) -> *mut c_char;
    pub fn dngettext(
        domainname: *const c_char,
        msgid1: *const c_char,
        msgid2: *const c_char,
        n: c_ulong,
    ) -> *mut c_char;
    pub fn dcngettext(
        domainname: *const c_char,
        msgid1: *const c_char,
        msgid2: *const c_char,
        n: c_ulong,
        category: c_int,
    ) -> *mut c_char;
    pub fn textdomain(domainname: *const c_char) -> *mut c_char;
    pub fn bindtextdomain(domainname: *const c_char, dirname: *const c_char) -> *mut c_char;
    pub fn bind_textdomain_codeset(
        domainname: *const c_char,
        codeset: *const c_char,
    ) -> *mut c_char;
}
