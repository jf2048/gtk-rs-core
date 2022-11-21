// Take a look at the license at the top of the repository in the LICENSE file.

use glib::{translate::*, GStr, GString, IntoGStr, IntoOptionalGStr};
use std::{
    ffi::{c_char, c_int},
    io,
};

#[doc(hidden)]
pub use glib;

#[cfg_attr(system_libintl, path = "libintl.rs")]
#[cfg_attr(not(system_libintl), path = "libintl-dynamic.rs")]
pub mod ffi;

pub mod macros;

#[repr(i32)]
#[derive(Debug, Eq, PartialEq, Ord, PartialOrd, Hash, Clone, Copy)]
pub enum LocaleCategory {
    CType = libc::LC_CTYPE as i32,
    Numeric = libc::LC_NUMERIC as i32,
    Time = libc::LC_TIME as i32,
    Collate = libc::LC_COLLATE as i32,
    Monetary = libc::LC_MONETARY as i32,
    Messages = libc::LC_MESSAGES as i32,
    All = libc::LC_ALL as i32,
    Paper = libc::LC_PAPER as i32,
    Name = libc::LC_NAME as i32,
    Address = libc::LC_ADDRESS as i32,
    Telephone = libc::LC_TELEPHONE as i32,
    Measurement = libc::LC_MEASUREMENT as i32,
    Identification = libc::LC_IDENTIFICATION as i32,
}

pub fn gettext(msgid: impl IntoGStr) -> &'static GStr {
    msgid.run_with_gstr(|msgid| unsafe {
        from_glib_none(glib::ffi::g_dgettext(
            std::ptr::null(),
            msgid.to_glib_none().0,
        ))
    })
}

#[doc(alias = "g_dgettext")]
pub fn dgettext(domain: impl IntoGStr, msgid: impl IntoGStr) -> &'static GStr {
    domain.run_with_gstr(|domain| {
        msgid.run_with_gstr(|msgid| unsafe {
            from_glib_none(glib::ffi::g_dgettext(
                domain.to_glib_none().0,
                msgid.to_glib_none().0,
            ))
        })
    })
}

#[doc(alias = "g_dcgettext")]
pub fn dcgettext(
    domain: Option<impl IntoGStr>,
    msgid: impl IntoGStr,
    category: LocaleCategory,
) -> &'static GStr {
    domain.run_with_gstr(|domain| {
        msgid.run_with_gstr(|msgid| unsafe {
            from_glib_none(glib::ffi::g_dcgettext(
                domain.to_glib_none().0,
                msgid.to_glib_none().0,
                category as c_int,
            ))
        })
    })
}

pub fn ngettext(
    msgid: impl IntoGStr,
    msgid_plural: impl IntoGStr,
    n: libc::c_ulong,
) -> &'static GStr {
    msgid.run_with_gstr(|msgid| {
        msgid_plural.run_with_gstr(|msgid_plural| unsafe {
            from_glib_none(glib::ffi::g_dngettext(
                std::ptr::null(),
                msgid.to_glib_none().0,
                msgid_plural.to_glib_none().0,
                n,
            ))
        })
    })
}

#[doc(alias = "g_dngettext")]
pub fn dngettext(
    domain: impl IntoGStr,
    msgid: impl IntoGStr,
    msgid_plural: impl IntoGStr,
    n: libc::c_ulong,
) -> &'static GStr {
    domain.run_with_gstr(|domain| {
        msgid.run_with_gstr(|msgid| {
            msgid_plural.run_with_gstr(|msgid_plural| unsafe {
                from_glib_none(glib::ffi::g_dngettext(
                    domain.to_glib_none().0,
                    msgid.to_glib_none().0,
                    msgid_plural.to_glib_none().0,
                    n,
                ))
            })
        })
    })
}

#[inline]
#[doc(hidden)]
pub fn pgettext_aux<'m>(
    domain: Option<impl IntoGStr>,
    msg_ctxt_id: &GStr,
    msgid: &'m GStr,
    category: LocaleCategory,
) -> &'m GStr {
    domain.run_with_gstr(|domain| unsafe {
        let translation = glib::ffi::g_dcgettext(
            domain.to_glib_none().0,
            msg_ctxt_id.to_glib_none().0,
            category as c_int,
        );
        if translation == msg_ctxt_id.as_ptr() {
            msgid
        } else {
            from_glib_none(translation)
        }
    })
}

#[inline]
#[doc(hidden)]
pub fn npgettext_aux<'m>(
    domain: Option<impl IntoGStr>,
    msg_ctxt_id: &GStr,
    msgid: &'m GStr,
    msgid_plural: &'m GStr,
    n: libc::c_ulong,
) -> &'m GStr {
    domain.run_with_gstr(|domain| unsafe {
        let translation = glib::ffi::g_dngettext(
            domain.to_glib_none().0,
            msg_ctxt_id.to_glib_none().0,
            msgid_plural.to_glib_none().0,
            n,
        );
        if translation == msg_ctxt_id.as_ptr() || translation == msgid_plural.as_ptr() {
            if n == 1 {
                msgid
            } else {
                msgid_plural
            }
        } else {
            from_glib_none(translation)
        }
    })
}

macro_rules! concat_ctx {
    ($context:expr, $msgid:expr) => {{
        let mut buf = smallvec::SmallVec::<[u8; 1024]>::from_vec($context.as_bytes().to_vec());
        buf.push(0x04);
        buf.extend_from_slice($msgid.as_bytes());
        buf.push(0x00);
        buf
    }};
}

#[inline]
pub fn pgettext_expr<'m>(context: &str, msgid: &'m GStr) -> &'m GStr {
    dcpgettext_expr(GStr::NONE, context, msgid, LocaleCategory::Messages)
}

#[inline]
#[doc(alias = "g_dpgettext2")]
pub fn dpgettext_expr<'m>(domain: impl IntoGStr, context: &str, msgid: &'m GStr) -> &'m GStr {
    dcpgettext_expr(Some(domain), context, msgid, LocaleCategory::Messages)
}

#[inline]
pub fn dcpgettext_expr<'m>(
    domain: Option<impl IntoGStr>,
    context: &str,
    msgid: &'m GStr,
    category: LocaleCategory,
) -> &'m GStr {
    let msg_ctxt_id = concat_ctx!(context, msgid);
    let msg_ctxt_id = unsafe { std::mem::transmute(msg_ctxt_id.as_slice()) };
    pgettext_aux(domain, msg_ctxt_id, msgid, category)
}

#[inline]
pub fn npgettext_expr<'m>(
    context: &str,
    msgid: &'m GStr,
    msgid_plural: &'m GStr,
    n: libc::c_ulong,
) -> &'m GStr {
    let msg_ctxt_id = concat_ctx!(context, msgid);
    let msg_ctxt_id = unsafe { std::mem::transmute(msg_ctxt_id.as_slice()) };
    npgettext_aux(GStr::NONE, msg_ctxt_id, msgid, msgid_plural, n)
}

#[inline]
pub fn dnpgettext_expr<'m>(
    domain: impl IntoGStr,
    context: &str,
    msgid: &'m GStr,
    msgid_plural: &'m GStr,
    n: libc::c_ulong,
) -> &'m GStr {
    let msg_ctxt_id = concat_ctx!(context, msgid);
    let msg_ctxt_id = unsafe { std::mem::transmute(msg_ctxt_id.as_slice()) };
    npgettext_aux(Some(domain), msg_ctxt_id, msgid, msgid_plural, n)
}

pub unsafe fn textdomain(domainname: impl IntoGStr) -> io::Result<()> {
    domainname.run_with_gstr(|domainname| {
        check_ret(unsafe { ffi::textdomain(domainname.to_glib_none().0) })?;
        Ok(())
    })
}

pub unsafe fn bindtextdomain(domainname: impl IntoGStr, dirname: impl IntoGStr) -> io::Result<()> {
    domainname.run_with_gstr(|domainname| {
        dirname.run_with_gstr(|dirname| {
            check_ret(unsafe {
                ffi::bindtextdomain(domainname.to_glib_none().0, dirname.to_glib_none().0)
            })?;
            Ok(())
        })
    })
}

pub unsafe fn bind_textdomain_codeset(
    domainname: impl IntoGStr,
    codeset: impl IntoGStr,
) -> io::Result<()> {
    domainname.run_with_gstr(|domainname| {
        codeset.run_with_gstr(|codeset| {
            check_ret(unsafe {
                ffi::bind_textdomain_codeset(domainname.to_glib_none().0, codeset.to_glib_none().0)
            })?;
            Ok(())
        })
    })
}

#[inline]
fn check_ret(ptr: *mut c_char) -> io::Result<*mut c_char> {
    if ptr.is_null() {
        Err(io::Error::last_os_error())
    } else {
        Ok(ptr)
    }
}

pub unsafe fn get_textdomain() -> io::Result<GString> {
    Ok(from_glib_none(check_ret(
        ffi::textdomain(std::ptr::null()),
    )?))
}

pub unsafe fn get_domain_directory(domainname: impl IntoGStr) -> GString {
    domainname.run_with_gstr(|domainname| {
        from_glib_none(ffi::bindtextdomain(
            domainname.to_glib_none().0,
            std::ptr::null(),
        ))
    })
}

pub unsafe fn get_textdomain_codeset(domainname: impl IntoGStr) -> Option<GString> {
    domainname.run_with_gstr(|domainname| {
        from_glib_none(ffi::bind_textdomain_codeset(
            domainname.to_glib_none().0,
            std::ptr::null(),
        ))
    })
}

#[macro_export]
macro_rules! gettext {
    ($msgid:literal $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::gettext($crate::glib::gstr!($msgid))
        )
    };
    ($msgid:literal $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::gettext(msgid);
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! dgettext {
    ($domain:literal, $msgid:literal $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::dgettext($crate::glib::gstr!($domain), $crate::glib::gstr!($msgid))
        )
    };
    ($domain:literal, $msgid:literal $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::dgettext($crate::glib::gstr!($domain), msgid);
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! cgettext {
    ($msgid:literal, $category:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::dcgettext($crate::glib::GStr::NONE, $crate::glib::gstr!($msgid), $category)
        )
    };
    ($msgid:literal, $category:expr, $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::dcgettext($crate::glib::GStr::NONE, msgid, $category);
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! dcgettext {
    ($domain:literal, $msgid:literal, $category:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::dcgettext(
                ::std::option::Option::Some($crate::glib::gstr!($domain)),
                $crate::glib::gstr!($msgid),
                $category,
            )
        )
    };
    ($domain:literal, $msgid:literal, $category:expr, $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::dcgettext(
            ::std::option::Option::Some($crate::glib::gstr!($domain)),
            msgid,
            $category,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! ngettext {
    ($msgid:literal, $msgid_plural:literal, $n:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::ngettext(
                $crate::glib::gstr!($msgid),
                $crate::glib::gstr!($msgid_plural),
                $n,
            )
        )
    };
    ($msgid:literal, $msgid_plural:literal, $n:expr $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let msgid_plural = $crate::glib::gstr!($msgid_plural);
        let n = $n;
        let s = $crate::ngettext(msgid, msgid_plural, n);
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned({
            if n == 1 && s.as_ptr() == msgid.as_ptr() {
                $crate::glib::gformat!($msgid $(, $args)+)
            } else if n != 1 && s.as_ptr() == msgid_plural.as_ptr() {
                $crate::glib::gformat!($msgid_plural $(, $args)+)
            } else {
                $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
            }
        })
    }};
}

#[macro_export]
macro_rules! dngettext {
    ($domain:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::dngettext(
                $crate::glib::gstr!($domain),
                $crate::glib::gstr!($msgid),
                $crate::glib::gstr!($msgid_plural),
                $n,
            )
        )
    };
    ($domain:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let msgid_plural = $crate::glib::gstr!($msgid_plural);
        let n = $n;
        let s = $crate::dngettext($crate::glib::gstr!($domain), $msgid, $msgid_plural, n);
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned({
            if n == 1 && s.as_ptr() == msgid.as_ptr() {
                $crate::glib::gformat!($msgid $(, $args)+)
            } else if n != 1 && s.as_ptr() == msgid_plural.as_ptr() {
                $crate::glib::gformat!($msgid_plural $(, $args)+)
            } else {
                $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
            }
        })
    }};
}

#[macro_export]
macro_rules! pgettext {
    ($context:literal, $msgid:literal $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::pgettext_aux(
                $crate::glib::GStr::NONE,
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $crate::LocaleCategory::Messages,
            )
        )
    };
    ($context:literal, $msgid:literal $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::pgettext_aux(
            $crate::glib::GStr::NONE,
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            $crate::LocaleCategory::Messages,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! dpgettext {
    ($domain:literal, $context:literal, $msgid:literal $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::pgettext_aux(
                ::std::option::Option::Some($crate::glib::gstr!($domain)),
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $crate::LocaleCategory::Messages,
            )
        )
    };
    ($domain:literal, $context:literal, $msgid:literal $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::pgettext_aux(
            ::std::option::Option::Some($crate::glib::gstr!($domain)),
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            $crate::LocaleCategory::Messages,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! cpgettext {
    ($context:literal, $msgid:literal, $category:expr, $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::pgettext_aux(
                $crate::glib::GStr::NONE,
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $category,
            )
        )
    };
    ($context:literal, $msgid:literal, $category:expr, $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::pgettext_aux(
            $crate::glib::GStr::NONE,
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            $category,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! dcpgettext {
    ($domain:literal, $context:literal, $msgid:literal, $category:expr, $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::pgettext_aux(
                ::std::option::Option::Some($crate::glib::gstr!($domain)),
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $category,
            )
        )
    };
    ($domain:literal, $context:literal, $msgid:literal, $category:expr, $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let s = $crate::pgettext_aux(
            ::std::option::Option::Some($crate::glib::gstr!($domain)),
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            $category,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned(if s.as_ptr() == msgid.as_ptr() {
            $crate::glib::gformat!($msgid $(, $args)+)
        } else {
            $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
        })
    }};
}

#[macro_export]
macro_rules! npgettext {
    ($context:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::npgettext_aux(
                $crate::glib::GStr::NONE,
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $crate::glib::gstr!($msgid_plural),
                $n,
            )
        )
    };
    ($context:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let msgid_plural = $crate::glib::gstr!($msgid_plural);
        let n = $n;
        let s = $crate::npgettext_aux(
            $crate::glib::GStr::NONE,
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            msgid_plural,
            n,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned({
            if n == 1 && s.as_ptr() == msgid.as_ptr() {
                $crate::glib::gformat!($msgid $(, $args)+)
            } else if n != 1 && s.as_ptr() == msgid_plural.as_ptr() {
                $crate::glib::gformat!($msgid_plural $(, $args)+)
            } else {
                $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
            }
        })
    }};
}

#[macro_export]
macro_rules! dnpgettext {
    ($domain:literal, $context:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(,)?) => {
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Borrowed(
            $crate::npgettext_aux(
                ::std::option::Option::Some($crate::glib::gstr!($domain)),
                $crate::glib::gstr_concat!($context, '\x04', $msgid),
                $crate::glib::gstr!($msgid),
                $crate::glib::gstr!($msgid_plural),
                $n,
            )
        )
    };
    ($domain:literal, $context:literal, $msgid:literal, $msgid_plural:literal, $n:expr $(, $args:expr)+ $(,)?) => {{
        let msgid = $crate::glib::gstr!($msgid);
        let msgid_plural = $crate::glib::gstr!($msgid_plural);
        let n = $n;
        let s = $crate::npgettext_aux(
            ::std::option::Option::Some($crate::glib::gstr!($domain)),
            $crate::glib::gstr_concat!($context, '\x04', $msgid),
            msgid,
            msgid_plural,
            n,
        );
        ::std::borrow::Cow::<'static, $crate::glib::GStr>::Owned({
            if n == 1 && s.as_ptr() == msgid.as_ptr() {
                $crate::glib::gformat!($msgid $(, $args)+)
            } else if n != 1 && s.as_ptr() == msgid_plural.as_ptr() {
                $crate::glib::gformat!($msgid_plural $(, $args)+)
            } else {
                $crate::glib::i18n_format!($crate::glib, s.as_str(), $msgid $(, $args)+)
            }
        })
    }};
}
