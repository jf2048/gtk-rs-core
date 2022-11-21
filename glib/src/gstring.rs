// Take a look at the license at the top of the repository in the LICENSE file.

use crate::translate::*;
use crate::types::{StaticType, Type};
use std::borrow::Borrow;
use std::cmp::Ordering;
use std::ffi::{CStr, CString, OsStr};
use std::fmt;
use std::hash;
use std::mem;
use std::ops::Deref;
use std::os::raw::{c_char, c_void};
use std::path::Path;
use std::ptr;
use std::slice;
use std::string::String;

// rustdoc-stripper-ignore-next
/// Representaion of a borrowed [`GString`].
///
/// This type is very similar to [`std::ffi::CStr`], but with one added constraint: the string
/// must also be valid UTF-8.
#[derive(Debug, Eq, Hash, Ord, PartialEq, PartialOrd)]
#[repr(transparent)]
pub struct GStr(str);

impl GStr {
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a byte slice.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring that the byte
    /// slice is valid UTF-8 and is nul-terminated.
    #[inline]
    pub fn from_utf8_with_nul(bytes: &[u8]) -> Result<&Self, GStrError> {
        Self::check_trailing_nul(bytes)?;
        std::str::from_utf8(bytes)?;
        Ok(unsafe { mem::transmute(bytes) })
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a byte slice, checking for interior nul-bytes.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring that the byte
    /// slice is valid UTF-8, is nul-terminated, and does not contain any interior nul-bytes.
    #[inline]
    pub fn from_utf8_with_nul_checked(bytes: &[u8]) -> Result<&Self, GStrError> {
        Self::check_nuls(bytes)?;
        std::str::from_utf8(bytes)?;
        Ok(unsafe { mem::transmute(bytes) })
    }
    // rustdoc-stripper-ignore-next
    /// Unsafely creates a GLib string wrapper from a byte slice.
    ///
    /// This function will cast the provided `bytes` to a `GStr` wrapper without performing any
    /// sanity checks.
    ///
    /// # Safety
    ///
    /// The provided slice **must** be valid UTF-8 and nul-terminated. It is undefined behavior to
    /// pass a slice that does not uphold those conditions.
    #[inline]
    pub const unsafe fn from_utf8_with_nul_unchecked(bytes: &[u8]) -> &Self {
        debug_assert!(!bytes.is_empty() && bytes[bytes.len() - 1] == 0);
        debug_assert!(std::str::from_utf8(bytes).is_ok());
        mem::transmute(bytes)
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a byte slice, truncating it at the first nul-byte.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring that the byte
    /// slice is valid UTF-8 and contains at least one nul-byte.
    #[inline]
    pub fn from_utf8_until_nul(bytes: &[u8]) -> Result<&Self, GStrError> {
        let nul_pos = memchr::memchr(0, bytes).ok_or(GStrError::NoTrailingNul)?;
        let bytes = unsafe { bytes.get_unchecked(..nul_pos + 1) };
        std::str::from_utf8(bytes)?;
        Ok(unsafe { mem::transmute(bytes) })
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a string slice.
    ///
    /// The string slice must be terminated with a nul-byte.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring
    /// that the string slice is nul-terminated.
    #[inline]
    pub fn from_str_with_nul(s: &str) -> Result<&Self, GStrError> {
        Self::check_trailing_nul(s)?;
        Ok(unsafe { mem::transmute(s) })
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a string slice, checking for interior nul-bytes.
    ///
    /// The string slice must be terminated with a nul-byte.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring
    /// that the string slice is nul-terminated and does not contain any interior nul-bytes.
    #[inline]
    pub fn from_str_with_nul_checked(s: &str) -> Result<&Self, GStrError> {
        Self::check_nuls(s)?;
        Ok(unsafe { mem::transmute(s) })
    }
    // rustdoc-stripper-ignore-next
    /// Unsafely creates a GLib string wrapper from a string slice. The string slice must be
    /// terminated with a nul-byte.
    ///
    /// This function will cast the provided string slice to a `GStr` without performing any sanity
    /// checks.
    ///
    /// # Safety
    ///
    /// The provided string slice **must** be nul-terminated. It is undefined behavior to pass a
    /// slice that does not uphold those conditions.
    #[inline]
    pub const unsafe fn from_str_with_nul_unchecked(s: &str) -> &Self {
        debug_assert!(!s.is_empty() && s.as_bytes()[s.len() - 1] == 0);
        mem::transmute(s)
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string wrapper from a string slice, truncating it at the first nul-byte.
    ///
    /// The string slice must contain at least one nul-byte.
    ///
    /// This function will cast the provided bytes to a `GStr` wrapper after ensuring
    /// that the string slice contains at least one nul-byte.
    #[inline]
    pub fn from_str_until_nul(s: &str) -> Result<&Self, GStrError> {
        let b = s.as_bytes();
        let nul_pos = memchr::memchr(0, b).ok_or(GStrError::NoTrailingNul)?;
        let s = unsafe { std::str::from_utf8_unchecked(b.get_unchecked(..nul_pos + 1)) };
        Ok(unsafe { mem::transmute(s) })
    }
    // rustdoc-stripper-ignore-next
    /// Wraps a raw C string with a safe GLib string wrapper. The provided C string **must** be
    /// valid UTF-8 and nul-terminated. All constraints from [`CStr::from_ptr`] also apply here.
    ///
    /// # Safety
    ///
    /// See [`CStr::from_ptr`](CStr::from_ptr#safety).
    #[inline]
    pub unsafe fn from_ptr<'a>(ptr: *const c_char) -> &'a Self {
        let cstr = CStr::from_ptr(ptr);
        Self::from_utf8_with_nul_unchecked(cstr.to_bytes_with_nul())
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a byte slice containing the trailing 0 byte.
    ///
    /// This function is the equivalent of [`GStr::to_bytes`] except that it will retain the
    /// trailing nul terminator instead of chopping it off.
    #[inline]
    pub const fn as_bytes_with_nul(&self) -> &[u8] {
        self.0.as_bytes()
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a byte slice.
    ///
    /// The returned slice will **not** contain the trailing nul terminator that this GLib
    /// string has.
    #[inline]
    pub const fn as_bytes(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
    // rustdoc-stripper-ignore-next
    /// Returns the inner pointer to this GLib string.
    ///
    /// The returned pointer will be valid for as long as `self` is, and points to a contiguous
    /// region of memory terminated with a 0 byte to represent the end of the string.
    ///
    /// **WARNING**
    ///
    /// The returned pointer is read-only; writing to it (including passing it to C code that
    /// writes to it) causes undefined behavior. It is your responsibility to make
    /// sure that the underlying memory is not freed too early.
    #[inline]
    pub const fn as_ptr(&self) -> *const c_char {
        self.0.as_ptr() as *const _
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a string slice.
    #[inline]
    pub const fn as_str(&self) -> &str {
        // Clip off the nul-byte
        unsafe {
            std::str::from_utf8_unchecked(std::slice::from_raw_parts(
                self.as_ptr() as *const _,
                self.0.len() - 1,
            ))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a C string slice, checking for interior nul-bytes.
    ///
    /// Returns `Err` if the string contains any interior nul-bytes.
    #[inline]
    pub fn to_cstr(&self) -> Result<&CStr, GStrInteriorNulError> {
        Self::check_interior_nuls(self.as_bytes())?;
        Ok(unsafe { self.to_cstr_unchecked() })
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a C string slice, truncating it at the first nul-byte.
    #[inline]
    pub fn to_cstr_until_nul(&self) -> &CStr {
        let b = self.as_bytes_with_nul();
        let nul_pos = memchr::memchr(0, b).unwrap();
        unsafe { CStr::from_bytes_with_nul_unchecked(b.get_unchecked(..nul_pos + 1)) }
    }
    // rustdoc-stripper-ignore-next
    /// Converts this GLib string to a C string slice, without checking for interior nul-bytes.
    ///
    /// # Safety
    ///
    /// `self` **must** not contain any interior nul-bytes besides the final terminating nul-byte.
    /// It is undefined behavior to call this on a string that contains interior nul-bytes.
    #[inline]
    pub const unsafe fn to_cstr_unchecked(&self) -> &CStr {
        CStr::from_bytes_with_nul_unchecked(self.as_bytes_with_nul())
    }

    fn check_nuls(s: impl AsRef<[u8]>) -> Result<(), GStrError> {
        let s = s.as_ref();
        if let Some(nul_pos) = memchr::memchr(0, s) {
            if s.len() == nul_pos + 1 {
                Ok(())
            } else {
                Err(GStrInteriorNulError(nul_pos).into())
            }
        } else {
            Err(GStrError::NoTrailingNul)
        }
    }
    #[inline]
    fn check_trailing_nul(s: impl AsRef<[u8]>) -> Result<(), GStrError> {
        if let Some(c) = s.as_ref().last().copied() {
            if c == 0 {
                return Ok(());
            }
        }
        Err(GStrError::NoTrailingNul)
    }
    // rustdoc-stripper-ignore-next
    /// Returns `Err` if the string slice contains any nul-bytes.
    #[inline]
    pub(crate) fn check_interior_nuls(s: impl AsRef<[u8]>) -> Result<(), GStrInteriorNulError> {
        if let Some(nul_pos) = memchr::memchr(0, s.as_ref()) {
            Err(GStrInteriorNulError(nul_pos))
        } else {
            Ok(())
        }
    }
}

// rustdoc-stripper-ignore-next
/// Error type holding all possible failures when creating a [`GStr`] reference.
#[derive(thiserror::Error, Debug)]
pub enum GStrError {
    #[error(transparent)]
    InvalidUtf8(#[from] std::str::Utf8Error),
    #[error(transparent)]
    InteriorNul(#[from] GStrInteriorNulError),
    #[error("data provided is not nul terminated")]
    NoTrailingNul,
}

// rustdoc-stripper-ignore-next
/// Error type indicating that a buffer had unexpected nul-bytes.
#[derive(thiserror::Error, Copy, Clone, PartialEq, Eq, Debug)]
#[error("data provided contains an interior nul-byte at byte pos {0}")]
pub struct GStrInteriorNulError(usize);

impl GStrInteriorNulError {
    // rustdoc-stripper-ignore-next
    /// Returns the position of the nul-byte in the slice that caused the conversion to fail.
    #[inline]
    pub fn nul_position(&self) -> usize {
        self.0
    }
}

// rustdoc-stripper-ignore-next
/// Converts a static string literal into a static nul-terminated string.
///
/// The expanded expression has type [`&'static GStr`]. This macro will panic if the
/// string literal contains any interior nul-bytes.
///
/// # Examples
///
/// ```
/// # fn main() {
/// use glib::{gstr, GStr, GString};
///
/// const MY_STRING: &GStr = gstr!("Hello");
/// assert_eq!(MY_STRING.as_bytes_with_nul()[5], 0u8);
/// let owned: GString = MY_STRING.to_owned();
/// assert_eq!(MY_STRING, owned);
/// # }
/// ```
///
/// [`&'static GStr`]: crate::GStr
#[macro_export]
macro_rules! gstr {
    ($s:literal) => {
        unsafe { $crate::GStr::from_utf8_with_nul_unchecked($crate::cstr_bytes!($s)) }
    };
}

impl Default for &GStr {
    fn default() -> Self {
        const SLICE: &[c_char] = &[0];
        unsafe { GStr::from_ptr(SLICE.as_ptr()) }
    }
}

impl fmt::Display for GStr {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl<'a> TryFrom<&'a CStr> for &'a GStr {
    type Error = std::str::Utf8Error;
    #[inline]
    fn try_from(s: &'a CStr) -> Result<Self, Self::Error> {
        s.to_str()?;
        Ok(unsafe { GStr::from_utf8_with_nul_unchecked(s.to_bytes_with_nul()) })
    }
}

impl PartialEq<GStr> for String {
    fn eq(&self, other: &GStr) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<str> for GStr {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for GStr {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<GStr> for &str {
    fn eq(&self, other: &GStr) -> bool {
        *self == other.as_str()
    }
}

impl PartialEq<String> for GStr {
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<GStr> for str {
    fn eq(&self, other: &GStr) -> bool {
        self == other.as_str()
    }
}

impl PartialOrd<GStr> for String {
    fn partial_cmp(&self, other: &GStr) -> Option<Ordering> {
        Some(self.cmp(&String::from(other.as_str())))
    }
}

impl PartialOrd<String> for GStr {
    fn partial_cmp(&self, other: &String) -> Option<Ordering> {
        Some(self.as_str().cmp(other.as_str()))
    }
}

impl PartialOrd<GStr> for str {
    fn partial_cmp(&self, other: &GStr) -> Option<Ordering> {
        Some(self.cmp(other.as_str()))
    }
}

impl PartialOrd<str> for GStr {
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        Some(self.as_str().cmp(other))
    }
}

impl AsRef<GStr> for GStr {
    fn as_ref(&self) -> &GStr {
        self
    }
}

impl AsRef<str> for GStr {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for GStr {
    fn as_ref(&self) -> &OsStr {
        OsStr::new(self.as_str())
    }
}

impl AsRef<Path> for GStr {
    fn as_ref(&self) -> &Path {
        Path::new(self.as_str())
    }
}

impl AsRef<[u8]> for GStr {
    fn as_ref(&self) -> &[u8] {
        self.as_bytes()
    }
}

impl Deref for GStr {
    type Target = str;

    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl ToOwned for GStr {
    type Owned = GString;

    #[inline]
    fn to_owned(&self) -> Self::Owned {
        if self.is_empty() {
            return GString::default();
        }
        // Always copy with the GLib allocator
        let b = self.as_bytes_with_nul();
        let inner = unsafe {
            let copy = ffi::g_strndup(b.as_ptr() as *const c_char, b.len());
            Inner::Foreign {
                ptr: ptr::NonNull::new_unchecked(copy),
                len: b.len() - 1,
            }
        };
        GString(inner)
    }
}

impl GlibPtrDefault for GStr {
    type GlibType = *mut c_char;
}

impl StaticType for GStr {
    fn static_type() -> Type {
        str::static_type()
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*const u8> for &GStr {
    #[inline]
    unsafe fn from_glib_none(ptr: *const u8) -> Self {
        assert!(!ptr.is_null(), "provided C string is NULL");
        let cstr = CStr::from_ptr(ptr as *const _);
        // Also check if it's valid UTF-8
        GStr::from_str_with_nul_unchecked(cstr.to_str().unwrap())
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*const i8> for &GStr {
    #[inline]
    unsafe fn from_glib_none(ptr: *const i8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*mut u8> for &GStr {
    #[inline]
    unsafe fn from_glib_none(ptr: *mut u8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*mut i8> for &GStr {
    #[inline]
    unsafe fn from_glib_none(ptr: *mut i8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

unsafe impl<'a> crate::value::FromValue<'a> for &'a GStr {
    type Checker = crate::value::GenericValueTypeOrNoneChecker<Self>;

    unsafe fn from_value(value: &'a crate::Value) -> Self {
        let ptr = gobject_ffi::g_value_get_string(value.to_glib_none().0);
        let cstr = CStr::from_ptr(ptr);
        assert!(
            cstr.to_str().is_ok(),
            "C string in glib::Value is not valid utf-8"
        );
        GStr::from_utf8_with_nul_unchecked(cstr.to_bytes_with_nul())
    }
}

impl crate::value::ToValue for GStr {
    #[inline]
    fn to_value(&self) -> crate::Value {
        self.as_str().to_value()
    }

    #[inline]
    fn value_type(&self) -> Type {
        str::static_type()
    }
}

impl crate::value::ToValue for &GStr {
    #[inline]
    fn to_value(&self) -> crate::Value {
        (*self).to_value()
    }

    #[inline]
    fn value_type(&self) -> Type {
        str::static_type()
    }
}

impl crate::value::ToValueOptional for GStr {
    #[inline]
    fn to_value_optional(s: Option<&Self>) -> crate::Value {
        crate::value::ToValueOptional::to_value_optional(s.map(|s| s.as_str()))
    }
}

#[doc(hidden)]
impl<'a> ToGlibPtr<'a, *const c_char> for GStr {
    type Storage = &'a Self;

    #[inline]
    fn to_glib_none(&'a self) -> Stash<'a, *const c_char, Self> {
        Stash(self.as_ptr() as *const _, self)
    }

    #[inline]
    fn to_glib_full(&self) -> *const c_char {
        self.as_str().to_glib_full()
    }
}

#[doc(hidden)]
impl<'a> ToGlibPtr<'a, *mut c_char> for GStr {
    type Storage = &'a Self;

    #[inline]
    fn to_glib_none(&'a self) -> Stash<'a, *mut c_char, Self> {
        Stash(self.as_ptr() as *mut _, self)
    }

    #[inline]
    fn to_glib_full(&self) -> *mut c_char {
        self.as_str().to_glib_full()
    }
}

// rustdoc-stripper-ignore-next
/// A type representing an owned, C-compatible, nul-terminated UTF-8 string.
///
/// `GString` is to <code>&[GStr]</code> as [`String`] is to <code>&[str]</code>: the former in
/// each pair are owned strings; the latter are borrowed references.
///
/// This type is similar to [`std::ffi::CString`], but with some special behavior. When debug
/// assertions are enabled, <code>[From]&lt;[String]></code> will panic if there are interior
/// nul-bytes. In production builds, no checks will be made for interior nul-bytes, and strings
/// that contain interior nul-bytes will simply end at first nul-byte when converting to a C
/// string.
///
/// The constructors beginning with `from_utf8` `and `from_string` can also be used to further
/// control how interior nul-bytes are handled.
#[repr(transparent)]
pub struct GString(Inner);
enum Inner {
    Native(Option<Box<str>>),
    Foreign {
        ptr: ptr::NonNull<c_char>,
        len: usize,
    },
}

unsafe impl Send for GString {}
unsafe impl Sync for GString {}

impl GString {
    // rustdoc-stripper-ignore-next
    /// Creates a new empty [`GString`].
    ///
    /// Does not allocate.
    #[inline]
    pub fn new() -> Self {
        Self(Inner::Native(None))
    }
    // rustdoc-stripper-ignore-next
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a byte vector.
    ///
    /// Takes ownership of `bytes`. Returns `Err` if it contains invalid UTF-8.
    ///
    /// A trailing nul-byte will be appended by this function.
    #[inline]
    pub fn from_utf8(bytes: Vec<u8>) -> Result<Self, std::string::FromUtf8Error> {
        Ok(Self::from_string_unchecked(String::from_utf8(bytes)?))
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a byte vector, checking for interior nul-bytes.
    ///
    /// Takes ownership of `bytes`, as long as it is valid UTF-8 and does not contain any interior
    /// nul-bytes. Otherwise, `Err` is returned.
    ///
    /// A trailing nul-byte will be appended by this function.
    #[inline]
    pub fn from_utf8_checked(bytes: Vec<u8>) -> Result<Self, GStringFromError<Vec<u8>>> {
        Ok(Self::from_string_checked(String::from_utf8(bytes)?)
            .map_err(|e| GStringInteriorNulError(e.0.into_bytes(), e.1))?)
    }
    // rustdoc-stripper-ignore-next
    /// Unsafely creates a GLib string by consuming a byte vector, without checking for UTF-8 or
    /// interior nul-bytes.
    ///
    /// A trailing nul-byte will be appended by this function.
    ///
    /// # Safety
    ///
    /// The byte vector **must** not contain invalid UTF-8 characters. It is undefined behavior to
    /// pass a vector that contains invalid UTF-8.
    #[inline]
    pub unsafe fn from_utf8_unchecked(mut v: Vec<u8>) -> Self {
        if v.is_empty() {
            Self(Inner::Native(None))
        } else {
            v.reserve_exact(1);
            v.push(0);
            Self(Inner::Native(Some(String::from_utf8_unchecked(v).into())))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a nul-terminated byte vector, without checking for
    /// interior nul-bytes.
    ///
    /// Takes ownership of `bytes`. Returns `Err` if it contains invalid UTF-8 or does not have a
    /// trailing nul-byte.
    #[inline]
    pub fn from_utf8_with_nul(bytes: Vec<u8>) -> Result<Self, GStringFromError<Vec<u8>>> {
        let s = String::from_utf8(bytes)?;
        if s.as_bytes().last().copied() != Some(0u8) {
            return Err(GStringNoTrailingNulError(s.into_bytes()).into());
        }
        if s.len() == 1 {
            Ok(Self(Inner::Native(None)))
        } else {
            Ok(Self(Inner::Native(Some(s.into()))))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a nul-terminated byte vector.
    ///
    /// Takes ownership of `bytes`. Returns `Err` if it contains invalid UTF-8, does not have a
    /// trailing nul-byte, or contains interior nul-bytes.
    #[inline]
    pub fn from_utf8_with_nul_checked(bytes: Vec<u8>) -> Result<Self, GStringFromError<Vec<u8>>> {
        let s = Self::from_utf8_with_nul(bytes)?;
        if let Err(e) = GStr::check_interior_nuls(&s) {
            return Err(GStringInteriorNulError(s.into_bytes(), e).into());
        }
        Ok(s)
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a byte vector, without checking for UTF-8, a trailing
    /// nul-byte, or interior nul-bytes.
    ///
    /// # Safety
    ///
    /// The byte vector **must** not contain invalid UTF-8 characters, and **must** have a trailing
    /// nul-byte. It is undefined behavior to pass a vector that does not uphold those conditions.
    #[inline]
    pub unsafe fn from_utf8_with_nul_unchecked(v: Vec<u8>) -> Self {
        debug_assert!(!v.is_empty() && v[v.len() - 1] == 0);
        let s = if cfg!(debug_assertions) {
            let s = String::from_utf8(v).unwrap();
            GStr::check_interior_nuls(&s[..s.len() - 1]).unwrap();
            s
        } else {
            String::from_utf8_unchecked(v)
        };
        if s.len() == 1 {
            Self(Inner::Native(None))
        } else {
            Self(Inner::Native(Some(s.into())))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a nul-terminated byte vector, truncating it at the first
    /// nul-byte.
    ///
    /// Takes ownership of `bytes`. Returns `Err` if it contains invalid UTF-8 or does not contain
    /// at least one nul-byte.
    pub fn from_utf8_until_nul(mut bytes: Vec<u8>) -> Result<Self, GStringFromError<Vec<u8>>> {
        let nul_pos = if let Some(nul_pos) = memchr::memchr(0, &bytes) {
            nul_pos
        } else {
            return Err(GStringNoTrailingNulError(bytes).into());
        };
        if nul_pos == 0 {
            Ok(Self(Inner::Native(None)))
        } else {
            if let Err(e) = std::str::from_utf8(unsafe { bytes.get_unchecked(..nul_pos) }) {
                return Err(GStringUtf8Error(bytes, e).into());
            }
            bytes.truncate(nul_pos + 1);
            let s = unsafe { String::from_utf8_unchecked(bytes) };
            Ok(Self(Inner::Native(Some(s.into()))))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a string, checking for interior nul-bytes.
    ///
    /// Takes ownership of `s`, as long as it does not contain any interior nul-bytes. Otherwise,
    /// `Err` is returned.
    ///
    /// A trailing nul-byte will be appended by this function.
    #[inline]
    pub fn from_string_checked(s: String) -> Result<Self, GStringInteriorNulError<String>> {
        if let Err(e) = GStr::check_interior_nuls(&s) {
            return Err(GStringInteriorNulError(s, e));
        }
        Ok(Self::from_string_unchecked(s))
    }
    // rustdoc-stripper-ignore-next
    /// Creates a GLib string by consuming a string, without checking for interior nul-bytes.
    ///
    /// A trailing nul-byte will be appended by this function.
    #[inline]
    pub fn from_string_unchecked(mut s: String) -> Self {
        if s.is_empty() {
            Self(Inner::Native(None))
        } else {
            s.reserve_exact(1);
            s.push('\0');
            Self(Inner::Native(Some(s.into())))
        }
    }
    // rustdoc-stripper-ignore-next
    /// Return the `GString` as string slice.
    #[inline]
    pub fn as_str(&self) -> &str {
        unsafe {
            let (ptr, len) = match self.0 {
                Inner::Native(None) => (ptr::null(), 0),
                Inner::Native(Some(ref s)) => (s.as_ptr() as *const u8, s.len() - 1),
                Inner::Foreign { ptr, len } => (ptr.as_ptr() as *const u8, len),
            };
            if len == 0 {
                ""
            } else {
                let slice = slice::from_raw_parts(ptr, len);
                std::str::from_utf8_unchecked(slice)
            }
        }
    }

    // rustdoc-stripper-ignore-next
    /// Extracts the [`GStr`] containing the entire string.
    #[inline]
    pub fn as_gstr(&self) -> &GStr {
        let bytes = match self.0 {
            Inner::Native(None) => return <&GStr>::default(),
            Inner::Native(Some(ref s)) => s.as_bytes(),
            Inner::Foreign { len, .. } if len == 0 => &[0],
            Inner::Foreign { ptr, len } => unsafe {
                slice::from_raw_parts(ptr.as_ptr() as *const _, len + 1)
            },
        };
        unsafe { GStr::from_utf8_with_nul_unchecked(bytes) }
    }

    // rustdoc-stripper-ignore-next
    /// Return the underlying pointer of the `GString`.
    #[inline]
    pub fn as_ptr(&self) -> *const c_char {
        match self.0 {
            Inner::Native(None) => <&GStr>::default().as_ptr(),
            Inner::Native(Some(ref s)) => s.as_ptr() as *const _,
            Inner::Foreign { ptr, .. } => ptr.as_ptr(),
        }
    }

    // rustdoc-stripper-ignore-next
    /// Consumes the `GString` and returns the underlying byte buffer.
    ///
    /// The returned buffer is not guaranteed to contain a trailing nul-byte.
    pub fn into_bytes(mut self) -> Vec<u8> {
        match &mut self.0 {
            Inner::Native(s) => match s.take() {
                None => Vec::new(),
                Some(s) => {
                    let mut s = String::from(s);
                    let _nul = s.pop();
                    debug_assert_eq!(_nul, Some('\0'));
                    s.into_bytes()
                }
            },
            Inner::Foreign { ptr, len } => {
                let bytes = unsafe { slice::from_raw_parts(ptr.as_ptr() as *const u8, *len - 1) };
                bytes.to_owned()
            }
        }
    }

    // rustdoc-stripper-ignore-next
    /// Consumes the `GString` and returns the underlying byte buffer, with trailing nul-byte.
    pub fn into_bytes_with_nul(mut self) -> Vec<u8> {
        match &mut self.0 {
            Inner::Native(s) => match s.take() {
                None => vec![0u8],
                Some(s) => str::into_boxed_bytes(s).into(),
            },
            Inner::Foreign { ptr, len } => {
                let bytes = unsafe { slice::from_raw_parts(ptr.as_ptr() as *const u8, *len) };
                bytes.to_owned()
            }
        }
    }
}

// rustdoc-stripper-ignore-next
/// Error type indicating that a buffer did not have a trailing nul-byte.
///
/// `T` is the type of the value the conversion was attempted from.
#[derive(thiserror::Error, Clone, PartialEq, Eq, Debug)]
#[error("data provided is not nul terminated")]
pub struct GStringNoTrailingNulError<T>(T);

impl<T> GStringNoTrailingNulError<T> {
    // rustdoc-stripper-ignore-next
    /// Returns the original value that was attempted to convert to [`GString`].
    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }
}

// rustdoc-stripper-ignore-next
/// Error type indicating that a buffer had unexpected nul-bytes.
///
/// `T` is the type of the value the conversion was attempted from.
#[derive(thiserror::Error, Clone, PartialEq, Eq, Debug)]
#[error("{1}")]
pub struct GStringInteriorNulError<T>(T, GStrInteriorNulError);

impl<T> GStringInteriorNulError<T> {
    // rustdoc-stripper-ignore-next
    /// Returns the original value that was attempted to convert to [`GString`].
    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }
    // rustdoc-stripper-ignore-next
    /// Fetch a [`GStrInteriorNulError`] to get more details about the conversion failure.
    #[inline]
    pub fn nul_error(&self) -> GStrInteriorNulError {
        self.1
    }
}

// rustdoc-stripper-ignore-next
/// Error type indicating that a buffer had invalid UTF-8.
///
/// `T` is the type of the value the conversion was attempted from.
#[derive(thiserror::Error, Clone, PartialEq, Eq, Debug)]
#[error("{1}")]
pub struct GStringUtf8Error<T>(T, std::str::Utf8Error);

impl<T> GStringUtf8Error<T> {
    // rustdoc-stripper-ignore-next
    /// Returns the original value that was attempted to convert to [`GString`].
    #[inline]
    pub fn into_inner(self) -> T {
        self.0
    }
    // rustdoc-stripper-ignore-next
    /// Fetch a [`Utf8Error`](std::str::Utf8Error) to get more details about the conversion
    /// failure.
    #[inline]
    pub fn utf8_error(&self) -> std::str::Utf8Error {
        self.1
    }
}

// rustdoc-stripper-ignore-next
/// Error type holding all possible failures when creating a [`GString`].
#[derive(thiserror::Error, Debug)]
pub enum GStringFromError<T> {
    #[error(transparent)]
    NoTrailingNul(#[from] GStringNoTrailingNulError<T>),
    #[error(transparent)]
    InteriorNul(#[from] GStringInteriorNulError<T>),
    #[error(transparent)]
    InvalidUtf8(#[from] GStringUtf8Error<T>),
    #[error("unable to convert")]
    Unspecified(T),
}

impl<T> GStringFromError<T> {
    pub fn into_inner(self) -> T {
        match self {
            Self::NoTrailingNul(GStringNoTrailingNulError(t)) => t,
            Self::InteriorNul(GStringInteriorNulError(t, _)) => t,
            Self::InvalidUtf8(GStringUtf8Error(t, _)) => t,
            Self::Unspecified(t) => t,
        }
    }
    #[inline]
    fn convert<R>(self, func: impl FnOnce(T) -> R) -> GStringFromError<R> {
        match self {
            Self::NoTrailingNul(GStringNoTrailingNulError(t)) => {
                GStringFromError::NoTrailingNul(GStringNoTrailingNulError(func(t)))
            }
            Self::InteriorNul(GStringInteriorNulError(t, e)) => {
                GStringFromError::InteriorNul(GStringInteriorNulError(func(t), e))
            }
            Self::InvalidUtf8(GStringUtf8Error(t, e)) => {
                GStringFromError::InvalidUtf8(GStringUtf8Error(func(t), e))
            }
            Self::Unspecified(t) => GStringFromError::Unspecified(func(t)),
        }
    }
}

impl From<std::string::FromUtf8Error> for GStringFromError<Vec<u8>> {
    #[inline]
    fn from(e: std::string::FromUtf8Error) -> Self {
        let ue = e.utf8_error();
        Self::InvalidUtf8(GStringUtf8Error(e.into_bytes(), ue))
    }
}

impl IntoGlibPtr<*mut c_char> for GString {
    // rustdoc-stripper-ignore-next
    /// Transform into a nul-terminated raw C string pointer.
    unsafe fn into_glib_ptr(self) -> *mut c_char {
        match self.0 {
            Inner::Native(None) => ffi::g_malloc0(1) as *mut _,
            Inner::Native(Some(ref s)) => ffi::g_strndup(s.as_ptr() as *const _, s.len()),
            Inner::Foreign { ptr, .. } => {
                let _s = mem::ManuallyDrop::new(self);
                ptr.as_ptr()
            }
        }
    }
}

impl Default for GString {
    #[inline]
    fn default() -> Self {
        Self::new()
    }
}

impl Clone for GString {
    #[inline]
    fn clone(&self) -> GString {
        self.as_str().into()
    }
}

impl fmt::Debug for GString {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        <&str as fmt::Debug>::fmt(&self.as_str(), f)
    }
}

impl Drop for GString {
    #[inline]
    fn drop(&mut self) {
        if let Inner::Foreign { ptr, .. } = self.0 {
            unsafe {
                ffi::g_free(ptr.as_ptr() as *mut _);
            }
        }
    }
}

impl fmt::Display for GString {
    #[inline]
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

impl hash::Hash for GString {
    #[inline]
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.as_str().hash(state)
    }
}

impl Borrow<GStr> for GString {
    #[inline]
    fn borrow(&self) -> &GStr {
        self.as_gstr()
    }
}

impl Borrow<str> for GString {
    #[inline]
    fn borrow(&self) -> &str {
        self.as_str()
    }
}

impl Ord for GString {
    #[inline]
    fn cmp(&self, other: &GString) -> Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl PartialOrd for GString {
    #[inline]
    fn partial_cmp(&self, other: &GString) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialEq for GString {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<GString> for String {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<GStr> for GString {
    #[inline]
    fn eq(&self, other: &GStr) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<&GStr> for GString {
    #[inline]
    fn eq(&self, other: &&GStr) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<str> for GString {
    #[inline]
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

impl PartialEq<&str> for GString {
    #[inline]
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<GString> for &GStr {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<GString> for &str {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        *self == other.as_str()
    }
}

impl PartialEq<String> for GString {
    #[inline]
    fn eq(&self, other: &String) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialEq<GString> for str {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        self == other.as_str()
    }
}

impl PartialEq<GString> for GStr {
    #[inline]
    fn eq(&self, other: &GString) -> bool {
        self.as_str() == other.as_str()
    }
}

impl PartialOrd<GString> for String {
    #[inline]
    fn partial_cmp(&self, other: &GString) -> Option<Ordering> {
        Some(self.cmp(&String::from(other.as_str())))
    }
}

impl PartialOrd<String> for GString {
    #[inline]
    fn partial_cmp(&self, other: &String) -> Option<Ordering> {
        Some(self.as_str().cmp(other.as_str()))
    }
}

impl PartialOrd<GString> for GStr {
    #[inline]
    fn partial_cmp(&self, other: &GString) -> Option<Ordering> {
        Some(self.as_str().cmp(other))
    }
}

impl PartialOrd<GStr> for GString {
    #[inline]
    fn partial_cmp(&self, other: &GStr) -> Option<Ordering> {
        Some(self.as_str().cmp(other.as_str()))
    }
}

impl PartialOrd<GString> for str {
    #[inline]
    fn partial_cmp(&self, other: &GString) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl PartialOrd<str> for GString {
    #[inline]
    fn partial_cmp(&self, other: &str) -> Option<Ordering> {
        Some(self.as_str().cmp(other))
    }
}

impl Eq for GString {}

impl AsRef<GStr> for GString {
    #[inline]
    fn as_ref(&self) -> &GStr {
        self.as_gstr()
    }
}

impl AsRef<str> for GString {
    #[inline]
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

impl AsRef<OsStr> for GString {
    #[inline]
    fn as_ref(&self) -> &OsStr {
        OsStr::new(self.as_str())
    }
}

impl AsRef<Path> for GString {
    #[inline]
    fn as_ref(&self) -> &Path {
        Path::new(self.as_str())
    }
}

impl AsRef<[u8]> for GString {
    #[inline]
    fn as_ref(&self) -> &[u8] {
        self.as_str().as_bytes()
    }
}

impl Deref for GString {
    type Target = str;

    #[inline]
    fn deref(&self) -> &str {
        self.as_str()
    }
}

impl From<GString> for String {
    #[inline]
    fn from(mut s: GString) -> Self {
        match &mut s.0 {
            Inner::Native(s) => match s.take() {
                None => Self::default(),
                Some(s) => {
                    // Moves the underlying string
                    let mut s = String::from(s);
                    let _nul = s.pop();
                    debug_assert_eq!(_nul, Some('\0'));
                    s
                }
            },
            Inner::Foreign { len, .. } if *len == 0 => String::new(),
            Inner::Foreign { ptr, len } => unsafe {
                // Creates a copy
                let slice = slice::from_raw_parts(ptr.as_ptr() as *const u8, *len);
                std::str::from_utf8_unchecked(slice).into()
            },
        }
    }
}

impl From<GString> for Box<str> {
    #[inline]
    fn from(s: GString) -> Self {
        // Potentially creates a copy
        String::from(s).into()
    }
}

impl From<String> for GString {
    #[inline]
    fn from(mut s: String) -> Self {
        // Moves the content of the String
        if cfg!(debug_assertions) {
            GStr::check_interior_nuls(&s).unwrap();
        }
        if s.is_empty() {
            Self(Inner::Native(None))
        } else {
            s.reserve_exact(1);
            s.push('\0');
            // No check for valid UTF-8 here
            Self(Inner::Native(Some(s.into())))
        }
    }
}

impl From<Box<str>> for GString {
    #[inline]
    fn from(s: Box<str>) -> Self {
        // Moves the content of the String
        s.into_string().into()
    }
}

impl From<&GStr> for GString {
    #[inline]
    fn from(s: &GStr) -> GString {
        s.to_owned()
    }
}

impl From<&str> for GString {
    #[inline]
    fn from(s: &str) -> Self {
        if cfg!(debug_assertions) {
            GStr::check_interior_nuls(s).unwrap();
        }
        if s.is_empty() {
            return Self::default();
        }
        // Allocates with the GLib allocator
        unsafe {
            // No check for valid UTF-8 here
            let copy = ffi::g_strndup(s.as_ptr() as *const c_char, s.len());
            GString(Inner::Foreign {
                ptr: ptr::NonNull::new_unchecked(copy),
                len: s.len(),
            })
        }
    }
}

impl From<CString> for GString {
    #[inline]
    fn from(s: CString) -> Self {
        // Moves the content of the CString
        // Also check if it's valid UTF-8
        let s = String::from_utf8(s.into_bytes_with_nul()).unwrap();
        Self(Inner::Native(Some(s.into_boxed_str())))
    }
}

impl From<&CStr> for GString {
    #[inline]
    fn from(c: &CStr) -> Self {
        // Creates a copy with the GLib allocator
        // Also check if it's valid UTF-8
        c.to_str().unwrap().into()
    }
}

#[doc(hidden)]
impl FromGlibPtrFull<*mut u8> for GString {
    #[inline]
    unsafe fn from_glib_full(ptr: *mut u8) -> Self {
        assert!(!ptr.is_null(), "provided C string is NULL");

        let cstr = CStr::from_ptr(ptr as *const _);
        // Check for valid UTF-8 here
        assert!(
            cstr.to_str().is_ok(),
            "provided C string is not valid utf-8"
        );
        Self(Inner::Foreign {
            ptr: ptr::NonNull::new_unchecked(ptr as *mut _),
            len: cstr.to_bytes().len(),
        })
    }
}

#[doc(hidden)]
impl FromGlibPtrFull<*mut i8> for GString {
    #[inline]
    unsafe fn from_glib_full(ptr: *mut i8) -> Self {
        from_glib_full(ptr as *mut u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrFull<*const u8> for GString {
    #[inline]
    unsafe fn from_glib_full(ptr: *const u8) -> Self {
        from_glib_full(ptr as *mut u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrFull<*const i8> for GString {
    #[inline]
    unsafe fn from_glib_full(ptr: *const i8) -> Self {
        from_glib_full(ptr as *mut u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*const u8> for GString {
    #[inline]
    unsafe fn from_glib_none(ptr: *const u8) -> Self {
        assert!(!ptr.is_null(), "provided C string is NULL");
        let cstr = CStr::from_ptr(ptr as *const _);
        // Also check if it's valid UTF-8
        cstr.to_str().unwrap().into()
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*const i8> for GString {
    #[inline]
    unsafe fn from_glib_none(ptr: *const i8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*mut u8> for GString {
    #[inline]
    unsafe fn from_glib_none(ptr: *mut u8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrNone<*mut i8> for GString {
    #[inline]
    unsafe fn from_glib_none(ptr: *mut i8) -> Self {
        from_glib_none(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrBorrow<*const u8> for GString {
    #[inline]
    unsafe fn from_glib_borrow(ptr: *const u8) -> Borrowed<Self> {
        assert!(!ptr.is_null());

        // Check for valid UTF-8 here
        let cstr = CStr::from_ptr(ptr as *const _);
        assert!(cstr.to_str().is_ok());
        Borrowed::new(Self(Inner::Foreign {
            ptr: ptr::NonNull::new_unchecked(ptr as *mut _),
            len: cstr.to_bytes().len(),
        }))
    }
}

#[doc(hidden)]
impl FromGlibPtrBorrow<*const i8> for GString {
    #[inline]
    unsafe fn from_glib_borrow(ptr: *const i8) -> Borrowed<Self> {
        from_glib_borrow(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrBorrow<*mut u8> for GString {
    #[inline]
    unsafe fn from_glib_borrow(ptr: *mut u8) -> Borrowed<Self> {
        from_glib_borrow(ptr as *const u8)
    }
}

#[doc(hidden)]
impl FromGlibPtrBorrow<*mut i8> for GString {
    #[inline]
    unsafe fn from_glib_borrow(ptr: *mut i8) -> Borrowed<Self> {
        from_glib_borrow(ptr as *const u8)
    }
}

impl<'a> ToGlibPtr<'a, *const c_char> for GString {
    type Storage = &'a Self;

    #[inline]
    fn to_glib_none(&'a self) -> Stash<'a, *const c_char, Self> {
        Stash(self.as_ptr() as *const c_char, self)
    }

    #[inline]
    fn to_glib_full(&self) -> *const c_char {
        unsafe { self.clone().into_glib_ptr() as *const _ }
    }
}

#[doc(hidden)]
impl<'a> ToGlibPtr<'a, *mut c_char> for GString {
    type Storage = &'a Self;

    #[inline]
    fn to_glib_none(&'a self) -> Stash<'a, *mut c_char, Self> {
        Stash(self.as_ptr() as *mut c_char, self)
    }

    #[inline]
    fn to_glib_full(&self) -> *mut c_char {
        unsafe { self.clone().into_glib_ptr() }
    }
}

#[doc(hidden)]
impl FromGlibContainer<*const c_char, *const i8> for GString {
    unsafe fn from_glib_none_num(ptr: *const i8, num: usize) -> Self {
        if num == 0 || ptr.is_null() {
            return Self::default();
        }
        let slice = slice::from_raw_parts(ptr as *const u8, num);
        // Also check if it's valid UTF-8
        std::str::from_utf8(slice).unwrap().into()
    }

    unsafe fn from_glib_container_num(ptr: *const i8, num: usize) -> Self {
        if num == 0 || ptr.is_null() {
            return Self::default();
        }

        // Check if it's valid UTF-8
        let slice = slice::from_raw_parts(ptr as *const u8, num);
        std::str::from_utf8(slice).unwrap();

        GString(Inner::Foreign {
            ptr: ptr::NonNull::new_unchecked(ptr as *mut _),
            len: num,
        })
    }

    unsafe fn from_glib_full_num(ptr: *const i8, num: usize) -> Self {
        if num == 0 || ptr.is_null() {
            return Self::default();
        }

        // Check if it's valid UTF-8
        let slice = slice::from_raw_parts(ptr as *const u8, num);
        std::str::from_utf8(slice).unwrap();

        GString(Inner::Foreign {
            ptr: ptr::NonNull::new_unchecked(ptr as *mut _),
            len: num,
        })
    }
}

#[doc(hidden)]
impl FromGlibContainer<*const c_char, *mut i8> for GString {
    unsafe fn from_glib_none_num(ptr: *mut i8, num: usize) -> Self {
        FromGlibContainer::from_glib_none_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_container_num(ptr: *mut i8, num: usize) -> Self {
        FromGlibContainer::from_glib_container_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_full_num(ptr: *mut i8, num: usize) -> Self {
        FromGlibContainer::from_glib_full_num(ptr as *const i8, num)
    }
}

#[doc(hidden)]
impl FromGlibContainer<*const c_char, *const u8> for GString {
    unsafe fn from_glib_none_num(ptr: *const u8, num: usize) -> Self {
        FromGlibContainer::from_glib_none_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_container_num(ptr: *const u8, num: usize) -> Self {
        FromGlibContainer::from_glib_container_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_full_num(ptr: *const u8, num: usize) -> Self {
        FromGlibContainer::from_glib_full_num(ptr as *const i8, num)
    }
}

#[doc(hidden)]
impl FromGlibContainer<*const c_char, *mut u8> for GString {
    unsafe fn from_glib_none_num(ptr: *mut u8, num: usize) -> Self {
        FromGlibContainer::from_glib_none_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_container_num(ptr: *mut u8, num: usize) -> Self {
        FromGlibContainer::from_glib_container_num(ptr as *const i8, num)
    }

    unsafe fn from_glib_full_num(ptr: *mut u8, num: usize) -> Self {
        FromGlibContainer::from_glib_full_num(ptr as *const i8, num)
    }
}

impl GlibPtrDefault for GString {
    type GlibType = *const c_char;
}

impl StaticType for GString {
    #[inline]
    fn static_type() -> Type {
        String::static_type()
    }
}

impl crate::value::ValueType for GString {
    type Type = String;
}

impl crate::value::ValueTypeOptional for GString {}

unsafe impl<'a> crate::value::FromValue<'a> for GString {
    type Checker = crate::value::GenericValueTypeOrNoneChecker<Self>;

    #[inline]
    unsafe fn from_value(value: &'a crate::Value) -> Self {
        Self::from(<&str>::from_value(value))
    }
}

impl crate::value::ToValue for GString {
    #[inline]
    fn to_value(&self) -> crate::Value {
        <&str>::to_value(&self.as_str())
    }

    #[inline]
    fn value_type(&self) -> Type {
        String::static_type()
    }
}

impl crate::value::ToValueOptional for GString {
    #[inline]
    fn to_value_optional(s: Option<&Self>) -> crate::Value {
        <str>::to_value_optional(s.as_ref().map(|s| s.as_str()))
    }
}

impl StaticType for Vec<GString> {
    #[inline]
    fn static_type() -> Type {
        <Vec<String>>::static_type()
    }
}

impl crate::value::ValueType for Vec<GString> {
    type Type = Vec<GString>;
}

unsafe impl<'a> crate::value::FromValue<'a> for Vec<GString> {
    type Checker = crate::value::GenericValueTypeChecker<Self>;

    unsafe fn from_value(value: &'a crate::value::Value) -> Self {
        let ptr = gobject_ffi::g_value_get_boxed(value.to_glib_none().0) as *const *const c_char;
        FromGlibPtrContainer::from_glib_none(ptr)
    }
}

impl crate::value::ToValue for Vec<GString> {
    fn to_value(&self) -> crate::value::Value {
        unsafe {
            let mut value = crate::value::Value::for_value_type::<Self>();
            let ptr: *mut *mut c_char = self.to_glib_full();
            gobject_ffi::g_value_take_boxed(value.to_glib_none_mut().0, ptr as *const c_void);
            value
        }
    }

    #[inline]
    fn value_type(&self) -> Type {
        <Vec<GString>>::static_type()
    }
}

impl_from_glib_container_as_vec_string!(GString, *const c_char);
impl_from_glib_container_as_vec_string!(GString, *mut c_char);

#[cfg(test)]
#[allow(clippy::disallowed_names)]
mod tests {
    use super::*;
    use std::ffi::CString;

    #[test]
    fn test_gstring() {
        let data = CString::new("foo").unwrap();
        let ptr = data.as_ptr();

        unsafe {
            let ptr_copy = ffi::g_strdup(ptr);
            let gstring = GString::from_glib_full(ptr_copy);
            assert_eq!(gstring.as_str(), "foo");
            let foo: Box<str> = gstring.into();
            assert_eq!(foo.as_ref(), "foo");
        }
    }

    #[test]
    fn test_owned_glib_string() {
        let data = CString::new("foo").unwrap();
        let ptr = data.as_ptr();
        unsafe {
            let ptr_copy = ffi::g_strdup(ptr);
            let gstr = GString::from_glib_full(ptr_copy);
            assert_eq!(gstr, "foo");
        }
    }

    #[test]
    fn test_gstring_from_str() {
        let gstring: GString = "foo".into();
        assert_eq!(gstring.as_str(), "foo");
        let foo: Box<str> = gstring.into();
        assert_eq!(foo.as_ref(), "foo");
    }

    #[test]
    fn test_string_from_gstring() {
        let gstring = GString::from("foo");
        assert_eq!(gstring.as_str(), "foo");
        let s = String::from(gstring);
        assert_eq!(s, "foo");
    }

    #[test]
    fn test_gstring_from_cstring() {
        let cstr = CString::new("foo").unwrap();
        let gstring = GString::from(cstr);
        assert_eq!(gstring.as_str(), "foo");
        let foo: Box<str> = gstring.into();
        assert_eq!(foo.as_ref(), "foo");
    }

    #[test]
    fn test_string_from_gstring_from_cstring() {
        let cstr = CString::new("foo").unwrap();
        let gstring = GString::from(cstr);
        assert_eq!(gstring.as_str(), "foo");
        let s = String::from(gstring);
        assert_eq!(s, "foo");
    }

    #[test]
    fn test_vec_u8_to_gstring() {
        let v: &[u8] = b"foo";
        let s: GString = GString::from_utf8(Vec::from(v)).unwrap();
        assert_eq!(s.as_str(), "foo");
    }

    #[test]
    fn test_as_ref_path() {
        fn foo<P: AsRef<Path>>(_path: P) {}
        let gstring: GString = "/my/path/".into();
        let gstr: &GStr = gstring.as_gstr();
        foo(gstr);
        foo(gstring);
    }

    #[test]
    fn test_from_glib_container() {
        unsafe {
            let test_a: GString = FromGlibContainer::from_glib_container_num(
                ffi::g_strdup("hello_world\0".as_ptr() as *const _),
                5,
            );
            assert_eq!("hello", test_a.as_str());

            let test_b: GString = FromGlibContainer::from_glib_none_num("hello_world".as_ptr(), 5);
            assert_eq!("hello", test_b.as_str());

            let test_c: GString =
                FromGlibContainer::from_glib_none_num(std::ptr::null::<std::os::raw::c_char>(), 0);
            assert_eq!("", test_c.as_str());

            let test_d: GString = FromGlibContainer::from_glib_none_num("".as_ptr(), 0);
            assert_eq!("", test_d.as_str());

            let test_e: GString =
                FromGlibContainer::from_glib_container_num(ffi::g_strdup(std::ptr::null()), 0);
            assert_eq!("", test_e.as_str());
        }
    }

    #[test]
    fn test_hashmap() {
        use std::collections::HashMap;

        let gstring = GString::from("foo");
        assert_eq!(gstring.as_str(), "foo");
        let mut h: HashMap<GString, i32> = HashMap::new();
        h.insert(gstring, 42);
        let gstring: GString = "foo".into();
        assert!(h.contains_key(&gstring));
    }
}
