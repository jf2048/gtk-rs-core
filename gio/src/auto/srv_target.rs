// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use glib::translate::*;

glib::wrapper! {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct SrvTarget(Boxed<ffi::GSrvTarget>);

    match fn {
        copy => |ptr| ffi::g_srv_target_copy(mut_override(ptr)),
        free => |ptr| ffi::g_srv_target_free(ptr),
        type_ => || ffi::g_srv_target_get_type(),
    }
}

impl SrvTarget {
    #[doc(alias = "g_srv_target_new")]
    pub fn new(hostname: &str, port: u16, priority: u16, weight: u16) -> SrvTarget {
        unsafe {
            from_glib_full(ffi::g_srv_target_new(
                hostname.to_glib_none().0,
                port,
                priority,
                weight,
            ))
        }
    }

    #[doc(alias = "g_srv_target_get_hostname")]
    #[doc(alias = "get_hostname")]
    pub fn hostname(&mut self) -> glib::GString {
        unsafe { from_glib_none(ffi::g_srv_target_get_hostname(self.to_glib_none_mut().0)) }
    }

    #[doc(alias = "g_srv_target_get_port")]
    #[doc(alias = "get_port")]
    pub fn port(&mut self) -> u16 {
        unsafe { ffi::g_srv_target_get_port(self.to_glib_none_mut().0) }
    }

    #[doc(alias = "g_srv_target_get_priority")]
    #[doc(alias = "get_priority")]
    pub fn priority(&mut self) -> u16 {
        unsafe { ffi::g_srv_target_get_priority(self.to_glib_none_mut().0) }
    }

    #[doc(alias = "g_srv_target_get_weight")]
    #[doc(alias = "get_weight")]
    pub fn weight(&mut self) -> u16 {
        unsafe { ffi::g_srv_target_get_weight(self.to_glib_none_mut().0) }
    }

    //#[doc(alias = "g_srv_target_list_sort")]
    //pub fn list_sort(targets: /*Unimplemented*/&[&Basic: Pointer]) -> /*Unimplemented*/glib::List<Basic: Pointer> {
    //    unsafe { TODO: call ffi:g_srv_target_list_sort() }
    //}
}
