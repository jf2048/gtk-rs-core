// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::File;
use crate::InputStream;
use glib::object::Cast;
use glib::object::IsA;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use std::boxed::Box as Box_;
use std::fmt;
use std::mem;
use std::mem::transmute;

glib::wrapper! {
    pub struct ApplicationCommandLine(Object<ffi::GApplicationCommandLine, ffi::GApplicationCommandLineClass>);

    match fn {
        type_ => || ffi::g_application_command_line_get_type(),
    }
}

pub const NONE_APPLICATION_COMMAND_LINE: Option<&ApplicationCommandLine> = None;

pub trait ApplicationCommandLineExt: 'static {
    #[doc(alias = "g_application_command_line_create_file_for_arg")]
    fn create_file_for_arg<P: AsRef<std::ffi::OsStr>>(&self, arg: P) -> File;

    #[doc(alias = "g_application_command_line_get_arguments")]
    fn arguments(&self) -> Vec<std::ffi::OsString>;

    #[doc(alias = "g_application_command_line_get_cwd")]
    fn cwd(&self) -> Option<std::path::PathBuf>;

    #[doc(alias = "g_application_command_line_get_environ")]
    fn environ(&self) -> Vec<std::ffi::OsString>;

    #[doc(alias = "g_application_command_line_get_exit_status")]
    fn exit_status(&self) -> i32;

    #[doc(alias = "g_application_command_line_get_is_remote")]
    fn is_remote(&self) -> bool;

    #[doc(alias = "g_application_command_line_get_options_dict")]
    fn options_dict(&self) -> glib::VariantDict;

    #[doc(alias = "g_application_command_line_get_platform_data")]
    fn platform_data(&self) -> Option<glib::Variant>;

    #[doc(alias = "g_application_command_line_get_stdin")]
    fn stdin(&self) -> Option<InputStream>;

    #[doc(alias = "g_application_command_line_getenv")]
    fn getenv<P: AsRef<std::ffi::OsStr>>(&self, name: P) -> Option<glib::GString>;

    //#[doc(alias = "g_application_command_line_print")]
    //fn print(&self, format: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs);

    //#[doc(alias = "g_application_command_line_printerr")]
    //fn printerr(&self, format: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs);

    #[doc(alias = "g_application_command_line_set_exit_status")]
    fn set_exit_status(&self, exit_status: i32);

    fn connect_property_is_remote_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<ApplicationCommandLine>> ApplicationCommandLineExt for O {
    fn create_file_for_arg<P: AsRef<std::ffi::OsStr>>(&self, arg: P) -> File {
        unsafe {
            from_glib_full(ffi::g_application_command_line_create_file_for_arg(
                self.as_ref().to_glib_none().0,
                arg.as_ref().to_glib_none().0,
            ))
        }
    }

    fn arguments(&self) -> Vec<std::ffi::OsString> {
        unsafe {
            let mut argc = mem::MaybeUninit::uninit();
            let ret = FromGlibContainer::from_glib_full_num(
                ffi::g_application_command_line_get_arguments(
                    self.as_ref().to_glib_none().0,
                    argc.as_mut_ptr(),
                ),
                argc.assume_init() as usize,
            );
            ret
        }
    }

    fn cwd(&self) -> Option<std::path::PathBuf> {
        unsafe {
            from_glib_none(ffi::g_application_command_line_get_cwd(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn environ(&self) -> Vec<std::ffi::OsString> {
        unsafe {
            FromGlibPtrContainer::from_glib_none(ffi::g_application_command_line_get_environ(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn exit_status(&self) -> i32 {
        unsafe { ffi::g_application_command_line_get_exit_status(self.as_ref().to_glib_none().0) }
    }

    fn is_remote(&self) -> bool {
        unsafe {
            from_glib(ffi::g_application_command_line_get_is_remote(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn options_dict(&self) -> glib::VariantDict {
        unsafe {
            from_glib_none(ffi::g_application_command_line_get_options_dict(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn platform_data(&self) -> Option<glib::Variant> {
        unsafe {
            from_glib_full(ffi::g_application_command_line_get_platform_data(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn stdin(&self) -> Option<InputStream> {
        unsafe {
            from_glib_full(ffi::g_application_command_line_get_stdin(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn getenv<P: AsRef<std::ffi::OsStr>>(&self, name: P) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_application_command_line_getenv(
                self.as_ref().to_glib_none().0,
                name.as_ref().to_glib_none().0,
            ))
        }
    }

    //fn print(&self, format: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:g_application_command_line_print() }
    //}

    //fn printerr(&self, format: &str, : /*Unknown conversion*//*Unimplemented*/Fundamental: VarArgs) {
    //    unsafe { TODO: call ffi:g_application_command_line_printerr() }
    //}

    fn set_exit_status(&self, exit_status: i32) {
        unsafe {
            ffi::g_application_command_line_set_exit_status(
                self.as_ref().to_glib_none().0,
                exit_status,
            );
        }
    }

    fn connect_property_is_remote_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_is_remote_trampoline<P, F: Fn(&P) + 'static>(
            this: *mut ffi::GApplicationCommandLine,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) where
            P: IsA<ApplicationCommandLine>,
        {
            let f: &F = &*(f as *const F);
            f(&ApplicationCommandLine::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::is-remote\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_is_remote_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for ApplicationCommandLine {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("ApplicationCommandLine")
    }
}
