// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::{
    DBusConnection, DBusInterface, DBusInterfaceInfo, DBusInterfaceSkeletonFlags,
    DBusMethodInvocation,
};
use glib::{
    prelude::*,
    signal::{connect_raw, SignalHandlerId},
    translate::*,
};
use std::{boxed::Box as Box_, fmt, mem::transmute, ptr};

glib::wrapper! {
    #[doc(alias = "GDBusInterfaceSkeleton")]
    pub struct DBusInterfaceSkeleton(Object<ffi::GDBusInterfaceSkeleton, ffi::GDBusInterfaceSkeletonClass>) @implements DBusInterface;

    match fn {
        type_ => || ffi::g_dbus_interface_skeleton_get_type(),
    }
}

impl DBusInterfaceSkeleton {
    pub const NONE: Option<&'static DBusInterfaceSkeleton> = None;
}

pub trait DBusInterfaceSkeletonExt: 'static {
    #[doc(alias = "g_dbus_interface_skeleton_export")]
    fn export(&self, connection: &DBusConnection, object_path: &str) -> Result<(), glib::Error>;

    #[doc(alias = "g_dbus_interface_skeleton_flush")]
    fn flush(&self);

    #[doc(alias = "g_dbus_interface_skeleton_get_connection")]
    #[doc(alias = "get_connection")]
    fn connection(&self) -> Option<DBusConnection>;

    #[doc(alias = "g_dbus_interface_skeleton_get_connections")]
    #[doc(alias = "get_connections")]
    fn connections(&self) -> glib::List<DBusConnection>;

    #[doc(alias = "g_dbus_interface_skeleton_get_flags")]
    #[doc(alias = "get_flags")]
    fn flags(&self) -> DBusInterfaceSkeletonFlags;

    #[doc(alias = "g_dbus_interface_skeleton_get_info")]
    #[doc(alias = "get_info")]
    fn info(&self) -> DBusInterfaceInfo;

    #[doc(alias = "g_dbus_interface_skeleton_get_object_path")]
    #[doc(alias = "get_object_path")]
    fn object_path(&self) -> Option<glib::GString>;

    #[doc(alias = "g_dbus_interface_skeleton_get_properties")]
    #[doc(alias = "get_properties")]
    fn properties(&self) -> glib::Variant;

    //#[doc(alias = "g_dbus_interface_skeleton_get_vtable")]
    //#[doc(alias = "get_vtable")]
    //fn vtable(&self) -> /*Ignored*/DBusInterfaceVTable;

    #[doc(alias = "g_dbus_interface_skeleton_has_connection")]
    fn has_connection(&self, connection: &DBusConnection) -> bool;

    #[doc(alias = "g_dbus_interface_skeleton_set_flags")]
    fn set_flags(&self, flags: DBusInterfaceSkeletonFlags);

    #[doc(alias = "g_dbus_interface_skeleton_unexport")]
    fn unexport(&self);

    #[doc(alias = "g_dbus_interface_skeleton_unexport_from_connection")]
    fn unexport_from_connection(&self, connection: &DBusConnection);

    #[doc(alias = "g-flags")]
    fn g_flags(&self) -> DBusInterfaceSkeletonFlags;

    #[doc(alias = "g-flags")]
    fn set_g_flags(&self, g_flags: DBusInterfaceSkeletonFlags);

    #[doc(alias = "g-authorize-method")]
    fn connect_g_authorize_method<F: Fn(&Self, &DBusMethodInvocation) -> bool + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId;

    #[doc(alias = "g-flags")]
    fn connect_g_flags_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId;
}

impl<O: IsA<DBusInterfaceSkeleton>> DBusInterfaceSkeletonExt for O {
    fn export(&self, connection: &DBusConnection, object_path: &str) -> Result<(), glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let is_ok = ffi::g_dbus_interface_skeleton_export(
                self.as_ref().to_glib_none().0,
                connection.to_glib_none().0,
                object_path.to_glib_none().0,
                &mut error,
            );
            debug_assert_eq!(is_ok == glib::ffi::GFALSE, !error.is_null());
            if error.is_null() {
                Ok(())
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    fn flush(&self) {
        unsafe {
            ffi::g_dbus_interface_skeleton_flush(self.as_ref().to_glib_none().0);
        }
    }

    fn connection(&self) -> Option<DBusConnection> {
        unsafe {
            from_glib_none(ffi::g_dbus_interface_skeleton_get_connection(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn connections(&self) -> glib::List<DBusConnection> {
        unsafe {
            FromGlibPtrContainer::from_glib_full(ffi::g_dbus_interface_skeleton_get_connections(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn flags(&self) -> DBusInterfaceSkeletonFlags {
        unsafe {
            from_glib(ffi::g_dbus_interface_skeleton_get_flags(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn info(&self) -> DBusInterfaceInfo {
        unsafe {
            from_glib_none(ffi::g_dbus_interface_skeleton_get_info(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn object_path(&self) -> Option<glib::GString> {
        unsafe {
            from_glib_none(ffi::g_dbus_interface_skeleton_get_object_path(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    fn properties(&self) -> glib::Variant {
        unsafe {
            from_glib_full(ffi::g_dbus_interface_skeleton_get_properties(
                self.as_ref().to_glib_none().0,
            ))
        }
    }

    //fn vtable(&self) -> /*Ignored*/DBusInterfaceVTable {
    //    unsafe { TODO: call ffi:g_dbus_interface_skeleton_get_vtable() }
    //}

    fn has_connection(&self, connection: &DBusConnection) -> bool {
        unsafe {
            from_glib(ffi::g_dbus_interface_skeleton_has_connection(
                self.as_ref().to_glib_none().0,
                connection.to_glib_none().0,
            ))
        }
    }

    fn set_flags(&self, flags: DBusInterfaceSkeletonFlags) {
        unsafe {
            ffi::g_dbus_interface_skeleton_set_flags(
                self.as_ref().to_glib_none().0,
                flags.into_glib(),
            );
        }
    }

    fn unexport(&self) {
        unsafe {
            ffi::g_dbus_interface_skeleton_unexport(self.as_ref().to_glib_none().0);
        }
    }

    fn unexport_from_connection(&self, connection: &DBusConnection) {
        unsafe {
            ffi::g_dbus_interface_skeleton_unexport_from_connection(
                self.as_ref().to_glib_none().0,
                connection.to_glib_none().0,
            );
        }
    }

    fn g_flags(&self) -> DBusInterfaceSkeletonFlags {
        glib::ObjectExt::property(self.as_ref(), "g-flags")
    }

    fn set_g_flags(&self, g_flags: DBusInterfaceSkeletonFlags) {
        glib::ObjectExt::set_property(self.as_ref(), "g-flags", &g_flags)
    }

    fn connect_g_authorize_method<F: Fn(&Self, &DBusMethodInvocation) -> bool + 'static>(
        &self,
        f: F,
    ) -> SignalHandlerId {
        unsafe extern "C" fn g_authorize_method_trampoline<
            P: IsA<DBusInterfaceSkeleton>,
            F: Fn(&P, &DBusMethodInvocation) -> bool + 'static,
        >(
            this: *mut ffi::GDBusInterfaceSkeleton,
            invocation: *mut ffi::GDBusMethodInvocation,
            f: glib::ffi::gpointer,
        ) -> glib::ffi::gboolean {
            let f: &F = &*(f as *const F);
            f(
                DBusInterfaceSkeleton::from_glib_borrow(this).unsafe_cast_ref(),
                &from_glib_borrow(invocation),
            )
            .into_glib()
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"g-authorize-method\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    g_authorize_method_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }

    fn connect_g_flags_notify<F: Fn(&Self) + 'static>(&self, f: F) -> SignalHandlerId {
        unsafe extern "C" fn notify_g_flags_trampoline<
            P: IsA<DBusInterfaceSkeleton>,
            F: Fn(&P) + 'static,
        >(
            this: *mut ffi::GDBusInterfaceSkeleton,
            _param_spec: glib::ffi::gpointer,
            f: glib::ffi::gpointer,
        ) {
            let f: &F = &*(f as *const F);
            f(DBusInterfaceSkeleton::from_glib_borrow(this).unsafe_cast_ref())
        }
        unsafe {
            let f: Box_<F> = Box_::new(f);
            connect_raw(
                self.as_ptr() as *mut _,
                b"notify::g-flags\0".as_ptr() as *const _,
                Some(transmute::<_, unsafe extern "C" fn()>(
                    notify_g_flags_trampoline::<Self, F> as *const (),
                )),
                Box_::into_raw(f),
            )
        }
    }
}

impl fmt::Display for DBusInterfaceSkeleton {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("DBusInterfaceSkeleton")
    }
}
