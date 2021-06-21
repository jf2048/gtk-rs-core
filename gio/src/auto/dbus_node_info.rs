// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::DBusInterfaceInfo;
use glib::translate::*;
use std::ptr;

glib::wrapper! {
    #[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
    pub struct DBusNodeInfo(Shared<ffi::GDBusNodeInfo>);

    match fn {
        ref => |ptr| ffi::g_dbus_node_info_ref(ptr),
        unref => |ptr| ffi::g_dbus_node_info_unref(ptr),
        type_ => || ffi::g_dbus_node_info_get_type(),
    }
}

impl DBusNodeInfo {
    #[doc(alias = "g_dbus_node_info_new_for_xml")]
    #[doc(alias = "new_for_xml")]
    pub fn for_xml(xml_data: &str) -> Result<DBusNodeInfo, glib::Error> {
        unsafe {
            let mut error = ptr::null_mut();
            let ret = ffi::g_dbus_node_info_new_for_xml(xml_data.to_glib_none().0, &mut error);
            if error.is_null() {
                Ok(from_glib_full(ret))
            } else {
                Err(from_glib_full(error))
            }
        }
    }

    #[doc(alias = "g_dbus_node_info_generate_xml")]
    pub fn generate_xml(&self, indent: u32, string_builder: &mut glib::String) {
        unsafe {
            ffi::g_dbus_node_info_generate_xml(
                self.to_glib_none().0,
                indent,
                string_builder.to_glib_none_mut().0,
            );
        }
    }

    #[doc(alias = "g_dbus_node_info_lookup_interface")]
    pub fn lookup_interface(&self, name: &str) -> Option<DBusInterfaceInfo> {
        unsafe {
            from_glib_none(ffi::g_dbus_node_info_lookup_interface(
                self.to_glib_none().0,
                name.to_glib_none().0,
            ))
        }
    }
}