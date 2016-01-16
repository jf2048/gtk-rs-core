// This file was generated by gir (463de47) from gir-files (11e0e6d)
// DO NOT EDIT

use Adjustment;
use Bin;
use Buildable;
use Container;
use CornerType;
use PolicyType;
use ShadowType;
use Widget;
use ffi;
use glib::object::Downcast;
use glib::object::IsA;
use glib::translate::*;
use std::mem;

glib_wrapper! {
    pub struct ScrolledWindow(Object<ffi::GtkScrolledWindow>): Widget, Container, Bin, Buildable;

    match fn {
        get_type => || ffi::gtk_scrolled_window_get_type(),
    }
}

impl ScrolledWindow {
    pub fn new(hadjustment: Option<&Adjustment>, vadjustment: Option<&Adjustment>) -> ScrolledWindow {
        assert_initialized_main_thread!();
        unsafe {
            Widget::from_glib_none(ffi::gtk_scrolled_window_new(hadjustment.to_glib_none().0, vadjustment.to_glib_none().0)).downcast_unchecked()
        }
    }
}

pub trait ScrolledWindowExt {
    fn add_with_viewport<T: IsA<Widget>>(&self, child: &T);

    #[cfg(gtk_3_4)]
    fn get_capture_button_press(&self) -> bool;

    fn get_hadjustment(&self) -> Option<Adjustment>;

    fn get_hscrollbar(&self) -> Option<Widget>;

    #[cfg(gtk_3_4)]
    fn get_kinetic_scrolling(&self) -> bool;

    fn get_min_content_height(&self) -> i32;

    fn get_min_content_width(&self) -> i32;

    #[cfg(gtk_3_16)]
    fn get_overlay_scrolling(&self) -> bool;

    fn get_placement(&self) -> CornerType;

    fn get_policy(&self) -> (PolicyType, PolicyType);

    fn get_shadow_type(&self) -> ShadowType;

    fn get_vadjustment(&self) -> Option<Adjustment>;

    fn get_vscrollbar(&self) -> Option<Widget>;

    #[cfg(gtk_3_4)]
    fn set_capture_button_press(&self, capture_button_press: bool);

    fn set_hadjustment(&self, hadjustment: &Adjustment);

    #[cfg(gtk_3_4)]
    fn set_kinetic_scrolling(&self, kinetic_scrolling: bool);

    fn set_min_content_height(&self, height: i32);

    fn set_min_content_width(&self, width: i32);

    #[cfg(gtk_3_16)]
    fn set_overlay_scrolling(&self, overlay_scrolling: bool);

    fn set_placement(&self, window_placement: CornerType);

    fn set_policy(&self, hscrollbar_policy: PolicyType, vscrollbar_policy: PolicyType);

    fn set_shadow_type(&self, type_: ShadowType);

    fn set_vadjustment(&self, vadjustment: &Adjustment);

    fn unset_placement(&self);
}

impl<O: IsA<ScrolledWindow>> ScrolledWindowExt for O {
    fn add_with_viewport<T: IsA<Widget>>(&self, child: &T) {
        unsafe {
            ffi::gtk_scrolled_window_add_with_viewport(self.to_glib_none().0, child.to_glib_none().0);
        }
    }

    #[cfg(gtk_3_4)]
    fn get_capture_button_press(&self) -> bool {
        unsafe {
            from_glib(ffi::gtk_scrolled_window_get_capture_button_press(self.to_glib_none().0))
        }
    }

    fn get_hadjustment(&self) -> Option<Adjustment> {
        unsafe {
            from_glib_none(ffi::gtk_scrolled_window_get_hadjustment(self.to_glib_none().0))
        }
    }

    fn get_hscrollbar(&self) -> Option<Widget> {
        unsafe {
            from_glib_none(ffi::gtk_scrolled_window_get_hscrollbar(self.to_glib_none().0))
        }
    }

    #[cfg(gtk_3_4)]
    fn get_kinetic_scrolling(&self) -> bool {
        unsafe {
            from_glib(ffi::gtk_scrolled_window_get_kinetic_scrolling(self.to_glib_none().0))
        }
    }

    fn get_min_content_height(&self) -> i32 {
        unsafe {
            ffi::gtk_scrolled_window_get_min_content_height(self.to_glib_none().0)
        }
    }

    fn get_min_content_width(&self) -> i32 {
        unsafe {
            ffi::gtk_scrolled_window_get_min_content_width(self.to_glib_none().0)
        }
    }

    #[cfg(gtk_3_16)]
    fn get_overlay_scrolling(&self) -> bool {
        unsafe {
            from_glib(ffi::gtk_scrolled_window_get_overlay_scrolling(self.to_glib_none().0))
        }
    }

    fn get_placement(&self) -> CornerType {
        unsafe {
            ffi::gtk_scrolled_window_get_placement(self.to_glib_none().0)
        }
    }

    fn get_policy(&self) -> (PolicyType, PolicyType) {
        unsafe {
            let mut hscrollbar_policy = mem::uninitialized();
            let mut vscrollbar_policy = mem::uninitialized();
            ffi::gtk_scrolled_window_get_policy(self.to_glib_none().0, &mut hscrollbar_policy, &mut vscrollbar_policy);
            (hscrollbar_policy, vscrollbar_policy)
        }
    }

    fn get_shadow_type(&self) -> ShadowType {
        unsafe {
            ffi::gtk_scrolled_window_get_shadow_type(self.to_glib_none().0)
        }
    }

    fn get_vadjustment(&self) -> Option<Adjustment> {
        unsafe {
            from_glib_none(ffi::gtk_scrolled_window_get_vadjustment(self.to_glib_none().0))
        }
    }

    fn get_vscrollbar(&self) -> Option<Widget> {
        unsafe {
            from_glib_none(ffi::gtk_scrolled_window_get_vscrollbar(self.to_glib_none().0))
        }
    }

    #[cfg(gtk_3_4)]
    fn set_capture_button_press(&self, capture_button_press: bool) {
        unsafe {
            ffi::gtk_scrolled_window_set_capture_button_press(self.to_glib_none().0, capture_button_press.to_glib());
        }
    }

    fn set_hadjustment(&self, hadjustment: &Adjustment) {
        unsafe {
            ffi::gtk_scrolled_window_set_hadjustment(self.to_glib_none().0, hadjustment.to_glib_none().0);
        }
    }

    #[cfg(gtk_3_4)]
    fn set_kinetic_scrolling(&self, kinetic_scrolling: bool) {
        unsafe {
            ffi::gtk_scrolled_window_set_kinetic_scrolling(self.to_glib_none().0, kinetic_scrolling.to_glib());
        }
    }

    fn set_min_content_height(&self, height: i32) {
        unsafe {
            ffi::gtk_scrolled_window_set_min_content_height(self.to_glib_none().0, height);
        }
    }

    fn set_min_content_width(&self, width: i32) {
        unsafe {
            ffi::gtk_scrolled_window_set_min_content_width(self.to_glib_none().0, width);
        }
    }

    #[cfg(gtk_3_16)]
    fn set_overlay_scrolling(&self, overlay_scrolling: bool) {
        unsafe {
            ffi::gtk_scrolled_window_set_overlay_scrolling(self.to_glib_none().0, overlay_scrolling.to_glib());
        }
    }

    fn set_placement(&self, window_placement: CornerType) {
        unsafe {
            ffi::gtk_scrolled_window_set_placement(self.to_glib_none().0, window_placement);
        }
    }

    fn set_policy(&self, hscrollbar_policy: PolicyType, vscrollbar_policy: PolicyType) {
        unsafe {
            ffi::gtk_scrolled_window_set_policy(self.to_glib_none().0, hscrollbar_policy, vscrollbar_policy);
        }
    }

    fn set_shadow_type(&self, type_: ShadowType) {
        unsafe {
            ffi::gtk_scrolled_window_set_shadow_type(self.to_glib_none().0, type_);
        }
    }

    fn set_vadjustment(&self, vadjustment: &Adjustment) {
        unsafe {
            ffi::gtk_scrolled_window_set_vadjustment(self.to_glib_none().0, vadjustment.to_glib_none().0);
        }
    }

    fn unset_placement(&self) {
        unsafe {
            ffi::gtk_scrolled_window_unset_placement(self.to_glib_none().0);
        }
    }
}
