// Take a look at the license at the top of the repository in the LICENSE file.

use glib::object::Cast;
use glib::translate::*;

use glib::subclass::prelude::*;

use crate::AsyncResult;

pub trait AsyncResultImpl: ObjectImpl {
    fn user_data(&self, res: &Self::Type) -> glib::Pointer;
    fn source_object(&self, res: &Self::Type) -> Option<glib::Object>;
    fn is_tagged(&self, res: &Self::Type, source_tag: glib::Pointer) -> bool;
}

pub trait AsyncResultImplExt: ObjectSubclass {
    fn parent_user_data(&self, res: &Self::Type) -> glib::Pointer;
    fn parent_source_object(&self, res: &Self::Type) -> Option<glib::Object>;
    fn parent_is_tagged(&self, res: &Self::Type, source_tag: glib::Pointer) -> bool;
}

impl<T: AsyncResultImpl> AsyncResultImplExt for T {
    fn parent_user_data(&self, res: &Self::Type) -> glib::Pointer {
        unsafe {
            let type_data = Self::type_data();
            let parent_iface = type_data.as_ref().parent_interface::<AsyncResult>()
                as *const ffi::GAsyncResultIface;

            let func = (*parent_iface)
                .get_user_data
                .expect("no parent \"get_user_data\" implementation");

            func(res.unsafe_cast_ref::<AsyncResult>().to_glib_none().0)
        }
    }
    fn parent_source_object(&self, res: &Self::Type) -> Option<glib::Object> {
        unsafe {
            let type_data = Self::type_data();
            let parent_iface = type_data.as_ref().parent_interface::<AsyncResult>()
                as *const ffi::GAsyncResultIface;

            let func = (*parent_iface)
                .get_source_object
                .expect("no parent \"get_source_object\" implementation");

            from_glib_full(func(res.unsafe_cast_ref::<AsyncResult>().to_glib_none().0))
        }
    }
    #[allow(clippy::not_unsafe_ptr_arg_deref)]
    fn parent_is_tagged(&self, res: &Self::Type, source_tag: glib::Pointer) -> bool {
        unsafe {
            let type_data = Self::type_data();
            let parent_iface = type_data.as_ref().parent_interface::<AsyncResult>()
                as *const ffi::GAsyncResultIface;

            let func = (*parent_iface)
                .is_tagged
                .expect("no parent \"is_tagged\" implementation");

            from_glib(func(
                res.unsafe_cast_ref::<AsyncResult>().to_glib_none().0,
                source_tag,
            ))
        }
    }
}

unsafe impl<T: AsyncResultImpl> IsImplementable<T> for AsyncResult {
    fn interface_init(iface: &mut glib::Interface<Self>) {
        let iface = iface.as_mut();
        iface.get_user_data = Some(async_result_get_user_data::<T>);
        iface.get_source_object = Some(async_result_get_source_object::<T>);
        iface.is_tagged = Some(async_result_is_tagged::<T>);
    }
}

unsafe extern "C" fn async_result_get_user_data<T: AsyncResultImpl>(
    res: *mut ffi::GAsyncResult,
) -> glib::ffi::gpointer {
    let instance = &*(res as *mut T::Instance);
    let imp = instance.imp();

    imp.user_data(from_glib_borrow::<_, AsyncResult>(res).unsafe_cast_ref())
}

unsafe extern "C" fn async_result_get_source_object<T: AsyncResultImpl>(
    res: *mut ffi::GAsyncResult,
) -> *mut glib::gobject_ffi::GObject {
    let instance = &*(res as *mut T::Instance);
    let imp = instance.imp();

    imp.source_object(from_glib_borrow::<_, AsyncResult>(res).unsafe_cast_ref())
        .to_glib_full()
}

unsafe extern "C" fn async_result_is_tagged<T: AsyncResultImpl>(
    res: *mut ffi::GAsyncResult,
    source_tag: glib::ffi::gpointer,
) -> glib::ffi::gboolean {
    let instance = &*(res as *mut T::Instance);
    let imp = instance.imp();

    imp.is_tagged(
        from_glib_borrow::<_, AsyncResult>(res).unsafe_cast_ref(),
        source_tag,
    )
    .into_glib()
}
