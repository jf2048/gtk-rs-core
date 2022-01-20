// Take a look at the license at the top of the repository in the LICENSE file.

use crate::Cancellable;
use crate::prelude::*;
use futures_core::Future;
use glib::{IsA, Source, translate::*};
use std::num::NonZeroU64;

#[derive(Debug, Eq, PartialEq)]
pub struct CancelledHandlerId(NonZeroU64);

impl CancelledHandlerId {
    // rustdoc-stripper-ignore-next
    /// Returns the internal signal handler ID.
    pub unsafe fn as_raw(&self) -> libc::c_ulong {
        self.0.get() as libc::c_ulong
    }
}

impl FromGlib<libc::c_ulong> for CancelledHandlerId {
    #[inline]
    unsafe fn from_glib(val: libc::c_ulong) -> Self {
        assert_ne!(val, 0);
        Self(NonZeroU64::new_unchecked(val as u64))
    }
}

unsafe impl Send for CancelledHandlerId {}
unsafe impl Sync for CancelledHandlerId {}

pub trait CancellableExtManual {
    #[doc(alias = "g_cancellable_connect")]
    fn connect<F: Fn(&Self) + Send + Sync + 'static>(&self, f: F) -> CancelledHandlerId;
    #[doc(alias = "g_cancellable_disconnect")]
    fn disconnect(&self, id: CancelledHandlerId);
    fn future(&self) -> std::pin::Pin<Box<dyn Future<Output = ()> + Send + 'static>>;
    fn source(&self) -> Source;
}

impl<O: IsA<Cancellable>> CancellableExtManual for O {
    #[doc(alias = "g_cancellable_connect")]
    fn connect<F: Fn(&Self) + Send + Sync + 'static>(&self, f: F) -> CancelledHandlerId {
        unsafe extern "C" fn connect_trampoline<
            P: IsA<Cancellable>,
            F: Fn(&P) + Send + Sync + 'static,
        >(
            this: *mut ffi::GCancellable,
            f: glib::ffi::gpointer,
        ) {
            let f: &mut Option<F> = &mut *(f as *mut Option<F>);
            let f = f
                .take()
                .expect("Cancellable::cancel() closure called multiple times");
            f(Cancellable::from_glib_borrow(this).unsafe_cast_ref())
        }

        unsafe extern "C" fn destroy_closure<F>(ptr: glib::ffi::gpointer) {
            Box::<F>::from_raw(ptr as *mut _);
        }

        unsafe {
            let f: Box<Option<F>> = Box::new(Some(f));
            let id = ffi::g_cancellable_connect(
                self.as_ptr() as *mut _,
                Some(std::mem::transmute::<_, unsafe extern "C" fn()>(
                    connect_trampoline::<Self, F> as *const (),
                )),
                Box::into_raw(f) as *mut _,
                Some(destroy_closure::<F>),
            );

            from_glib(id)
        }
    }
    #[doc(alias = "g_cancellable_disconnect")]
    fn disconnect(&self, id: CancelledHandlerId) {
        unsafe {
            ffi::g_cancellable_disconnect(
                self.as_ptr() as *mut _,
                id.as_raw()
            )
        };
    }
    fn future(&self) -> std::pin::Pin<Box<dyn Future<Output = ()> + Send + 'static>> {
        let cancellable = self.as_ref();
        Box::pin(glib::SourceFuture::new(glib::clone!(@strong cancellable => move |send| {
            let mut send = Some(send);
            CancellableExtManual::connect(&cancellable, move |_| {
                send.take().unwrap().send(()).ok();
            });
            cancellable.source()
        })))
    }
    fn source(&self) -> Source {
        unsafe {
            from_glib_full(ffi::g_cancellable_source_new(
                self.as_ref().to_glib_none().0
            ))
        }
    }
}

#[test]
fn check_callback() {
    let c = crate::Cancellable::new();
    c.connect_cancelled(|_| {});
    c.cancel(); // if it doesn't crash at this point, then we're good to go!
}
