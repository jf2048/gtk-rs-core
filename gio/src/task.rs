// Take a look at the license at the top of the repository in the LICENSE file.

use crate::AsyncResult;
use crate::Cancellable;
use crate::prelude::*;
use futures_channel::oneshot;
use futures_core::Future;
use futures_util::FutureExt;
use futures_util::future::OptionFuture;
use glib::object::IsA;
use glib::object::ObjectType as ObjectType_;
use glib::signal::connect_raw;
use glib::signal::SignalHandlerId;
use glib::translate::*;
use glib::value::ValueType;
use glib::Cast;
use std::any::Any;
use std::boxed::Box as Box_;
use std::fmt;
use std::io;
use std::mem::transmute;
use std::panic;
use std::ptr;

glib::wrapper! {
    // rustdoc-stripper-ignore-next
    /// `LocalTask` provides idiomatic access to gio's `GTask` API, for
    /// instance by being generic over their value type, while not completely departing
    /// from the underlying C API. `LocalTask` does not require its value to be `Send`
    /// and `Sync` and thus is useful to to implement gio style asynchronous
    /// tasks that run in the glib main loop. If you need to run tasks in threads
    /// see the `Task` type.
    ///
    /// The constructors of `LocalTask` and `Task` is marked as unsafe because this API does
    /// not allow to automatically enforce all the invariants required to be a completely
    /// safe abstraction. See the `Task` type for more details.
    #[doc(alias = "GTask")]
    pub struct LocalTask<V: ValueType>(Object<ffi::GTask, ffi::GTaskClass>) @implements AsyncResult;

    match fn {
        type_ => || ffi::g_task_get_type(),
    }
}

glib::wrapper! {
    // rustdoc-stripper-ignore-next
    /// `Task` provides idiomatic access to gio's `GTask` API, for
    /// instance by being generic over their value type, while not completely departing
    /// from the underlying C API. `Task` is `Send` and `Sync` and requires its value to
    /// also be `Send` and `Sync`, thus is useful to to implement gio style asynchronous
    /// tasks that run in threads. If you need to only run tasks in glib main loop
    /// see the `LocalTask` type.
    ///
    /// The constructors of `LocalTask` and `Task` is marked as unsafe because this API does
    /// not allow to automatically enforce all the invariants required to be a completely
    /// safe abstraction. The caller is responsible to ensure the following requirements
    /// are satisfied
    ///
    /// * You should not create a `LocalTask`, upcast it to a `glib::Object` and then
    ///   downcast it to a `Task`, as this will bypass the thread safety requirements
    /// * You should ensure that the `return_result`, `return_error_if_cancelled` and
    ///   `propagate()` methods are only called once.
    #[doc(alias = "GTask")]
    pub struct Task<V: ValueType + Send>(Object<ffi::GTask, ffi::GTaskClass>) @implements AsyncResult;

    match fn {
        type_ => || ffi::g_task_get_type(),
    }
}

macro_rules! task_impl {
    ($name:ident $(, @bound: $bound:tt)? $(, @safety: $safety:tt)?) => {
        impl <V: ValueType $(+ $bound)?> $name<V> {
            #[doc(alias = "g_task_new")]
            #[allow(unused_unsafe)]
            pub unsafe fn new<S, P, Q>(
                source_object: Option<&S>,
                cancellable: Option<&P>,
                callback: Q,
            ) -> Self
            where
                S: IsA<glib::Object> $(+ $bound)?,
                P: IsA<Cancellable>,
                Q: FnOnce($name<V>, Option<&S>) $(+ $bound)? + 'static,
            {
                let callback_data = Box_::new(callback);
                unsafe extern "C" fn trampoline<
                    S: IsA<glib::Object> $(+ $bound)?,
                    V: ValueType $(+ $bound)?,
                    Q: FnOnce($name<V>, Option<&S>) $(+ $bound)? + 'static,
                >(
                    source_object: *mut glib::gobject_ffi::GObject,
                    res: *mut ffi::GAsyncResult,
                    user_data: glib::ffi::gpointer,
                ) {
                    let callback: Box_<Q> = Box::from_raw(user_data as *mut _);
                    let task = AsyncResult::from_glib_none(res)
                        .downcast::<$name<V>>()
                        .unwrap();
                    let source_object = Option::<glib::Object>::from_glib_borrow(source_object);
                    callback(
                        task,
                        source_object.as_ref().as_ref().map(|s| s.unsafe_cast_ref()),
                    );
                }
                let callback = trampoline::<S, V, Q>;
                unsafe {
                    from_glib_full(ffi::g_task_new(
                        source_object.map(|p| p.as_ref()).to_glib_none().0,
                        cancellable.map(|p| p.as_ref()).to_glib_none().0,
                        Some(callback),
                        Box_::into_raw(callback_data) as *mut _,
                    ))
                }
            }

            #[doc(alias = "g_task_get_cancellable")]
            #[doc(alias = "get_cancellable")]
            pub fn cancellable(&self) -> Cancellable {
                unsafe { from_glib_none(ffi::g_task_get_cancellable(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_get_check_cancellable")]
            #[doc(alias = "get_check_cancellable")]
            pub fn is_check_cancellable(&self) -> bool {
                unsafe { from_glib(ffi::g_task_get_check_cancellable(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_set_check_cancellable")]
            pub fn set_check_cancellable(&self, check_cancellable: bool) {
                unsafe {
                    ffi::g_task_set_check_cancellable(self.to_glib_none().0, check_cancellable.into_glib());
                }
            }

            #[cfg(any(feature = "v2_60", feature = "dox"))]
            #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_60")))]
            #[doc(alias = "g_task_set_name")]
            pub fn set_name(&self, name: Option<&str>) {
                unsafe {
                    ffi::g_task_set_name(self.to_glib_none().0, name.to_glib_none().0);
                }
            }

            #[doc(alias = "g_task_set_return_on_cancel")]
            pub fn set_return_on_cancel(&self, return_on_cancel: bool) -> bool {
                unsafe {
                    from_glib(ffi::g_task_set_return_on_cancel(
                        self.to_glib_none().0,
                        return_on_cancel.into_glib(),
                    ))
                }
            }

            #[doc(alias = "g_task_is_valid")]
            pub fn is_valid(
                result: &impl IsA<AsyncResult>,
                source_object: Option<&impl IsA<glib::Object>>,
            ) -> bool {
                unsafe {
                    from_glib(ffi::g_task_is_valid(
                        result.as_ref().to_glib_none().0,
                        source_object.map(|p| p.as_ref()).to_glib_none().0,
                    ))
                }
            }

            #[doc(alias = "get_priority")]
            #[doc(alias = "g_task_get_priority")]
            pub fn priority(&self) -> glib::source::Priority {
                unsafe { FromGlib::from_glib(ffi::g_task_get_priority(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_set_priority")]
            pub fn set_priority(&self, priority: glib::source::Priority) {
                unsafe {
                    ffi::g_task_set_priority(self.to_glib_none().0, priority.into_glib());
                }
            }

            #[doc(alias = "g_task_get_completed")]
            #[doc(alias = "get_completed")]
            pub fn is_completed(&self) -> bool {
                unsafe { from_glib(ffi::g_task_get_completed(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_get_context")]
            #[doc(alias = "get_context")]
            pub fn context(&self) -> glib::MainContext {
                unsafe { from_glib_none(ffi::g_task_get_context(self.to_glib_none().0)) }
            }

            #[cfg(any(feature = "v2_60", feature = "dox"))]
            #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_60")))]
            #[doc(alias = "g_task_get_name")]
            #[doc(alias = "get_name")]
            pub fn name(&self) -> Option<glib::GString> {
                unsafe { from_glib_none(ffi::g_task_get_name(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_get_return_on_cancel")]
            #[doc(alias = "get_return_on_cancel")]
            pub fn is_return_on_cancel(&self) -> bool {
                unsafe { from_glib(ffi::g_task_get_return_on_cancel(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_had_error")]
            pub fn had_error(&self) -> bool {
                unsafe { from_glib(ffi::g_task_had_error(self.to_glib_none().0)) }
            }

            #[doc(alias = "completed")]
            pub fn connect_completed_notify<F>(&self, f: F) -> SignalHandlerId
            where
                F: Fn(&$name<V>) $(+ $bound)? + 'static,
            {
                unsafe extern "C" fn notify_completed_trampoline<V, F>(
                    this: *mut ffi::GTask,
                    _param_spec: glib::ffi::gpointer,
                    f: glib::ffi::gpointer,
                ) where
                    V: ValueType $(+ $bound)?,
                    F: Fn(&$name<V>) + 'static,
                {
                    let f: &F = &*(f as *const F);
                    f(&from_glib_borrow(this))
                }
                unsafe {
                    let f: Box_<F> = Box_::new(f);
                    connect_raw(
                        self.as_ptr() as *mut _,
                        b"notify::completed\0".as_ptr() as *const _,
                        Some(transmute::<_, unsafe extern "C" fn()>(
                            notify_completed_trampoline::<V, F> as *const (),
                        )),
                        Box_::into_raw(f),
                    )
                }
            }

            // the following functions are marked unsafe since they cannot be called
            // more than once, but we have no way to enforce that since the task can be cloned

            #[doc(alias = "g_task_return_error_if_cancelled")]
            #[allow(unused_unsafe)]
            pub $($safety)? fn return_error_if_cancelled(&self) -> bool {
                unsafe { from_glib(ffi::g_task_return_error_if_cancelled(self.to_glib_none().0)) }
            }

            #[doc(alias = "g_task_return_value")]
            #[doc(alias = "g_task_return_boolean")]
            #[doc(alias = "g_task_return_int")]
            #[doc(alias = "g_task_return_pointer")]
            #[doc(alias = "g_task_return_error")]
            #[allow(unused_unsafe)]
            pub $($safety)? fn return_result(self, result: Result<V, glib::Error>) {
                #[cfg(not(feature = "v2_64"))]
                unsafe extern "C" fn value_free(value: *mut libc::c_void) {
                    let _: glib::Value = from_glib_full(value as *mut glib::gobject_ffi::GValue);
                }

                match result {
                    #[cfg(feature = "v2_64")]
                    Ok(v) => unsafe {
                        ffi::g_task_return_value(
                            self.to_glib_none().0,
                            v.to_value().to_glib_full() as *mut _,
                        )
                    },
                    #[cfg(not(feature = "v2_64"))]
                    Ok(v) => unsafe {
                        ffi::g_task_return_pointer(
                            self.to_glib_none().0,
                            v.to_value().to_glib_full() as *mut _,
                            Some(value_free),
                        )
                    },
                    Err(e) => unsafe {
                        ffi::g_task_return_error(self.to_glib_none().0, e.to_glib_full() as *mut _);
                    },
                }
            }

            #[doc(alias = "g_task_propagate_value")]
            #[doc(alias = "g_task_propagate_boolean")]
            #[doc(alias = "g_task_propagate_int")]
            #[doc(alias = "g_task_propagate_pointer")]
            #[allow(unused_unsafe)]
            pub $($safety)? fn propagate(self) -> Result<V, glib::Error> {
                let mut error = ptr::null_mut();

                unsafe {
                    #[cfg(feature = "v2_64")]
                    {
                        let mut value = glib::Value::uninitialized();
                        ffi::g_task_propagate_value(
                            self.to_glib_none().0,
                            value.to_glib_none_mut().0,
                            &mut error,
                        );

                        if error.is_null() {
                            Ok(V::from_value(&value))
                        } else {
                            Err(from_glib_full(error))
                        }
                    }

                    #[cfg(not(feature = "v2_64"))]
                    {
                        let value = ffi::g_task_propagate_pointer(self.to_glib_none().0, &mut error);

                        if error.is_null() {
                            let value = Option::<glib::Value>::from_glib_full(
                                value as *mut glib::gobject_ffi::GValue,
                            )
                            .expect("Task::propagate() called before Task::return_result()");
                            Ok(V::from_value(&value))
                        } else {
                            Err(from_glib_full(error))
                        }
                    }
                }
            }
        }

        impl <V: ValueType $(+ $bound)?> std::fmt::Display for $name<V> {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                f.write_str(stringify!($name))
            }
        }
    }
}

task_impl!(LocalTask);
task_impl!(Task, @bound: Send, @safety: unsafe);

impl<V: ValueType + Send> Task<V> {
    #[doc(alias = "g_task_run_in_thread")]
    pub fn run_in_thread<S, Q>(&self, task_func: Q)
    where
        S: IsA<glib::Object> + Send,
        Q: FnOnce(Self, Option<&S>, Option<&Cancellable>) + Send + 'static,
    {
        let task_func_data = Box_::new(task_func);

        // We store the func pointer into the task data.
        // We intentionally do not expose a way to set the task data in the bindings.
        // If we detect that the task data is set, there is not much we can do, so we panic.
        unsafe {
            assert!(
                ffi::g_task_get_task_data(self.to_glib_none().0).is_null(),
                "Task data was manually set or the task was run thread multiple times"
            );

            ffi::g_task_set_task_data(
                self.to_glib_none().0,
                Box_::into_raw(task_func_data) as *mut _,
                None,
            );
        }

        unsafe extern "C" fn trampoline<V, S, Q>(
            task: *mut ffi::GTask,
            source_object: *mut glib::gobject_ffi::GObject,
            user_data: glib::ffi::gpointer,
            cancellable: *mut ffi::GCancellable,
        ) where
            V: ValueType + Send,
            S: IsA<glib::Object> + Send,
            Q: FnOnce(Task<V>, Option<&S>, Option<&Cancellable>) + Send + 'static,
        {
            let task = Task::from_glib_none(task);
            let source_object = Option::<glib::Object>::from_glib_borrow(source_object);
            let cancellable = Option::<Cancellable>::from_glib_borrow(cancellable);
            let task_func: Box_<Q> = Box::from_raw(user_data as *mut _);
            task_func(
                task,
                source_object.as_ref().as_ref().map(|s| s.unsafe_cast_ref()),
                cancellable.as_ref().as_ref(),
            );
        }

        let task_func = trampoline::<V, S, Q>;
        unsafe {
            ffi::g_task_run_in_thread(self.to_glib_none().0, Some(task_func));
        }
    }
}

unsafe impl<V: ValueType + Send> Send for Task<V> {}
unsafe impl<V: ValueType + Send> Sync for Task<V> {}

pub fn spawn_blocking<T: Send + 'static, F: FnOnce(Option<&Cancellable>) -> T + Send + 'static>(
    cancellable: Option<&Cancellable>,
    func: F,
) -> JoinHandle<T> {
    let task = unsafe { Task::new(None, cancellable, |_, _: Option<&Cancellable>| {}) };
    let (join, tx) = JoinHandle::new(cancellable.cloned());

    task.run_in_thread(move |_task: Task<bool>,
                             _source_object: Option<&Cancellable>,
                             cancellable: Option<&Cancellable>| {
        let res = std::panic::catch_unwind(panic::AssertUnwindSafe(move || func(cancellable)));
        let cancelled = cancellable.map(|c| c.is_cancelled()).unwrap_or(false);
        if !cancelled {
            tx.send(res).ok();
        }
    });

    join
}

#[derive(Debug)]
pub struct JoinHandle<T> {
    rx: Option<oneshot::Receiver<Result<T, Box<dyn Any + Send + 'static>>>>,
    cancellable: Option<Cancellable>
}

unsafe impl<T: Send> Send for JoinHandle<T> {}
unsafe impl<T: Send> Sync for JoinHandle<T> {}

impl<T> JoinHandle<T> {
    fn new(cancellable: Option<Cancellable>) -> (Self, oneshot::Sender<Result<T, Box<dyn Any + Send + 'static>>>) {
        let (tx, rx) = oneshot::channel();
        (Self {
            rx: Some(rx),
            cancellable
        }, tx)
    }
    pub fn cancellable(&self) -> Option<&Cancellable> {
        self.cancellable.as_ref()
    }
}

impl<T> Unpin for JoinHandle<T> {}

impl<T> Future for JoinHandle<T> {
    type Output = Result<T, JoinError>;

    fn poll(mut self: std::pin::Pin<&mut Self>, cx: &mut std::task::Context<'_>) -> std::task::Poll<Self::Output> {
        let mut rx = match self.rx.take() {
            Some(rx) => rx,
            None => return std::task::Poll::Pending
        };
        let mut cancel = OptionFuture::from(self.cancellable.as_ref().map(|c| c.future().fuse()));
        Box::pin(async move {
            futures_util::select! {
                res = rx =>  {
                    res.map_err(|_| {
                        let cancellable = self.cancellable().unwrap();
                        cancellable.cancel();
                        JoinError::cancelled(cancellable)
                    })
                    .and_then(|res| res.map_err(JoinError::panic))
                },
                _ = cancel => {
                    Err(JoinError::cancelled(self.cancellable().unwrap()))
                }
            }
        }).poll_unpin(cx)
    }
}

impl<T> futures_core::FusedFuture for JoinHandle<T> {
    fn is_terminated(&self) -> bool {
       self.rx.is_none()
    }
}

pub struct JoinError {
    inner: JoinErrorInner,
}

enum JoinErrorInner {
    Cancelled(glib::Error),
    Panic(Box<dyn Any + Send + 'static>),
}

impl JoinError {
    fn cancelled(cancellable: &Cancellable) -> Self {
        Self {
            inner: JoinErrorInner::Cancelled(cancellable.set_error_if_cancelled().unwrap_err()),
        }
    }

    fn panic(err: Box<dyn Any + Send + 'static>) -> Self {
        Self {
            inner: JoinErrorInner::Panic(err),
        }
    }

    pub fn is_cancelled(&self) -> bool {
        matches!(&self.inner, JoinErrorInner::Cancelled(_))
    }

    pub fn into_cancelled(self) -> glib::Error {
        self.try_into_cancelled()
            .expect("`JoinError` reason is not a cancelled.")
    }

    pub fn try_into_cancelled(self) -> Result<glib::Error, JoinError> {
        match self.inner {
            JoinErrorInner::Cancelled(e) => Ok(e),
            _ => Err(self),
        }
    }

    pub fn is_panic(&self) -> bool {
        matches!(&self.inner, JoinErrorInner::Panic(_))
    }

    pub fn into_panic(self) -> Box<dyn Any + Send + 'static> {
        self.try_into_panic()
            .expect("`JoinError` reason is not a panic.")
    }

    pub fn try_into_panic(self) -> Result<Box<dyn Any + Send + 'static>, JoinError> {
        match self.inner {
            JoinErrorInner::Panic(p) => Ok(p),
            _ => Err(self),
        }
    }
}

impl fmt::Display for JoinError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            JoinErrorInner::Cancelled(_) => write!(fmt, "cancelled"),
            JoinErrorInner::Panic(_) => write!(fmt, "panic"),
        }
    }
}

impl fmt::Debug for JoinError {
    fn fmt(&self, fmt: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.inner {
            JoinErrorInner::Cancelled(_) => write!(fmt, "JoinError::Cancelled"),
            JoinErrorInner::Panic(_) => write!(fmt, "JoinError::Panic(...)"),
        }
    }
}

impl std::error::Error for JoinError {}

impl From<JoinError> for io::Error {
    fn from(src: JoinError) -> io::Error {
        io::Error::new(
            io::ErrorKind::Other,
            match src.inner {
                JoinErrorInner::Cancelled(_) => "task was cancelled",
                JoinErrorInner::Panic(_) => "task panicked",
            },
        )
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::prelude::*;
    use crate::test_util::run_async_local;

    #[test]
    fn test_int_async_result() {
        match run_async_local(|tx, l| {
            let cancellable = crate::Cancellable::new();
            let task = unsafe {
                crate::LocalTask::new(
                    None,
                    Some(&cancellable),
                    move |t: LocalTask<i32>, _b: Option<&glib::Object>| {
                        tx.send(t.propagate()).unwrap();
                        l.quit();
                    },
                )
            };
            task.return_result(Ok(100_i32));
        }) {
            Err(_) => panic!(),
            Ok(i) => assert_eq!(i, 100),
        }
    }

    #[test]
    fn test_object_async_result() {
        use glib::subclass::prelude::*;
        pub struct MySimpleObjectPrivate {
            pub size: std::cell::RefCell<Option<i64>>,
        }

        #[glib::object_subclass]
        impl ObjectSubclass for MySimpleObjectPrivate {
            const NAME: &'static str = "MySimpleObjectPrivate";
            type Type = MySimpleObject;

            fn new() -> Self {
                Self {
                    size: std::cell::RefCell::new(Some(100)),
                }
            }
        }

        impl ObjectImpl for MySimpleObjectPrivate {}

        glib::wrapper! {
            pub struct MySimpleObject(ObjectSubclass<MySimpleObjectPrivate>);
        }

        impl MySimpleObject {
            pub fn new() -> Self {
                glib::Object::new(&[]).expect("Failed to create MySimpleObject")
            }

            #[doc(alias = "get_size")]
            pub fn size(&self) -> Option<i64> {
                *self.imp().size.borrow()
            }

            pub fn set_size(&self, size: i64) {
                self.imp().size.borrow_mut().replace(size);
            }
        }

        impl Default for MySimpleObject {
            fn default() -> Self {
                Self::new()
            }
        }

        match run_async_local(|tx, l| {
            let cancellable = crate::Cancellable::new();
            let task = unsafe {
                crate::LocalTask::new(
                    None,
                    Some(&cancellable),
                    move |t: LocalTask<glib::Object>, _b: Option<&glib::Object>| {
                        tx.send(t.propagate()).unwrap();
                        l.quit();
                    },
                )
            };
            let my_object = MySimpleObject::new();
            my_object.set_size(100);
            task.return_result(Ok(my_object.upcast::<glib::Object>()));
        }) {
            Err(_) => panic!(),
            Ok(o) => {
                let o = o.downcast::<MySimpleObject>().unwrap();
                assert_eq!(o.size(), Some(100));
            }
        }
    }

    #[test]
    fn test_error() {
        match run_async_local(|tx, l| {
            let cancellable = crate::Cancellable::new();
            let task = unsafe {
                crate::LocalTask::new(
                    None,
                    Some(&cancellable),
                    move |t: LocalTask<i32>, _b: Option<&glib::Object>| {
                        tx.send(t.propagate()).unwrap();
                        l.quit();
                    },
                )
            };
            task.return_result(Err(glib::Error::new(
                crate::IOErrorEnum::WouldBlock,
                "WouldBlock",
            )));
        }) {
            Err(e) => match e.kind().unwrap() {
                crate::IOErrorEnum::WouldBlock => {}
                _ => panic!(),
            },
            Ok(_) => panic!(),
        }
    }

    #[test]
    fn test_cancelled() {
        match run_async_local(|tx, l| {
            let cancellable = crate::Cancellable::new();
            let task = unsafe {
                crate::LocalTask::new(
                    None,
                    Some(&cancellable),
                    move |t: LocalTask<i32>, _b: Option<&glib::Object>| {
                        tx.send(t.propagate()).unwrap();
                        l.quit();
                    },
                )
            };
            cancellable.cancel();
            task.return_error_if_cancelled();
        }) {
            Err(e) => match e.kind().unwrap() {
                crate::IOErrorEnum::Cancelled => {}
                _ => panic!(),
            },
            Ok(_) => panic!(),
        }
    }
}
