use crate::auto::traits::ListModelExt;
use crate::ListModel;
use glib::object::Cast;
use glib::object::IsA;
use glib::Object;
use glib::ObjectExt;
use std::fmt;

glib::wrapper! {
    pub struct TypedListModel<T: (IsA<Object>)>(Interface<ffi::GListModel, ffi::GListModelInterface>)
        @extra_traits {},
        @checkers (TypedListModelCastChecker<T>) (TypedListModelValueChecker<T>),
        @requires ListModel;

    match fn {
        type_ => || ffi::g_list_model_get_type(),
    }
}

#[doc(hidden)]
unsafe impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>>
    IsA<TypedListModel<Super>> for TypedListModel<Sub>
{
}

#[doc(hidden)]
impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>> AsRef<TypedListModel<Super>>
    for TypedListModel<Sub>
{
    fn as_ref(&self) -> &TypedListModel<Super> {
        self.upcast_ref()
    }
}

#[doc(hidden)]
pub struct TypedListModelCastChecker<T>(std::marker::PhantomData<T>);

impl<T: IsA<Object>> glib::object::ObjectCastChecker<TypedListModel<T>>
    for TypedListModelCastChecker<T>
{
    fn check<U: glib::ObjectType>(obj: &U) -> bool {
        if glib::object::GenericObjectCastChecker::<ListModel>::check(obj) {
            let obj = unsafe { obj.unsafe_cast_ref::<ListModel>() };
            if obj.item_type().is_a(T::static_type()) {
                return true;
            }
        }
        false
    }
}

#[doc(hidden)]
pub struct TypedListModelValueChecker<T>(std::marker::PhantomData<T>);

unsafe impl<T: IsA<Object>> glib::value::ValueTypeChecker for TypedListModelValueChecker<T> {
    type Error = glib::value::ValueTypeMismatchOrNoneError<glib::value::ValueTypeMismatchError>;

    fn check(value: &glib::Value) -> Result<(), Self::Error> {
        if let Some(model) = value.get::<Option<&ListModel>>()? {
            let model_type = model.item_type();
            let expected = T::static_type();
            if !model_type.is_a(expected) {
                return Err(glib::value::ValueTypeMismatchError::new(model_type, expected).into());
            }
        }

        Ok(())
    }
}

impl<T: IsA<Object>> TypedListModel<T> {
    pub const NONE: Option<&'static TypedListModel<T>> = None;
}

pub trait TypedListModelExt: IsA<ListModel> {
    #[doc(alias = "g_list_model_get_object")]
    #[doc(alias = "get_object")]
    fn item<T: IsA<Object>>(&self, position: u32) -> Option<T>
    where
        Self: IsA<TypedListModel<T>>;
    // rustdoc-stripper-ignore-next
    /// Get an immutable snapshot of the container inside the `TypedListModel`.
    /// Any modification done to the returned container `Vec` will not be
    /// reflected on the `TypedListModel`.
    fn snapshot_items<T: IsA<Object>>(&self) -> Vec<T>
    where
        Self: IsA<TypedListModel<T>>;
    // rustdoc-stripper-ignore-next
    /// Returns an iterator with the elements returned by [`Self::snapshot_items`]
    fn into_items_iter<T: IsA<Object>>(self) -> std::vec::IntoIter<T>
    where
        Self: IsA<TypedListModel<T>>;
}

impl<O: IsA<ListModel>> TypedListModelExt for O {
    fn item<T: IsA<Object>>(&self, position: u32) -> Option<T>
    where
        Self: IsA<TypedListModel<T>>,
    {
        self.object(position).map(|o| {
            o.downcast().unwrap_or_else(|o| {
                panic!(
                    "List model type mismatch. Actual {:?}, requested {:?}",
                    o.type_(),
                    T::static_type()
                );
            })
        })
    }
    fn snapshot_items<T: IsA<Object>>(&self) -> Vec<T>
    where
        Self: IsA<TypedListModel<T>>,
    {
        let count = self.n_items();
        let mut res = Vec::with_capacity(count as usize);
        for i in 0..count {
            res.push(self.item(i).unwrap())
        }
        res
    }
    fn into_items_iter<T: IsA<Object>>(self) -> std::vec::IntoIter<T>
    where
        Self: IsA<TypedListModel<T>>,
    {
        self.snapshot_items().into_iter()
    }
}

impl<T: IsA<Object>> fmt::Display for TypedListModel<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TypedListModel")
    }
}
