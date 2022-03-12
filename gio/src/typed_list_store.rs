use crate::traits::ListModelExt;
use crate::ListModel;
use crate::ListStore;
use crate::TypedListModel;
use glib::object::Cast;
use glib::object::IsA;
use glib::Object;
use std::cmp::Ordering;
use std::fmt;

glib::wrapper! {
    pub struct TypedListStore<T: (IsA<Object>)>(Object<ffi::GListStore, ffi::GListStoreClass>)
        @extra_traits {},
        @checkers (TypedListStoreCastChecker<T>) (TypedListStoreValueChecker<T>),
        @implements ListStore, ListModel;

    match fn {
        type_ => || ffi::g_list_store_get_type(),
    }
}

#[doc(hidden)]
unsafe impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>>
    IsA<TypedListStore<Super>> for TypedListStore<Sub>
{
}

#[doc(hidden)]
impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>> AsRef<TypedListStore<Super>>
    for TypedListStore<Sub>
{
    fn as_ref(&self) -> &TypedListStore<Super> {
        self.upcast_ref()
    }
}

#[doc(hidden)]
unsafe impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>>
    IsA<TypedListModel<Super>> for TypedListStore<Sub>
{
}

#[doc(hidden)]
impl<Super: IsA<Super> + IsA<Object>, Sub: IsA<Super> + IsA<Object>> AsRef<TypedListModel<Super>>
    for TypedListStore<Sub>
{
    fn as_ref(&self) -> &TypedListModel<Super> {
        self.upcast_ref()
    }
}

#[doc(hidden)]
pub struct TypedListStoreCastChecker<T>(std::marker::PhantomData<T>);

impl<T: IsA<Object>> glib::object::ObjectCastChecker<TypedListStore<T>>
    for TypedListStoreCastChecker<T>
{
    fn check<U: glib::ObjectType>(obj: &U) -> bool {
        if glib::object::GenericObjectCastChecker::<ListStore>::check(obj) {
            let obj = unsafe { obj.unsafe_cast_ref::<ListStore>() };
            if obj.item_type().is_a(T::static_type()) {
                return true;
            }
        }
        false
    }
}

#[doc(hidden)]
pub struct TypedListStoreValueChecker<T>(std::marker::PhantomData<T>);

unsafe impl<T: IsA<Object>> glib::value::ValueTypeChecker for TypedListStoreValueChecker<T> {
    type Error = glib::value::ValueTypeMismatchOrNoneError<glib::value::ValueTypeMismatchError>;

    fn check(value: &glib::Value) -> Result<(), Self::Error> {
        if let Some(model) = value.get::<Option<&ListStore>>()? {
            let model_type = model.item_type();
            let expected = T::static_type();
            if !model_type.is_a(expected) {
                return Err(glib::value::ValueTypeMismatchError::new(model_type, expected).into());
            }
        }

        Ok(())
    }
}

impl<T: IsA<Object>> TypedListStore<T> {
    #[doc(alias = "g_list_store_new")]
    #[inline]
    pub fn new() -> Self {
        let store = ListStore::new(T::static_type());
        unsafe { store.unsafe_cast() }
    }

    #[inline]
    #[doc(alias = "g_list_store_append")]
    pub fn append<U: IsA<T>>(&self, item: &U) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }
            .append(item.upcast_ref::<T>().upcast_ref::<Object>())
    }

    #[inline]
    #[cfg(any(feature = "v2_64", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v2_64")))]
    #[doc(alias = "g_list_store_find")]
    pub fn find<U: IsA<T>>(&self, item: &U) -> Option<u32> {
        unsafe { self.unsafe_cast_ref::<ListStore>() }
            .find(item.upcast_ref::<T>().upcast_ref::<Object>())
    }

    #[inline]
    #[doc(alias = "g_list_store_insert")]
    pub fn insert<U: IsA<T>>(&self, position: u32, item: &U) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }
            .insert(position, item.upcast_ref::<T>().upcast_ref::<Object>())
    }

    #[doc(alias = "g_list_store_remove")]
    pub fn remove(&self, position: u32) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }.remove(position)
    }

    #[inline]
    #[doc(alias = "g_list_store_remove_all")]
    pub fn remove_all(&self) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }.remove_all()
    }
    #[inline]
    #[doc(alias = "g_list_store_insert_sorted")]
    pub fn insert_sorted<U: IsA<T>, F: FnMut(&T, &T) -> Ordering>(
        &self,
        item: &U,
        mut compare_func: F,
    ) -> u32 {
        unsafe { self.unsafe_cast_ref::<ListStore>() }.insert_sorted(
            item.upcast_ref::<T>().upcast_ref::<Object>(),
            move |a, b| compare_func(a.downcast_ref().unwrap(), b.downcast_ref().unwrap()),
        )
    }

    #[inline]
    #[doc(alias = "g_list_store_sort")]
    pub fn sort<F: FnMut(&T, &T) -> Ordering>(&self, mut compare_func: F) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }
            .sort(move |a, b| compare_func(a.downcast_ref().unwrap(), b.downcast_ref().unwrap()))
    }

    #[inline]
    #[doc(alias = "g_list_store_splice")]
    pub fn splice(&self, position: u32, n_removals: u32, additions: &[impl IsA<T> + IsA<Object>]) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }.splice(position, n_removals, additions)
    }

    // rustdoc-stripper-ignore-next
    /// Appends all elements in a slice to the `TypedListStore`.
    #[inline]
    pub fn extend_from_slice(&self, additions: &[impl IsA<T> + IsA<Object>]) {
        unsafe { self.unsafe_cast_ref::<ListStore>() }.extend_from_slice(additions)
    }
}

impl<T: IsA<Object>, A: IsA<T> + IsA<Object>> std::iter::Extend<A> for TypedListStore<T> {
    fn extend<I: IntoIterator<Item = A>>(&mut self, iter: I) {
        let additions = iter.into_iter().collect::<Vec<_>>();
        self.splice(self.n_items(), 0, &additions)
    }
}

impl<T: IsA<Object>> Default for TypedListStore<T> {
    fn default() -> Self {
        glib::object::Object::new::<Self>(&[])
            .expect("Can't construct TypedListStore object with default parameters")
    }
}

impl<T: IsA<Object>> fmt::Display for TypedListStore<T> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.write_str("TypedListStore")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::auto::traits::FileExt;
    use crate::prelude::TypedListModelExt;
    use crate::Application;
    use crate::File;
    use glib::StaticType;
    use glib::ToValue;
    use std::path::PathBuf;

    #[test]
    fn type_checking() {
        let store = TypedListStore::<File>::new();
        store.append(&File::for_path("/"));
        let _: Object = store.item(0).unwrap();
        assert_eq!(
            store.item::<File>(0).unwrap().path().unwrap(),
            PathBuf::from("/")
        );

        let object = store.upcast_ref::<Object>();
        object.downcast_ref::<TypedListStore<Object>>().unwrap();
        object.downcast_ref::<ListModel>().unwrap();
        object.downcast_ref::<TypedListModel<File>>().unwrap();
        object.downcast_ref::<TypedListModel<Object>>().unwrap();
        assert!(object
            .downcast_ref::<TypedListStore<Application>>()
            .is_none());
        assert!(object
            .downcast_ref::<TypedListModel<Application>>()
            .is_none());

        let model = store.upcast_ref::<ListModel>();
        assert_eq!(model.item_type(), File::static_type());
        model.downcast_ref::<TypedListStore<Object>>().unwrap();
        model.downcast_ref::<TypedListModel<File>>().unwrap();
        model.downcast_ref::<TypedListModel<Object>>().unwrap();
        assert!(model
            .downcast_ref::<TypedListStore<Application>>()
            .is_none());
        assert!(model
            .downcast_ref::<TypedListModel<Application>>()
            .is_none());

        let typed_model = store.upcast_ref::<TypedListModel<File>>();
        assert_eq!(typed_model.item_type(), File::static_type());
        typed_model.downcast_ref::<TypedListStore<File>>().unwrap();

        let object_model = store.upcast_ref::<TypedListModel<Object>>();
        object_model
            .downcast_ref::<TypedListStore<Object>>()
            .unwrap();
        object_model.downcast_ref::<TypedListModel<File>>().unwrap();
        object_model.downcast_ref::<TypedListStore<File>>().unwrap();

        let untyped_store = store.upcast_ref::<ListStore>();
        untyped_store.upcast_ref::<TypedListModel<Object>>();
        untyped_store.upcast_ref::<TypedListStore<Object>>();
        untyped_store
            .dynamic_cast_ref::<TypedListModel<File>>()
            .unwrap();
        assert!(untyped_store
            .downcast_ref::<TypedListStore<Application>>()
            .is_none());
        assert!(untyped_store
            .dynamic_cast_ref::<TypedListModel<Application>>()
            .is_none());

        let object_store = store.upcast_ref::<TypedListStore<Object>>();
        object_store.upcast_ref::<TypedListModel<Object>>();
        object_store
            .dynamic_cast_ref::<TypedListModel<File>>()
            .unwrap();
        object_store.downcast_ref::<TypedListStore<File>>().unwrap();

        let value = store.to_value();
        value.get::<ListStore>().unwrap();
        value.get::<TypedListStore<File>>().unwrap();
        value.get::<TypedListStore<Object>>().unwrap();
        value.get::<ListModel>().unwrap();
        value.get::<TypedListModel<File>>().unwrap();
        value.get::<TypedListModel<Object>>().unwrap();
        assert!(value.get::<TypedListStore<Application>>().is_err());
        assert!(value.get::<TypedListModel<Application>>().is_err());
    }
}
