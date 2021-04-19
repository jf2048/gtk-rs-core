// This file was generated by gir (https://github.com/gtk-rs/gir)
// from gir-files (https://github.com/gtk-rs/gir-files)
// DO NOT EDIT

use crate::Euler;
use crate::Matrix;
use crate::Vec3;
use crate::Vec4;
use glib::translate::*;
use std::mem;

glib::wrapper! {
    #[derive(Debug, PartialOrd, Ord, Hash)]
    pub struct Quaternion(Boxed<ffi::graphene_quaternion_t>);

    match fn {
        copy => |ptr| glib::gobject_ffi::g_boxed_copy(ffi::graphene_quaternion_get_type(), ptr as *mut _) as *mut ffi::graphene_quaternion_t,
        free => |ptr| glib::gobject_ffi::g_boxed_free(ffi::graphene_quaternion_get_type(), ptr as *mut _),
        init => |_ptr| (),
        clear => |_ptr| (),
        type_ => || ffi::graphene_quaternion_get_type(),
    }
}

impl Quaternion {
    #[cfg(any(feature = "v1_10", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v1_10")))]
    #[doc(alias = "graphene_quaternion_add")]
    pub fn add(&self, b: &Quaternion) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_add(
                self.to_glib_none().0,
                b.to_glib_none().0,
                res.to_glib_none_mut().0,
            );
            res
        }
    }

    #[doc(alias = "graphene_quaternion_dot")]
    pub fn dot(&self, b: &Quaternion) -> f32 {
        unsafe { ffi::graphene_quaternion_dot(self.to_glib_none().0, b.to_glib_none().0) }
    }

    #[doc(alias = "graphene_quaternion_equal")]
    fn equal(&self, b: &Quaternion) -> bool {
        unsafe {
            from_glib(ffi::graphene_quaternion_equal(
                self.to_glib_none().0,
                b.to_glib_none().0,
            ))
        }
    }

    #[doc(alias = "graphene_quaternion_init")]
    pub fn init(&mut self, x: f32, y: f32, z: f32, w: f32) {
        unsafe {
            ffi::graphene_quaternion_init(self.to_glib_none_mut().0, x, y, z, w);
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_angle_vec3")]
    pub fn init_from_angle_vec3(&mut self, angle: f32, axis: &Vec3) {
        unsafe {
            ffi::graphene_quaternion_init_from_angle_vec3(
                self.to_glib_none_mut().0,
                angle,
                axis.to_glib_none().0,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_angles")]
    pub fn init_from_angles(&mut self, deg_x: f32, deg_y: f32, deg_z: f32) {
        unsafe {
            ffi::graphene_quaternion_init_from_angles(
                self.to_glib_none_mut().0,
                deg_x,
                deg_y,
                deg_z,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_euler")]
    pub fn init_from_euler(&mut self, e: &Euler) {
        unsafe {
            ffi::graphene_quaternion_init_from_euler(self.to_glib_none_mut().0, e.to_glib_none().0);
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_matrix")]
    pub fn init_from_matrix(&mut self, m: &Matrix) {
        unsafe {
            ffi::graphene_quaternion_init_from_matrix(
                self.to_glib_none_mut().0,
                m.to_glib_none().0,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_quaternion")]
    pub fn init_from_quaternion(&mut self, src: &Quaternion) {
        unsafe {
            ffi::graphene_quaternion_init_from_quaternion(
                self.to_glib_none_mut().0,
                src.to_glib_none().0,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_radians")]
    pub fn init_from_radians(&mut self, rad_x: f32, rad_y: f32, rad_z: f32) {
        unsafe {
            ffi::graphene_quaternion_init_from_radians(
                self.to_glib_none_mut().0,
                rad_x,
                rad_y,
                rad_z,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_from_vec4")]
    pub fn init_from_vec4(&mut self, src: &Vec4) {
        unsafe {
            ffi::graphene_quaternion_init_from_vec4(
                self.to_glib_none_mut().0,
                src.to_glib_none().0,
            );
        }
    }

    #[doc(alias = "graphene_quaternion_init_identity")]
    pub fn init_identity(&mut self) {
        unsafe {
            ffi::graphene_quaternion_init_identity(self.to_glib_none_mut().0);
        }
    }

    #[doc(alias = "graphene_quaternion_invert")]
    pub fn invert(&self) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_invert(self.to_glib_none().0, res.to_glib_none_mut().0);
            res
        }
    }

    #[cfg(any(feature = "v1_10", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v1_10")))]
    #[doc(alias = "graphene_quaternion_multiply")]
    pub fn multiply(&self, b: &Quaternion) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_multiply(
                self.to_glib_none().0,
                b.to_glib_none().0,
                res.to_glib_none_mut().0,
            );
            res
        }
    }

    #[doc(alias = "graphene_quaternion_normalize")]
    pub fn normalize(&self) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_normalize(self.to_glib_none().0, res.to_glib_none_mut().0);
            res
        }
    }

    #[cfg(any(feature = "v1_10", feature = "dox"))]
    #[cfg_attr(feature = "dox", doc(cfg(feature = "v1_10")))]
    #[doc(alias = "graphene_quaternion_scale")]
    pub fn scale(&self, factor: f32) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_scale(self.to_glib_none().0, factor, res.to_glib_none_mut().0);
            res
        }
    }

    #[doc(alias = "graphene_quaternion_slerp")]
    pub fn slerp(&self, b: &Quaternion, factor: f32) -> Quaternion {
        unsafe {
            let mut res = Quaternion::uninitialized();
            ffi::graphene_quaternion_slerp(
                self.to_glib_none().0,
                b.to_glib_none().0,
                factor,
                res.to_glib_none_mut().0,
            );
            res
        }
    }

    #[doc(alias = "graphene_quaternion_to_angle_vec3")]
    pub fn to_angle_vec3(&self) -> (f32, Vec3) {
        unsafe {
            let mut angle = mem::MaybeUninit::uninit();
            let mut axis = Vec3::uninitialized();
            ffi::graphene_quaternion_to_angle_vec3(
                self.to_glib_none().0,
                angle.as_mut_ptr(),
                axis.to_glib_none_mut().0,
            );
            let angle = angle.assume_init();
            (angle, axis)
        }
    }

    #[doc(alias = "graphene_quaternion_to_angles")]
    pub fn to_angles(&self) -> (f32, f32, f32) {
        unsafe {
            let mut deg_x = mem::MaybeUninit::uninit();
            let mut deg_y = mem::MaybeUninit::uninit();
            let mut deg_z = mem::MaybeUninit::uninit();
            ffi::graphene_quaternion_to_angles(
                self.to_glib_none().0,
                deg_x.as_mut_ptr(),
                deg_y.as_mut_ptr(),
                deg_z.as_mut_ptr(),
            );
            let deg_x = deg_x.assume_init();
            let deg_y = deg_y.assume_init();
            let deg_z = deg_z.assume_init();
            (deg_x, deg_y, deg_z)
        }
    }

    #[doc(alias = "graphene_quaternion_to_matrix")]
    pub fn to_matrix(&self) -> Matrix {
        unsafe {
            let mut m = Matrix::uninitialized();
            ffi::graphene_quaternion_to_matrix(self.to_glib_none().0, m.to_glib_none_mut().0);
            m
        }
    }

    #[doc(alias = "graphene_quaternion_to_radians")]
    pub fn to_radians(&self) -> (f32, f32, f32) {
        unsafe {
            let mut rad_x = mem::MaybeUninit::uninit();
            let mut rad_y = mem::MaybeUninit::uninit();
            let mut rad_z = mem::MaybeUninit::uninit();
            ffi::graphene_quaternion_to_radians(
                self.to_glib_none().0,
                rad_x.as_mut_ptr(),
                rad_y.as_mut_ptr(),
                rad_z.as_mut_ptr(),
            );
            let rad_x = rad_x.assume_init();
            let rad_y = rad_y.assume_init();
            let rad_z = rad_z.assume_init();
            (rad_x, rad_y, rad_z)
        }
    }

    #[doc(alias = "graphene_quaternion_to_vec4")]
    pub fn to_vec4(&self) -> Vec4 {
        unsafe {
            let mut res = Vec4::uninitialized();
            ffi::graphene_quaternion_to_vec4(self.to_glib_none().0, res.to_glib_none_mut().0);
            res
        }
    }
}

impl PartialEq for Quaternion {
    #[inline]
    fn eq(&self, other: &Self) -> bool {
        self.equal(other)
    }
}

impl Eq for Quaternion {}
