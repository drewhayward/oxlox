use std::{
    ops::{Deref, DerefMut},
    ptr::NonNull,
};

use crate::object::{Object, ObjectType};


/// A reference to a Gc-managed value which allows cloning and dropping while ignoring the borrow
/// checking rules.
#[derive(Debug, Copy)]
pub struct GcRef<T> {
    ptr: *mut T,
}

impl<T> Clone for GcRef<T> {
    fn clone(&self) -> Self {
        GcRef { ptr: self.ptr }
    }
}

impl<T> Deref for GcRef<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { self.ptr.as_ref().unwrap() }
    }
}

impl<T> DerefMut for GcRef<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { self.ptr.as_mut().unwrap() }
    }
}

/// Lox runtime heap which implements garbage collection using mark-and-sweep.
#[derive(Debug)]
pub struct GcHeap {
    root: Option<NonNull<Object>>,
}

impl GcHeap {
    pub fn new() -> GcHeap {
        GcHeap { root: None }
    }

    pub fn allocate(&mut self, object: ObjectType) -> GcRef<Object> {
        let current_root = self.root.take();
        let boxed = Box::new(Object {
            next: current_root,
            value: object,
        });

        self.root = NonNull::new(Box::into_raw(boxed));

        GcRef {
            ptr: self.root.unwrap().as_ptr(),
        }
    }
}
