use std::{
    collections::HashMap,
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

impl<T> GcRef<T> {
    pub fn ptr_equal(&self, other: &GcRef<T>) -> bool {
        self.ptr == other.ptr
    }
}

impl GcRef<Object> {
    /// Obtain a ref to the underlying GC object
    pub fn as_obj_ref(&self) -> &ObjectType {
        &self.value
    }

    /// Obtain a mutable ref to the underlying GC object
    pub fn as_obj_mut(&mut self) -> &mut ObjectType {
        &mut self.value
    }
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
    // Table for string interning
    strings: HashMap<String, GcRef<Object>>,
}

impl GcHeap {
    pub fn new() -> GcHeap {
        GcHeap {
            root: None,
            strings: HashMap::new(),
        }
    }

    /// Allocate an object on the heap and register it with the garbage collector.
    pub fn allocate(&mut self, object: ObjectType) -> GcRef<Object> {
        // String interning
        let mut s_value: Option<String> = None;
        if let ObjectType::String(s) = &object {
            if let Some(gcref) = self.strings.get(s) {
                return gcref.clone();
            }

            s_value = Some(s.clone());
        }

        let current_root = self.root.take();
        let boxed = Box::new(Object {
            next: current_root,
            value: object,
        });

        self.root = NonNull::new(Box::into_raw(boxed));

        let gcref = GcRef {
            ptr: self.root.unwrap().as_ptr(),
        };

        if let Some(s) = s_value {
            self.strings.insert(s, gcref.clone());
        };

        gcref
    }
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        let mut current = self.root;
        while let Some(obj) = current {
            unsafe {
                current = obj.as_ref().next;
                obj.drop_in_place();
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn it_allocates() {
        let mut heap = GcHeap::new();

        let obj1 = ObjectType::String("foo".to_string());

        let ref1 = heap.allocate(obj1);

        assert_eq!(ref1.value, ObjectType::String("foo".to_string()));
    }

    #[test]
    fn it_interns_identical_strings() {
        let mut heap = GcHeap::new();

        let obj1 = ObjectType::String("foo".to_string());
        let obj2 = ObjectType::String("foo".to_string());

        let gcref1 = heap.allocate(obj1);
        let gcref2 = heap.allocate(obj2);

        assert_eq!(gcref1.ptr, gcref2.ptr)
    }
}
