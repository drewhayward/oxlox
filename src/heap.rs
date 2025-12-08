use std::cell::RefCell;
use std::{
    cell::{Ref, RefMut},
    collections::HashMap,
    ptr::NonNull,
};

use crate::object::Object;

/// A node in the linked-list of the [GcHeap]. Owns both the value in the entry and the next value
/// in the linked-list
///
/// See [GcHeap] for more information
#[derive(Debug)]
struct GcEntry {
    // TODO: Do we need this to not be owned?
    next: Option<NonNull<GcEntry>>,
    value: RefCell<Object>,
}

/// A smart-pointer to a garbage-collected lox [Object]
///
/// Uses RefCell to safely manage access, [GcHeap] is responsible for dropping the underlying
/// object
#[derive(Debug)]
pub struct Gc {
    entry: NonNull<GcEntry>,
}

impl Gc {
    /// Obtain a [Ref] to the underlying GC object
    pub fn borrow(&self) -> Ref<'_, Object> {
        // SAFETY
        // GcHeap enforces that an entry will remain valid until all reachable Gc refs are gone
        let entry = unsafe { &*NonNull::as_ptr(self.entry) };

        entry.value.borrow()
    }

    /// Obtain a [RefMut] to the underlying GC object
    pub fn borrow_mut<'a>(&'a self) -> RefMut<'a, Object> {
        // SAFETY
        // GcHeap enforces that an entry will remain valid until all reachable Gc refs are gone
        let entry = unsafe { &*NonNull::as_ptr(self.entry) };

        entry.value.borrow_mut()
    }
}

impl Clone for Gc {
    fn clone(&self) -> Self {
        Gc { entry: self.entry }
    }
}

/// Lox runtime heap which implements garbage collection using mark-and-sweep.
///
/// Objects are allocated by [GcHeap::allocate] which returns a [Gc] smart pointer.
/// Access is mediated via [Gc::borrow] and [Gc::borrow_mut] which return
/// [Ref<Object>]/[RefMut<Object>] respectively.
#[derive(Debug)]
pub struct GcHeap {
    /// Root to a linked list of garbage-collected values
    root: Option<NonNull<GcEntry>>,

    /// Table for string interning
    strings: HashMap<String, Gc>,
}

impl GcHeap {
    pub fn new() -> GcHeap {
        GcHeap {
            root: None,
            strings: HashMap::new(),
        }
    }

    /// Allocate an object on the heap and register it with the garbage collector.
    pub fn allocate(&mut self, object: Object) -> Gc {
        // Create new head of list
        let new_entry = Box::new(GcEntry {
            next: self.root.take(),
            value: RefCell::new(object),
        });

        // SAFETY
        // We can safely make an unchecked [NonNull] because Box won't give us a null pointer
        self.root = unsafe { Some(NonNull::new_unchecked(Box::into_raw(new_entry))) };

        let gcref = Gc {
            entry: self.root.unwrap(),
        };

        gcref
    }

    // TODO: Implement mark & sweep
}

impl Drop for GcHeap {
    fn drop(&mut self) {
        let mut current = self.root;
        while let Some(obj) = current {
            // SAFETY
            // This ptr was allocated by Box for type GcEntry and has not been freed by a GC
            // collection. We are now transferring responsibility of dropping to this local Box
            let mut entry = unsafe { Box::from_raw(NonNull::as_ptr(obj)) };

            current = entry.next.take();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn shares_memory() {
        let mut heap = GcHeap::new();

        let ref1 = heap.allocate(Object::String("foo".to_string()));
        let ref2 = ref1.clone();

        {
            let mut a = ref2.borrow_mut();
            match *a {
                Object::String(ref mut s) => *s = "bar".to_string(),
            }
        }

        let b = ref1.borrow();
        match *b {
            Object::String(ref s) => assert_eq!(*s, "bar".to_string()),
        }
    }

    #[test]
    #[should_panic]
    fn panics_on_double_mut() {
        let mut heap = GcHeap::new();

        let obj1 = Object::String("foo".to_string());

        let ref1 = heap.allocate(obj1);
        let ref2 = ref1.clone();

        let a = ref1.borrow_mut();
        let b = ref2.borrow_mut();

        println!("{:?} {:?}", a, b);
    }
}
