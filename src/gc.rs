use std::rc::Rc;

use crate::vm::GcObj;

#[derive(Debug)]
struct Allocation {
    pub object: Rc<GcObj>,
    next: Option<Box<Allocation>>,
}

#[derive(Debug)]
pub struct GarbageCollector {
    // Hmm what goes here?
    /// Head of a linked list of object allocations
    root: Option<Box<Allocation>>,
}

/// Basic garbage collector implemented by cloning Rc's
/// Obviously we could just clone around references to the Rc's without a coordinating
/// GC, but this way we can emulate garbage collection.
impl GarbageCollector {
    pub fn new() -> GarbageCollector {
        GarbageCollector { root: None }
    }
    pub fn register_object(&mut self, object: GcObj) -> Rc<GcObj> {
        let current_root = self.root.take();
        let new_root = Box::new(Allocation {
            object: Rc::new(object),
            next: current_root,
        });

        self.root = Some(new_root);

        self.root.as_ref().unwrap().object.clone()
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn multiple_mut_refs() {
        let mut gc = GarbageCollector::new();

        let obj1 = gc.register_object(GcObj::String("foo".to_string()));

        {
            let _obj2 = Rc::clone(&obj1);
            assert_eq!(Rc::strong_count(&obj1), 3);
        }

        assert_eq!(Rc::strong_count(&obj1), 2);

    }
}
