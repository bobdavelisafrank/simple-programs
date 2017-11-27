/* FIFO data structures.
 *
 * Purpose : Gives one access to FIFO data structures like Circular buffers.
 * Author  : Maxwell Powlison (bobdavelisafrank@protonmail.com)
 */
use std::vec;



// Underlying data structure.
#[derive(Clone)]
pub struct Circular<T> {
        dat: Vec<Option<T>>,
        index: usize,
}



// For iteration.
pub struct CircularIter<'a, T:'a> {
        buf: &'a Circular<T>,
        iter_index: Option<usize>, /* Weirdness due to circularity of buf. */
}



// Functions for using the buffer.
impl <T: Copy> Circular<T> {
        // Creates a new circular buffer.
        pub fn new() -> Circular<T>
        {
                Circular {
                        dat: Vec::new(),
                        index: 0,
                }
        }

        // Creates a new circular buffer, of a given size.
        pub fn create(amount: usize) -> Circular<T>
        {
                let mut buf: Circular<T> = Circular::new();
                buf.expand (amount);

                return buf;
        }

        // Returns the length of the buffer.
        pub fn len(&self) -> usize
        {
                self.dat.len()
        }
        
        // Expands the buffer by a given number of elements.
        pub fn expand (&mut self, amount: usize)
        {
                for _ in 0..amount {
                        // Pushes the value onto the vector beneath the fifo,
                        // increasing the fifo's capacity.
                        self.dat.push (None);
                }
        }
        
        // Replaces the oldest element in the buffer.
        pub fn push (&mut self, elem: T)
        {
                self.dat[self.index] = Some (elem);
                
                self.index = (self.index + 1) % self.len();
        }
        
        // Gets the Nth element of the buffer.
        pub fn get (&self, index: usize) -> Option<T>
        {
                // Takes the modulo of the index.
                let index_mod = index % self.len();
                
                // Uses the new index to return an element.
                return self.dat[index_mod];
        }
}



// Implements the circular buffer as an iterator.
impl <'a, T: Copy> IntoIterator for &'a Circular<T> {

        type Item = T;
        type IntoIter = CircularIter<'a, T>;

        fn into_iter (self) -> Self::IntoIter
        {
                CircularIter {
                        buf: &self,
                        iter_index: None
                }
        }
}



// Iterator functions.
impl <'a, T: Copy> CircularIter<'a, T> {

        // Gets the next item in the buffer.
        fn internal_next (&mut self) -> Option<T>
        {
                // Checks for an iteration index, to know if we are starting,
                // ending, or continuing an iteration.
                let val_index = match self.iter_index {
                        Some (index) => {
                                // self.index is the oldest value in the array,
                                // and the edge case for the first value is
                                // handled below. This means that if we have
                                // looped back to self.index, we have iterated
                                // through the complete buffer.
                                if index == self.buf.index {
                                        // Resets the iter_index and returns.
                                        self.iter_index = None;
                                        return None;
                                }

                                index
                        },

                        None => {
                                // If self.iter_index has not been set up yet,
                                // then we are starting iteration.
                                self.iter_index = Some (self.buf.index);
                                
                                self.buf.index
                        },
                };

                // Gets the next value and increments the index.
                let val = self.buf.get (val_index);
                self.iter_index = Some ((val_index + 1) % self.buf.len());

                // Since the values are stored as optionals in the first place,
                // we just return the result of `self.get`.
                return val;
        }

        // Returns the number of elements remaining.
        fn internal_size_hint (&self) -> (usize, Option<usize>)
        {
                let val_index = match self.iter_index {
                        Some (index) =>
                                index,

                        None => {
                                return (0, None);
                        }
                };
                
                let val = if val_index < self.buf.index {
                        self.buf.index - val_index
                        
                } else {
                        self.buf.index + self.buf.len() - val_index
                };

                // Since the type stored in the array is itself an option, we
                // cannot be sure of the lower limit of how many items are
                // remaining without checking manually.
                return (0, Some (val));
        }
}



impl <'a, T: Copy> Iterator for CircularIter<'a, T> {
        
        // Item that the circular buffer contains matches the buffer's
        // implemented type.
        type Item = T;

        // Wraps the internal next function.
        fn next (&mut self) -> Option<Self::Item>
        {
                return self.internal_next();
        }

        // Wraps the internal size_hint function.
        fn size_hint (&self) -> (usize, Option<usize>)
        {
                return self.internal_size_hint();
        }
}



#[test]
/** test-overwrite
 *
 * Tests that the `push` command is properly modular.
 */
fn overwrite()
{
        // Creates a circular buffer and adds several elements
        // to it.
        let mut x_buf: Circular<u32> = Circular::new();
        x_buf.expand(3);
        
        x_buf.push(42);
        x_buf.push(1729);
        
        // Checks that the elements made it onto the buffer.
        assert! (x_buf.get(0) == Some(42));
        assert! (x_buf.get(1) == Some(1729));
        assert! (x_buf.get(2) == None);
        
        // Pushes two more elements onto the buffer, causing
        // an overwrite to trigger.
        x_buf.push(8675309);
        x_buf.push(6026961);
        
        // Checks that the elements made it onto the buffer in
        // the correct order.
        assert! (x_buf.get(0) == Some(6026961));
        assert! (x_buf.get(1) == Some(1729));
        assert! (x_buf.get(2) == Some(8675309));
}



#[test]
fn expand()
{
        // Creates a circular buffer 3 elements in size.
        let mut x_buf: Circular<u32> = Circular::new();
        x_buf.expand (3);
        
        // Pushes 2 elements onto the buffer.
        x_buf.push(42);
        x_buf.push(1729);
        
        // Checks that the elements made it onto the buffer.
        assert! (x_buf.get(0) == Some(42));
        assert! (x_buf.get(1) == Some(1729));
        assert! (x_buf.get(2) == None);
        
        // Expands the buffer size by 2.
        x_buf.expand (2);
        
        // Pushes three more elements onto the buffer.
        x_buf.push(8675309);
        x_buf.push(6026961);
        x_buf.push(0xCAFE_BABE);
        
        // Checks that the elements made it onto the buffer in
        // the correct order.
        assert! (x_buf.get(0) == Some(42));
        assert! (x_buf.get(1) == Some(1729));
        assert! (x_buf.get(2) == Some(8675309));
        assert! (x_buf.get(3) == Some(6026961));
        assert! (x_buf.get(4) == Some(0xCAFE_BABE));
}



#[test]
fn modular_index()
{
        // Creates a circular buffer 3 elements in size.
        let mut x_buf: Circular<u32> = Circular::new();
        x_buf.expand (3);
        
        // Pushes 2 elements onto the buffer.
        x_buf.push(42);
        x_buf.push(1729);
        
        // Checks that the elements can be accessed by
        // a corresponding modular index.
        assert! (x_buf.get(3) == Some(42));
        assert! (x_buf.get(4) == Some(1729));
        assert! (x_buf.get(5) == None);
}


