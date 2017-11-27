// A simple function which prints out all triangular-square numbers less than
// 2^64.
fn main ()
{
    // previous -> Triangular-square number previous in the sequence.
    // current  -> Current triangular-square number in the sequence.
    // tmp      -> Used for calculating the next triangular-square.
    // maximum  -> Used as an approximate maximum bound for triangular-squares,
    //             in order to prevent overflow.
    let mut previous : u64 = 0;
    let mut current  : u64 = 1;
    let mut tmp      : u64;
    let     maximum  : u64 = 0xFFFF_FFFF_FFFF_FFFF / 34;

    loop {
        // Prints the current triangular-square.
        println ! ("A square-triangular number is {}.", current);

        // Checks if overflow will occur.
        if current > maximum {
            return;
        }

        // Calculates the next triangular-square, makes it the current, and
        // updates the previous.
        tmp      = 34 * current - previous + 2;
        previous = current;
        current  = tmp;
    }
}
