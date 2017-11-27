// A solution to http://www.techiedelight.com/find-pair-with-given-sum-array/.
use std::env;



// Gets the list and sum-value from the command-line.
fn parse_args() -> (Vec<isize>, isize)
{
        let mut args: Vec<String> = env::args().collect();

        if args.len() < 3 {
                panic! ("Error: Too few arguments!");
        }

        // Don't need name of executable.
        args.remove(0);

        let index_last = args.len() - 1;

        let sum = args[index_last]
                .parse()
                .expect ("Error: Could not parse integer (sum).");

        args.remove(index_last);

        let list: Vec<isize> = args.into_iter().map (
                |string| string
                        .parse()
                        .expect ("Error: Could not parse integer (list)")
        )
                .collect();

        (list, sum)
}



/* Finds common elements between two sorted lists.
 *
 * The lists being sorted is an optimization that allows us to find the
 * intersections with only one pass of each list, in linear O(n + m) time.
 *
 * With two sorted lists x and y, if we find a value in y that is greater than
 * or equal to a value in x, then all the following values in x have no
 * intersection with any of the previous values in y (and vice versa).
 *
 * Or, for another way to think of it, if y is sorted, then we only have to
 * search for an intersection with a value from x up until the point where we
 * find a value in y which is greater than or equal to x. But, if x is
 * sorted, then when we start checking the next x value we can continue the
 * search in y where we left off (since the next x value is greater than or
 * equal to the previous, so all the y values passed up can't be equal to
 * it).
 */
fn intersect_sorted<T: Copy + Ord + std::fmt::Debug> (
        list_x: &Vec<T>,
        list_y: &Vec<T>
) -> Vec<T>
{
        let mut common = Vec::new();
        let mut index: usize = 0;
        let y_len = list_y.len();

        'next_x: for x in list_x {
                
                let x_deref = *x;

                loop {
                        if index >= y_len {
                                break 'next_x;
                        }
                        
                        let y_deref = list_y[index];
                        
                        if x_deref == y_deref {
                                common.push(x_deref);
                                continue 'next_x;
                                
                        } else if x_deref < y_deref {
                                continue 'next_x;
                                
                        } else {
                                index += 1;
                        }
                }
        }

        common
}



/* Finds pairs of elements from a list which have a desired sum.
 *
 * We take the list of elements, and we generate a list of elements which would
 * have to be in the list for pairs to exist (in this case, by copying each
 * element, then subtracting it from the desired sum). Then, we just find
 * which of those elements are in the original list.
 */
fn sum_pair (mut list: Vec<isize>, sum: isize) -> Vec<(isize, isize)>
{
        // Part of intersection algorithm. Also allows us to cut the searched
        // list in half (due to symmetry of pairs).
        list.sort();
        
        let mut subbed = Vec::new();

        for i in (0..list.len()).rev() {
                subbed.push (sum - list[i]);
        }

        // `+1` to capture a value that, when added to itself, gets the sum.
        let half_len = list.len() / 2 + 1;
        list.truncate(half_len);
        subbed.truncate(half_len);

        intersect_sorted (&subbed, &list)
                .into_iter()
                .map (|x| (x, sum - x))
                .collect()
}



fn main()
{
        let (list, sum) = parse_args();

        print! ("List  : {:?}\n", list);
        print! ("Sum   : {}\n", sum);

        let pairs = sum_pair (list, sum);

        print! ("Pairs : {:?}\n", pairs);
}



/* On another note with the intersection algorithm, I find that searching for
 * one does have a lot of mention of hash-tables; that one puts each value from
 * the x-array into a hash table, then one can perform searches of best-case
 * O(1) time on elements in the y-array, resulting also in O(n + m) calculation
 * time.
 *
 * The advantage of the algorithm we have in place here is that it can match
 * the same O(n + m) calculation time, at the cost of sorting the lists (which
 * can often be a heavy cost, adding n*log(n) + m*log(m)).
 *
 *   But...
 * If the first list is sorted, then our second list is ordered as well (since
 * addition to a value preserves order, and subtraction from a value reverses 
 * it), eliminiating the second sort. Half of the output of our intersection
 * ends up being the first part of each pair, and the other half ends up being
 * the second part for each pair -- and in the case for our intersection, which
 * has ordered output for ordered input, we have a clean separation of these
 * values. Combine this with the fact that we can calculate the other half of
 * the pair value very easily, and we can just throw away half of the
 * intersection output. But since our intersection algorithm has ordered
 * output for ordered input, we can get rid of the larger half of output by
 * getting rid of the larger half of input, letting us cut the input lists to
 * the intersection in half.
 */
