/* Complex numbers module.
 * 
 * Purpose : Provides a generic complex-number type and operations.
 * Author  : Maxwell Powlison (bobdavelisafrank@protonmail.com)
 */
use std::ops::{Add, Sub, Neg, Mul, Div};



// Complex numbers type.
#[derive(Copy, Clone, PartialEq, Debug)]
pub struct Complex<T> {
        real: T,
        imag: T,
}



// Generic complex operations.
impl <T: Copy> Complex<T> {
        
        // Builds a complex number from a real and imaginary part.
        pub fn pair (real: T, imag: T) -> Complex<T>
        {
                return Complex {real: real, imag: imag};
        }

        pub fn new (real: T, imag: T) -> Complex<T>
        {
                return Complex::pair (real, imag);
        }

        // Gets the real part of the current complex number.
        pub fn real (&self) -> T
        {
                return self.real;
        }

        // Gets the imaginary part of the current complex number.
        pub fn imag (&self) -> T
        {
                return self.imag;
        }

        // Sets the real part of the current complex number.
        pub fn set_real (&mut self, val: T)
        {
                self.real = val;
        }

        // Sets the imaginary part of the current complex number.
        pub fn set_imag (&mut self, val: T)
        {
                self.imag = val;
        }
}



/* Implements more complex operations of complex numbers (those requiring
 * standard field operations).
 */
impl <T:
      Add<Output=T> +
      Sub<Output=T> +
      Neg<Output=T> +
      Mul<Output=T> +
      Div<Output=T> +
      Copy>
        Complex<T>
{
        // Returns the square-magnitude of the current complex number.
        pub fn mag_square (&self) -> T
        {
                return self.real * self.real + self.imag * self.imag;
        }
        
        // Returns the conjugate of the current complex number.
        pub fn conj (&self) -> Complex<T>
        {
                return Complex::pair (
                        self.real,
                        -self.imag,
                )
        }

        // Squares the current complex number.
        pub fn square (&self) -> Complex<T>
        {
                return Complex::pair (
                        self.real * self.real - self.imag * self.imag,
                        self.real * self.imag + self.real * self.imag,
                )
        }
        
        // Takes the reciprocal of the current complex number.
        pub fn reciproc (&self) -> Complex<T>
        {
                let divisor = self.mag_square();
                
                return Complex::pair (
                        self.real / divisor,
                        -self.imag / divisor,
                )
        }
}



// Complex addition.
impl <T: Add<Output=T> + Copy> Add for Complex<T> {

        // Addition of complex numbers forms a complex number.
        type Output = Complex<T>;
        
        // Adds the current complex number to another.
        fn add (self, z: Complex<T>) -> Self::Output
        {
                return Complex::pair (
                        self.real + z.real,
                        self.imag + z.imag,
                )
        }
}



// Complex subtraction.
impl <T: Sub<Output=T> + Copy> Sub for Complex<T> {

        // Subtraction of complex numbers forms a complex number.
        type Output = Complex<T>;
        
        // Subtracts another complex number.
        fn sub (self, z: Complex<T>) -> Self::Output
        {
                return Complex::pair (
                        self.real - z.real,
                        self.imag - z.imag,
                )
        }
}



// Complex multiplication.
impl <T:
      Add<Output=T> +
      Sub<Output=T> +
      Mul<Output=T> +
      Copy>
        Mul for Complex<T>
{
        
        // Multiplication of complex numbers forms complex numbers.
        type Output = Complex<T>;
        
        // Multiplies the current complex number by another.
        fn mul (self, z: Complex<T>) -> Self::Output
        {
                return Complex::pair (
                        self.real * z.real - self.imag * z.imag,
                        self.real * z.imag + self.imag * z.real,
                )
        }
}



// Complex division.
impl <T:
      Add<Output=T> + 
      Sub<Output=T> + 
      Neg<Output=T> + 
      Mul<Output=T> + 
      Div<Output=T> +
      Copy>
        Div for Complex<T>
{
        // Division of complex numbers results in complex numbers.
        type Output = Complex<T>;
        
        // Divides the current complex number by another.
        fn div (self, z: Complex<T>) -> Self::Output
        {
                let divisor = z.mag_square();
                
                return Complex::pair (
                        (self.real * z.real + self.imag * z.imag) / divisor,
                        (self.imag * z.real - self.real * z.imag) / divisor,
                )
        }
}



#[cfg(test)]
/** Testing Section
 *
 * Don't read this section if you hate unfactored code. It will kill you if
 * you do, especially since it has up to 7 levels of indentation.
 *
 * TODO: Rewrite this section to make it good.
 */
mod tests {
        #[test]
        // Complex data construction/retrieval test.
        fn cplx_data()
        {
                // O(n^2), n = |lower| + |upper|.
                let lower_bound = -512;
                let upper_bound =  512;
                
                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<isize> =
                                        super::Complex::new (
                                                x,
                                                y,
                                        );

                                assert! (z.real() == x);
                                assert! (z.imag() == y);
                        }
                }
        }
        
        #[test]
        // Conjugation test.
        fn cplx_conj()
        {
                // O(n^2), n = |lower| + |upper|.
                let lower_bound = -512;
                let upper_bound =  512;

                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<isize> =
                                        super::Complex::new (
                                                x,
                                                y,
                                        );

                                let conj = z.conj();

                                assert_eq! (z.real(),  conj.real());
                                assert_eq! (z.imag(), -conj.imag());
                        }
                }
        }
        
        #[test]
        // Reciprication test.
        fn cplx_rprc()
        {
                // O(n^2), n = |lower| + |upper|.
                let lower_bound = -512;
                let upper_bound =  512;

                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<f64> =
                                        super::Complex::new (
                                                x as f64,
                                                y as f64,
                                        );

                                // Takes the conjugate twice, which should
                                // produce the original value (to some
                                // margin of error).
                                if z.mag_square() == 0.0 {
                                        continue;
                                }

                                let rec = z.reciproc();

                                // Not impossible for this to happen due to
                                // rounding error...
                                if rec.mag_square() == 0.0 {
                                        continue;
                                }
                                
                                let drec = rec.reciproc();

                                // Rounds out the result and compares.
                                let z_round = super::Complex::new (
                                        z.real().round(),
                                        z.imag().round(),
                                );

                                let drec_round = super::Complex::new (
                                        drec.real().round(),
                                        drec.imag().round(),
                                );
                                
                                assert_eq! (z_round, drec_round);
                        }
                }
        }
        
        #[test]
        // Magnitude test.
        fn cplx_mags()
        {
                // O(n^2), n = |lower| + |upper|.
                let lower_bound = -512;
                let upper_bound =  512;

                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<isize> =
                                        super::Complex::new (
                                                x,
                                                y,
                                        );

                                assert_eq! (z.mag_square(), x*x + y*y);
                        }
                }
        }

        #[test]
        // Squaring test. Depends on multiplication.
        fn cplx_sqre_mul()
        {
                // O(n^2), n = |lower| + |upper|.
                let lower_bound = -512;
                let upper_bound =  512;

                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<isize> =
                                        super::Complex::new (
                                                x,
                                                y,
                                        );

                                let square0 = z.square();
                                let square1 = z * z;

                                assert_eq! (square0, square1);
                        }
                }
        }
        
        #[test]
        // Addition test.
        fn cplx_add()
        {
                // O(n^4), n = |lower| + |upper|. Very quick growth.
                let lower_bound = -16;
                let upper_bound =  16;

                for y1 in lower_bound..upper_bound {
                        for x1 in lower_bound..upper_bound {
                                for y2 in lower_bound..upper_bound {
                                        for x2 in lower_bound..upper_bound {
                                                let z1 = super::Complex::new (
                                                        x1,
                                                        y1,
                                                );
                                                
                                                let z2 = super::Complex::new (
                                                        x2,
                                                        y2,
                                                );
                                                
                                                let sum = z1 + z2;
                                                
                                                let real = x1 + x2;
                                                let imag = y1 + y2;
                                                
                                                assert_eq! (sum.real(), real);
                                                assert_eq! (sum.imag(), imag);
                                        }
                                }
                        }
                }
        }
        
        #[test]
        // Subtraction test.
        fn cplx_sub()
        {
                // O(n^2), n = |lower| + |upper|. Very quick growth.
                let lower_bound = -16;
                let upper_bound =  16;

                for y1 in lower_bound..upper_bound {
                        for x1 in lower_bound..upper_bound {
                                for y2 in lower_bound..upper_bound {
                                        for x2 in lower_bound..upper_bound {
                                                let z1 = super::Complex::new (
                                                        x1,
                                                        y1,
                                                );
                                                
                                                let z2 = super::Complex::new (
                                                        x2,
                                                        y2,
                                                );
                                                
                                                let diff = z1 - z2;
                                                
                                                let real = x1 - x2;
                                                let imag = y1 - y2;
                                                
                                                assert_eq! (diff.real(), real);
                                                assert_eq! (diff.imag(), imag);
                                        }
                                }
                        }
                }
        }

        #[test]
        // Combined addition/subtraction test.
        fn cplx_add_sub()
        {
                let lower_bound = -512;
                let upper_bound =  512;

                for y in lower_bound..upper_bound {
                        for x in lower_bound..upper_bound {
                                let z: super::Complex<isize> =
                                        super::Complex::new (
                                                x,
                                                y,
                                        );

                                let sum     = z + z;
                                let sumdiff = sum - z;

                                assert_eq! (z, sumdiff);
                        }
                }
        }
        
        #[test]
        // Multiplication test.
        fn cplx_mul()
        {
                // O(n^2), n = |lower| + |upper|. Very quick growth.
                let lower_bound = -16;
                let upper_bound =  16;

                for y1 in lower_bound..upper_bound {
                        for x1 in lower_bound..upper_bound {
                                for y2 in lower_bound..upper_bound {
                                        for x2 in lower_bound..upper_bound {
                                                let z1 = super::Complex::new (
                                                        x1,
                                                        y1,
                                                );
                                                
                                                let z2 = super::Complex::new (
                                                        x2,
                                                        y2,
                                                );
                                                
                                                let prod = z1 * z2;
                                                
                                                let a = x1;
                                                let b = y1;
                                                
                                                let c = x2;
                                                let d = y2;
                                                
                                                let real = a*c - b*d;
                                                let imag = b*c + a*d;
                                                
                                                assert_eq! (prod.real(), real);
                                                assert_eq! (prod.imag(), imag);
                                        }
                                }
                        }
                }
        }
        
        #[test]
        // Division test.
        fn cplx_div()
        {
                // O(n^2), n = |lower| + |upper|. Very quick growth.
                let lower_bound: isize = -16;
                let upper_bound: isize =  16;

                for y1 in lower_bound..upper_bound {
                        for x1 in lower_bound..upper_bound {
                                for y2 in lower_bound..upper_bound {
                                        for x2 in lower_bound..upper_bound {
                                                let a = x1 as f64;
                                                let b = y1 as f64;
                                                
                                                let c = x2 as f64;
                                                let d = y2 as f64;
                                                
                                                let z1: super::Complex<f64> =
                                                        super::Complex::new (
                                                                a,
                                                                b,
                                                        );
                                                
                                                let z2: super::Complex<f64> =
                                                        super::Complex::new (
                                                                c,
                                                                d,
                                                        );
                                                
                                                if z2.mag_square() == 0.0 {
                                                        continue;
                                                }

                                                let div = z1 / z2;
                                                
                                                let divisor = c*c + d*d;
                                                let real = (a*c + b*d)/divisor;
                                                
                                                let imag = (b*c - a*d)/divisor;
                                                
                                                assert_eq! (
                                                        div.real().round(),
                                                        real.round()
                                                );
                                                
                                                assert_eq! (
                                                        div.imag().round(),
                                                        imag.round()
                                                );
                                        }
                                }
                        }
                }
        }
}
