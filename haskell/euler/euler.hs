-- Euler's method of approximating ODEs.

{-- _Y
 -
 - Uses Euler's method to approximate the solution to a system of 2 ODEs.
 -
 - Inputs:
 -   t  :
 -     The final t-value you want to approximate.
 -
 -   dt :
 -     The step-size you want to take to get to the final t-value.
 -
 -   dx :
 -     The known derivative of x with respect to t, written as a function of t,
 -     x, and y (with arguments in that order).
 -
 -   dy :
 -     The known derivative of y with respect to t, written as a function of t,
 -     x, and y (with arguments in that order).
 -
 -   (t0, x0, y0) :
 -     The initial values for the equation.
 -
 - Return:
 -   The new values (tN, xN, yN) corresponding to the given t-point.
 -}
_Y ::
  Double                                 ->
  Double                                 ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double, Double, Double)               ->
  (Double, Double, Double)
_Y t dt dx dy (t0, x0, y0) = rec_Y (round (t / dt)) dt (t0, x0, y0)
  where
    rec_Y ::
      (Integral a) =>
      a                        ->
      Double                   ->
      (Double, Double, Double) ->
      (Double, Double, Double)
    rec_Y n dt (tN, xN, yN)
      | n == 0 =
        (tN, xN, yN)
      | n >  0 =
        rec_Y (n - 1) dt
          {- Euler's method is really simple, isn't it?
           -
           - If you expand a bit of it out, you get x0 + sum (dx/dt * dt), which
           - is just a generic integral approximation.
           -
           - The real problems with it are its ignorance of the second
           - derivative.
           -}
          (tN + dt
          , xN + (dx tN xN yN) * dt
          , yN + (dy tN xN yN) * dt
          )



-- Uses the second derivative to achieve an over-approximation of the result.
_Y2 ::
  Double                                 ->
  Double                                 ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double, Double, Double)               ->
  (Double, Double, Double)
_Y2 t dt dx dy d2x d2y (t0, x0, y0) = rec_Y (round (t / dt)) dt (t0, x0, y0)
  where
    rec_Y ::
      (Integral a) =>
      a                        ->
      Double                   ->
      (Double, Double, Double) ->
      (Double, Double, Double)
    rec_Y n dt (tN, xN, yN)
      | n == 0 =
        (tN, xN, yN)
      | n >  0 =
        rec_Y (n - 1) dt
          (tN + dt
          , xN + (dx tN xN yN) * dt + (d2x tN xN yN) * (dt * dt)
          , yN + (dy tN xN yN) * dt + (d2y tN xN yN) * (dt * dt)
          )

-- Uses an average of the under-approx that is the vanilla method and the
-- over-approx of the modified method to try and achieve an even better result.
_Y3 ::
  Double                                 ->
  Double                                 ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double -> Double -> Double -> Double) ->
  (Double, Double, Double)               ->
  (Double, Double, Double)
_Y3 t dt dx dy d2x d2y (t0, x0, y0) = rec_Y (round (t / dt)) dt (t0, x0, y0)
  where
    rec_Y ::
      (Integral a) =>
      a                        ->
      Double                   ->
      (Double, Double, Double) ->
      (Double, Double, Double)
    rec_Y n dt (tN, xN, yN)
      | n == 0 =
        (tN, xN, yN)
      | n >  0 =
        rec_Y (n - 1) dt
          (tN + dt
          , xN + (dx tN xN yN) * dt + 0.5 * ((d2x tN xN yN) * (dt * dt))
          , yN + (dy tN xN yN) * dt + 0.5 * ((d2y tN xN yN) * (dt * dt))
          )
