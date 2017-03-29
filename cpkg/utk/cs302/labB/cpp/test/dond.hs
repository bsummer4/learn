-- Recursive version in haskell :).  I belive a memoized version would
-- be about the same length using an infinte list (lazy evaluation)

adj x y = abs(x - y) <= 1

avg l len = sum l / (fromIntegral len :: Float)

dond s 0 l    = 1.0
dond s t (-1) = avg [(dond s (t - 1) x) | x <- (range s)] s
dond s t l    = avg [(dond s (t - 1) x) | x <- (range s), not (adj x l)] s
