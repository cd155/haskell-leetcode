module WeekFour where

{-
    34. Course Schedule

    Given a list of class you have to take, and prerequisites. Check wether you 
    can finish all courses.

    The same question in 
    https://github.com/cd155/algorithm-design-with-haskell/blob/71661bb4fa30663da215351bfaf55cd768952af7/src/Graph.hs#L64
-}
-- use number to represent class
type Class = Int
type Prereq = (Class,Class)
