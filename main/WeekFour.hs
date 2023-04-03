module WeekFour where

{-
  34. Course Schedule

  Given a list of class you have to take, and prerequisites.

  1. Check wether you can finish all courses.
  2. List a path how to finish all course.

  The same question in
  https://github.com/cd155/algorithm-design-with-haskell/blob/71661bb4fa30663da215351bfaf55cd768952af7/src/Graph.hs#L64

  Test Cases:

  canFinishAll classes prerequisites -> True

  roadMapOf classes prerequisites -> "efabdc"
-}

classes = ['a', 'b', 'c', 'd', 'e', 'f']

prerequisites = [('a', 'd'), ('f', 'b'), ('b', 'd'), ('f', 'a'), ('d', 'c')]

-- use number to represent class
type Class = Char

type Prereq = (Class, Class)

type Graph = ([Class], [Prereq])

-- visit all node in the graph
depthFirst :: Graph -> [Class]
depthFirst = error "Not Implement"