\documentclass[a4paper,12pt]{article}
\usepackage{minted}
\title{Assignment 1}
\author{Rory Stephenson 300160212 stepherory}
\date{5 March 2014}
\begin{document}
\newminted[code]{haskell}{linenos}
\maketitle

\section*{Module Setup}
\begin{code}
module Helper where
\end{code}

\section*{Core}
\texttt{myContains} accepts an element and a list of the same type and returns
\texttt{True} if the element is in the list and False otherwise. The type of the element 
and list must implement \texttt{Eq}.

\begin{code}
myContains :: Eq a => [a] -> a -> Bool
myContains x y = foldr (\ x0 y0 -> y0 || (x0 == y)) False x
\end{code}

\noindent \texttt{mySum} accepts a list of elements (which must be a subtype of \texttt{Num}) and
returns the sum of all the elements in the list using foldr.

\begin{code}
mySum :: Num a => [a] -> a
mySum x = foldr (+) 0 x
\end{code}

\noindent \texttt{mySort} accepts a list of elements which are a subtype of \texttt{Ord} and returns
that list in ascending order. It uses recursive mergesort to achieve the sorting.

\begin{code}
mySort :: Ord a => [a] -> [a]
mySort [] = []
mySort [a] = [a]
mySort xs = merge (mySort xs1) (mySort xs2)
            where 
            	(xs1, xs2) = splitAt (quot (length xs) 2) xs

merge :: Ord a => [a] -> [a] -> [a]
merge [] y = y
merge x [] = x
merge (x:xrest) (y:yrest) | x <= y = x: merge xrest (y:yrest)
merge (x:xrest) (y:yrest) | otherwise = y: merge (x:xrest) yrest
\end{code}

\noindent \texttt{myFilter} accepts a function and a list and returns a new list containing
all of the elements which evaluate to \texttt{True} when the provided function is
applied to them (order is maintained). The provided function must accept an element of the provided
list and return a \texttt{Bool}. \texttt{evaluateElem} is a helper function which takes a 
function, an element and a list and appends the element to the list if the function returns
true for the given element.

\begin{code}
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f list = foldr (evaluateElem f) [] list

evaluateElem :: (a -> Bool) -> a -> [a] -> [a]
evaluateElem f e list | f e == True = e:list
                      | otherwise = list
\end{code}

\noindent \texttt{myLast} returns the last element of a list. If the provided list is empty
it will throw an "Empty List" exception.

\begin{code}
myLast :: [a] -> a
myLast [] = error "Empty List"
myLast x = x !! ((length x) - 1)
\end{code}

\noindent \texttt{myUnzip} accepts a list of pairs and returns a pair of lists such that the
first element of every pair is in the first list and the second element of
every pair is in the second list. Ordering is maintained.

\begin{code}
myUnzip :: [(a, b)] -> ([a],[b])
myUnzip x = foldr (\ (a,b) (c,d) -> (a:c,b:d)) ([],[]) x
\end{code}

\section*{Completion}

\texttt{powerSet} accepts a list and returns a list which contains the powerset of
the provided list (every possible combination of the provided elements plus empty list).
It (intentionally) does not remove duplicate elements and therefore does not return an
actual set.

\begin{code}
powerSet :: [a] -> [[a]]
powerSet [] = [[]]
powerSet (x:xs) = powerSet xs ++ map (x:) (powerSet xs)
\end{code}

\noindent \texttt{scrabblify} accepts a string and returns an Int which is the score that the provided
string would earn if played in scrabble (without bonus tiles). \texttt{scrabLetter} is a helper
function which maps a \texttt{Char} to its scrabble value

\begin{code}
scrabblify :: String -> Int
scrabblify x = mySum $ map scrabLetter x

scrabLetter :: Char -> Int
scrabLetter x | myContains "aAeEiIlLnNoOrRsStTuU" x = 1
              | myContains "dDgG" x = 2
              | myContains "bBcCmMpP" x = 3
              | myContains "fFhHvVwWyY" x = 4
              | myContains "kK" x = 5
              | myContains "jJxX" x = 8
              | myContains "qQzZ" x = 10
              | otherwise = 0
\end{code}

\noindent \texttt{sortBy} sorts a list with an ordering determined by the provided function. The provided function
must accept two elements of the list given and return an \texttt{Ordering}. \texttt{mergeComp} is a helper function
which merges two two lists according to the \texttt{ordering} specified by the provided function.

\begin{code}
sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy _ [a] = [a]
sortBy f xs = mergeComp f (sortBy f xs1) (sortBy f xs2)
            where 
            	(xs1, xs2) = splitAt (quot (length xs) 2) xs

mergeComp :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeComp _ [] y = y
mergeComp _ x [] = x
mergeComp f (x:xrest) (y:yrest) | f x y == LT = 
  x: mergeComp f xrest (y:yrest)
mergeComp f (x:xrest) (y:yrest) | otherwise = 
  y: mergeComp f (x:xrest) yrest
\end{code}

\noindent \texttt{intersperse} takes an element and a list of the same type and returns a list
where the provided element is inserted between each of the original elements. 
If the list is empty or only has one element this means the returned list will
be the same as the given list.

\begin{code}
intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse x (y:ys) = y: foldr (\ a b -> x:a:b) [] ys
\end{code}
\end{document}