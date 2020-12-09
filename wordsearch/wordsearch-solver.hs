module WordSearch where 
import Data.Maybe

type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- Returns a list of the input words matched with their Just Placement or Nothing
-- [String]                     =  list of words to try find
-- WordSearchGrid               =  the wordsearch grid of characters
-- [(String, Maybe Placement)]  =  the list of words with their placement
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch words grid = let
                                 solve [] acc = acc
                                 solve (word:ws) acc = solve ws (acc ++ [(word, searchOneWord grid word)])
                             in
                                 solve words []

-- Will search for one specific word in the wordsearch
-- WordSearchGrid   =  the wordsearch grid of characters
-- String           =  the one specific word we are searching for
-- Maybe Placement  =  the potential placement of this weird
searchOneWord :: WordSearchGrid -> String -> Maybe Placement
searchOneWord grid word = let
                              startPositions = getPositionsOfStartLetter grid (head word)
                              possibilities = getPossibilitiesForPositions grid startPositions (word !! 1) []
                              output = [searchAllDirsForOnePos grid [(spos, drop 2 word, plce) | (spos, plces) <- possibilities, plce <- plces]]
                          in
                              hasValue output

-- Given the list of all attemtps to find, if we find a Just value, return it (with r and c swapped) assuming constraint of only one valid solution, else will return Nothing for this word
-- [Maybe Placement]  =  list of all potential placements for all possibilities that are checked for one word
-- Maybe Placement    =  the potential placement we find, or Nothing
hasValue :: [Maybe Placement] -> Maybe Placement
hasValue [] = Nothing
hasValue (Nothing:ps) = hasValue ps
hasValue (p@(Just ((r, c), orient)):ps) = if isJust p then Just ((c, r), orient) else hasValue ps  

-- Get all the positions of the start letter of a word
-- WordSearchGrid   =  the wordsearch grid of characters
-- Char             =  the starting char we are searching for
-- [Posn]           =  positions in the wordsearch that equal the given Char
getPositionsOfStartLetter :: WordSearchGrid -> Char -> [Posn]
getPositionsOfStartLetter grid char = let 
                                          getPositionsOfStartLetter' [] _ acc = acc
                                          getPositionsOfStartLetter' (row:rs) rowNum acc = getPositionsOfStartLetter' rs (rowNum + 1) (acc ++ getPositionInRow row char (rowNum, 0) [])  
                                      in
                                          getPositionsOfStartLetter' grid 0 []

-- Given a row and the letter, search for positions which match the given Char on the given row
-- [Char]  =  the row of characters we are searching through
-- Char    =  the starting char we are searching for
-- Posn    =  the current position we are searching at
-- [Posn]  =  the accumulator of matching points
-- [Posn]  =  ^
getPositionInRow :: [Char] -> Char -> Posn -> [Posn] -> [Posn]
getPositionInRow [] _ _ acc = acc                                           
getPositionInRow (gridchar:cs) char (row, col) acc = if char == gridchar then getPositionInRow cs char (row, col + 1) (acc ++ [(row, col)])
                                                     else getPositionInRow cs char (row, col + 1) acc

-- Given a list of starting positions and the second character, will find all possible next possibilities to check
-- WordSearchGrid         =  the wordsearch grid of characters
-- [Posn]                 =  the list of starting points to check
-- Char                   =  the second char of the word we are searching for
-- [(Posn, [Placement])]  =  the accumulator of the starting position and list of the positions with the given Char and the Orientation from the starting position
-- [(Posn, [Placement])]  =  ^
getPossibilitiesForPositions :: WordSearchGrid -> [Posn] -> Char -> [(Posn, [Placement])] -> [(Posn, [Placement])]
getPossibilitiesForPositions _ [] _ acc = acc
getPossibilitiesForPositions grid (p:ps) char acc = getPossibilitiesForPositions grid ps char (acc ++ [(p, getPossibilitiesForOnePosition grid p char)])

-- Given a list of starting positions and the second character, will find all possible next possibilities to check
-- WordSearchGrid  =  the wordsearch grid of characters
-- Posn            =  the starting point to check
-- Char            =  the second char of the word we are searching for
-- [Placement]     =  the list of positions of given Char and the Orientation from the starting point
getPossibilitiesForOnePosition :: WordSearchGrid -> Posn -> Char -> [Placement]
getPossibilitiesForOnePosition grid pos char = getAnyDirsAndPositions grid char pos 0 []

-- Given a list of starting positions and the second character, will find all possible next possibilities to check
-- WordSearchGrid  =  the wordsearch grid of characters                                                                   0 1 2     (`div` & `mod`)    (-1,-1)  (-1, 0)  (-1, 1)
-- Char            =  the second char of the word we are searching for                                                    3 c 5     -------------->    ( 0,-1)  ( 0, 0)  ( 0, 1)
-- Posn            =  the starting point to check                                                                         6 7 8                        ( 1,-1)  ( 1, 0)  ( 1, 1)
-- Int             =  the index of which direction we will be checking as shown to the upper-right (c is the start position) (using `div` and `mod` can change to offsets we need) 
-- [Placement]     =  accumulator of positions and directions that still satisfy the input word so far
-- [Placement]     =  ^ 
getAnyDirsAndPositions :: WordSearchGrid -> Char -> Posn -> Int -> [Placement] -> [Placement]
getAnyDirsAndPositions grid char (r, c) index acc = let
                                                        rowoffset = (index `div` 3) - 1
                                                        coloffset = (index `mod` 3) - 1
                                                        -- if (rowoffset, coloffset) equals (0, 0), dir = Nothing as we do not want to check the starting square
                                                        dir = getDirectionOfOffset (rowoffset, coloffset)
                                                        charInDir = if isNothing dir then Nothing else getCharacter grid (r + rowoffset, c + coloffset)
                                                    in
                                                        -- finished searching around
                                                        if index > 8 then acc
                                                        -- if the position with offsets does not match the char, or a variable is Nothing, skip it
                                                        else if isNothing dir || isNothing charInDir || fromJust charInDir /= char then getAnyDirsAndPositions grid char (r, c) (index + 1) acc
                                                        -- if the position with offsets matches the char, add it to the accumulator
                                                        else getAnyDirsAndPositions grid char (r, c) (index + 1) (acc ++ [((r + rowoffset, c + coloffset), fromJust dir)])

-- Will continue search for all the remaining possibilities and return whether their is a valid Placement found or not
-- WordSearchGrid               =  the wordsearch grid of characters
-- [(Posn, [Char], Placement)]  =  the list consisting of terms (startingPosition, charsOfWordLeftToFind, (currentPositionWeAreAt, orientationFromStartingPosition)
-- Maybe Placement              =  the potential solution to the word we are currently searching for
searchAllDirsForOnePos :: WordSearchGrid -> [(Posn, [Char], Placement)] -> Maybe Placement
searchAllDirsForOnePos _ [] = Nothing
searchAllDirsForOnePos grid (toSearch:next) = let
                                                  searched = searchOneDirForOnePos grid toSearch 
                                              in
                                                  if isNothing searched then searchAllDirsForOnePos grid next
                                                  else searched

-- Will continue the search of one of the remaining possibilities and return whether that sequence generates a valid Placement or not
-- WordSearchGrid             =  the wordsearch grid of characters
-- (Posn, [Char], Placement)  =  tuple with the starting position, the characters left to find, and then the current position and Orientation
-- Maybe Placement            =  the potential solution to the word we are currently searching for
searchOneDirForOnePos :: WordSearchGrid -> (Posn, [Char], Placement) -> Maybe Placement                                                                       
searchOneDirForOnePos grid (origPos, trimmedWord, (currPos, orient)) = let  
                                                                           nextLetter = searchFurther grid (head trimmedWord) orient currPos
                                                                           (newr, newc) = maybe (-1, -1) fst nextLetter
                                                                       in  
                                                                           -- if letter does not match the Char we need (head of the trimmed word), return Nothing as word not here
                                                                           if isNothing nextLetter then Nothing
                                                                           -- if the letter has a placement, and we only had one character left, we have found the whole word so return the Placement
                                                                           else if length trimmedWord == 1 then Just (origPos, orient)
                                                                           -- continue searching with the next letter of trimmedWord
                                                                           else searchOneDirForOnePos grid (origPos, tail trimmedWord, ((newr, newc), orient))

-- Will find the next letter in the given possibility, and return a Placement if it matches the Char
-- WordSearchGrid   =  the wordsearch grid of characters
-- Char             =  the Char we are looking for
-- Orientation      =  the Orientation we are looking in 
-- Posn             =  the position we were just searching at
-- Maybe Placement  =  the potential placement or Nothing if the word is not this direction/position
searchFurther :: WordSearchGrid -> Char -> Orientation -> Posn -> Maybe Placement
searchFurther grid char orient (r, c) = let
                                               (rowoffset, coloffset) = getOffsetForDirection orient
                                               charInDir = getCharacter grid (r + rowoffset, c + coloffset)
                                           in
                                               if isNothing charInDir || fromJust charInDir /= char then Nothing
                                               else Just ((r + rowoffset, c + coloffset), orient)

-- Will return the Char at the given position if in-bounds, else Nothing (this is needed for letters along the borders)
-- WordSearchGrid  =  the wordsearch grid of characters
-- Posn            =  the position we are trying to find the letter
-- Maybe Char      =  if position is out-of-bounds then Nothing, else the letter itself
getCharacter :: WordSearchGrid -> Posn -> Maybe Char
getCharacter grid (r, c) = if r < 0 || c < 0 || r >= length grid || c >= length (head grid) then Nothing else Just ((grid !! r) !! c)

-- Given a direction, return the offset pair that point in that direction
-- Orientation  =  the direction we want the offsets for
-- Posn         =  the offset pair for the direction
getOffsetForDirection :: Orientation -> Posn
getOffsetForDirection dir = case dir of
                                 UpBack      -> (-1, -1)
                                 Back        -> ( 0, -1) 
                                 DownBack    -> ( 1, -1)
                                 Up          -> (-1,  0)
                                 Down        -> ( 1,  0)
                                 UpForward   -> (-1,  1)
                                 Forward     -> ( 0,  1)
                                 DownForward -> ( 1,  1)

-- Given a certain offset pair, return the direction this offset points in
-- Posn         =  the offset pair we want the Orientation for
-- Orientation  =  the orientation this offset pair points in, or Nothing if its (0, 0) as no direction
getDirectionOfOffset :: Posn -> Maybe Orientation
getDirectionOfOffset (ro, co) = case (ro, co) of
                                     (-1, -1) -> Just UpBack
                                     ( 0, -1) -> Just Back
                                     ( 1, -1) -> Just DownBack
                                     (-1,  0) -> Just Up
                                     ( 1,  0) -> Just Down
                                     (-1,  1) -> Just UpForward
                                     ( 0,  1) -> Just Forward
                                     ( 1,  1) -> Just DownForward
                                     _        -> Nothing