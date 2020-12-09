module WordSearch where 
import Data.Maybe

type WordSearchGrid = [[ Char ]]
type Placement = (Posn,Orientation)
type Posn = (Int,Int)
data Orientation = Forward | Back | Up | Down | UpForward | UpBack | DownForward | DownBack deriving (Eq,Ord,Show,Read)

-- Given a list of words and a wordsearch, will return a list of the words with Nothing if they are not found, or a Placement if it is found in the grid
solveWordSearch :: [String] -> WordSearchGrid -> [(String, Maybe Placement)]
solveWordSearch words grid = let
                                 solve [] acc = acc
                                 solve (word:ws) acc = solve ws (acc ++ [(word, searchOneWord word grid)])
                             in
                                 solve words []

-- Given word and the wordsearch, will search to see if this one word is in the wordsearch and will return either Nothing or the Placement where it is
searchOneWord :: String -> WordSearchGrid -> Maybe Placement
searchOneWord word grid = let
                              startPositions = getPositionsOfStartLetter (head word) grid
                          in
                              search [(p, tail word, p, Nothing) | p <- startPositions] grid

-- Takes in a word and the wordsearch grid and returns a list of positions of where the grid character equals the word's first character
getPositionsOfStartLetter :: Char -> WordSearchGrid -> [Posn]
getPositionsOfStartLetter char grid = let 
                                          getPositionsOfStartLetter' [] _ acc = acc
                                          getPositionsOfStartLetter' (row:rs) rowNum acc = getPositionsOfStartLetter' rs (rowNum + 1) (acc ++ getPositionInRow char row (rowNum, 0) [])  
                                      in
                                          getPositionsOfStartLetter' grid 0 []

-- Takes a word, the wordsearch and a given row number, and returns the list of positions where the grid has the word's first character, only for the given row
getPositionInRow :: Char -> [Char] -> Posn -> [Posn] -> [Posn]
getPositionInRow _ [] _ acc = acc                                           
getPositionInRow charToMatch (gridchar:cs) (row, col) acc = if charToMatch == gridchar then getPositionInRow charToMatch cs (row, col + 1) (acc ++ [(row, col)])
                                                            else getPositionInRow charToMatch cs (row, col + 1) acc

-- Takes in a list of all the next cells to search and the wordsearch, and returns either Nothing or the Placement where it is
-- startpos = initial position of the original start letter, so we know where it started (as could start from multiple locations)
-- trimmedword = the word being trimmed down to search for, when trimmedword is empty then we know we have found the whole word so can return a valid placement
-- pos = the position of the character that generated this particular cell to be searched
-- orient = the orientation we are searching in for this cell (for the start cell, will be Nothing, otherwise will have some orientation)
search :: [(Posn, [Char], Posn, Maybe Orientation)] -> WordSearchGrid -> Maybe Placement
search [] _ = Nothing
search (((r, c), [], _, orient):_) _ = Just ((c, r), fromJust orient)
search ((startpos, trimmedword, pos, orient):next) grid = let 
                                               neighbourCellsThatWork = neighbourCellsEqualNextChar grid (head trimmedword, orient) pos 0 []
                                               toAdd = [(startpos, tail trimmedword, p, o) | (p, o) <- neighbourCellsThatWork]
                                           in
                                               search (next ++ toAdd) grid

-- Takes in the next character to check and the position of the previous character in the wordsearch, and some offset coords and returns a list of points with a direction where the word is continued
-- grid = wordsearch
-- (char, orient) = character we are searching for along with the direction we're searching in (orient is maybe as the start value has no direction, it searches all 8 directions)
-- (r, c) = position of the letter we just found
-- index = index for which direction we look in when we search all 8 neighbouring cells initially
-- acc = accummulator to return with all next cells to search
neighbourCellsEqualNextChar :: WordSearchGrid -> (Char, Maybe Orientation) -> Posn -> Int -> [(Posn, Maybe Orientation)] -> [(Posn, Maybe Orientation)]
neighbourCellsEqualNextChar grid (char, orient) (r, c) index acc = if isNothing orient then
                                                                       -- If there is a start cell, then find each neighbour cell and add to the "to check" list
                                                                       let
                                                                           rowoffset = (index `div` 3) - 1
                                                                           coloffset = (index `mod` 3) - 1
                                                                           dir = getDirectionOfOffset (rowoffset, coloffset)
                                                                           charInDir = if isNothing dir then Nothing else getCharacter grid (r + rowoffset, c + coloffset)
                                                                       in
                                                                           if index > 8 then acc
                                                                           else if isNothing dir || isNothing charInDir || fromJust charInDir /= char then neighbourCellsEqualNextChar grid (char, orient) (r, c) (index + 1) acc
                                                                           else neighbourCellsEqualNextChar grid (char, orient) (r, c) (index + 1) (acc ++ [((r + rowoffset, c + coloffset), dir)])
                                                                   else
                                                                       -- If we already know what direction we are searching in, only continue searching this direction if it equals the letter needed for that cell
                                                                       let
                                                                           (rowoffset, coloffset) = getOffsetForDirection (fromJust orient)
                                                                           charInDir = getCharacter grid (r + rowoffset, c + coloffset)
                                                                       in
                                                                           if isNothing charInDir || fromJust charInDir /= char then [] 
                                                                           else [((r + rowoffset, c + coloffset), orient)]

-- Given the wordsearch and a position, will return Nothing if out of bounds or the character at that position if in bounds 
getCharacter :: WordSearchGrid -> Posn -> Maybe Char
getCharacter grid (r, c) = if r < 0 || c < 0 || r >= length grid || c >= length (head grid) then Nothing else Just ((grid !! r) !! c)

-- Given a direction, return the offset pair that point in that direction
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

-- Given a certain offset pair, return the direction this offset points in (or nothing, if it is the central one)
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