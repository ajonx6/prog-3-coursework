import WordSearch

-- Test to see whether a letter not in the wordsearch returns no start positions
ws1   = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
w1    = 'X'
test1 = null (getPositionsOfStartLetter w1 ws1)

-- Test to see whether a letter to be found in a given row returns the right number of positions
ws2   = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
w2    = 'A'
test2 = length (getPositionInRow w2 (ws2 !! 1) (1, 0) []) == 3

-- Test to see whether a letter to be found in a given row returns the correct positions
ws3   = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
w3    = 'A'
sol3  = [(1, 1), (1, 3), (1, 7)]
test3 = getPositionInRow w2 (ws2 !! 1) (1, 0) [] == sol3

-- Test to see whether a letter to be found in the wordsearch returns the right number of positions
ws4   = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
w4    = 'S'
test4 = length (getPositionsOfStartLetter w4 ws4) == 5

-- Test to see whether a letter to be found in the wordsearch returns the correct positions
ws5   = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
w5    = 'S'
sol5  = [(0, 7), (1, 0), (2, 2), (6, 7), (7, 5)]
test5 = getPositionsOfStartLetter w5 ws5 == sol5

-- Test to see whether all words given are found on the wordsearch with a given Placement
--ws1 = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
--w1 = ["HASKELL", "STRING", "STACK", "MAIN", "METHOD"]
--s1 = [("HASKELL", Just((0, 0), DownForward)), ("STRING", Just((7, 0), Back)), ("STACK", Just((2, 2), Forward)), ("MAIN", Just((2, 7), Up)), ("METHOD", Just((4, 3), Down))]
--test1 :: Bool
--test1 = solveWordSearch w1 ws1 == s1

-- Test to see whether a word not on the wordsearch has no Placement
--ws2 = ["HAGNIRTSH", "SACAGETAK", "GCSTACKEL", "MGJKMILKI", "EKNLETGCN", "TNIRTLETE", "IRAAHCLSR", "MAMROSAGD", "GIZKDDNRG"]
--w2 = ["HASKELL", "NOPASS", "FAIL", "MAIN", "METHOD"]
--s2 = [("HASKELL", Just((0, 0), DownForward)), ("NOPASS", Nothing), ("FAIL", Nothing), ("MAIN", Just((2, 7), Up)), ("METHOD", Just((4, 3), Down))]
--test2 :: Bool
--test2 = solveWordSearch w2 ws2 == s2

--ws3 = ["ABCD", "ABCD", "ABCD", "ABCD"]