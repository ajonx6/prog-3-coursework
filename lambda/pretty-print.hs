import Data.Text

data LamMacroExpr = LamDef [(String , LamExpr)] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

-- Given a LamMacroExpr, pretty print it then output it to preserve single '\'
-- LamMacroExpr  =  the expression to pretty print
-- IO ()         =  we return an IO of no type so we can print the output
printLambda :: LamMacroExpr -> IO()
printLambda l = putStrLn (printLamMacroExpr l)

-- This function will do all the calculations and return the final string
-- LamMacroExpr  =  the expression to pretty print
-- String        =  the pretty printed output
printLamMacroExpr :: LamMacroExpr -> String
printLamMacroExpr d@(LamDef ms e) = let
                                        start = printStartMacros d
                                        macroText = getMacroExpressions ms
                                        text = printLamExpr e
                                    in
                                        start ++ replaceMacroText macroText text

-- This function returns all the "def N = E1 in ..." text in 1 go
-- LamMacroExpr  =  the macros to print
-- String        =  the pretty printed macros 
printStartMacros :: LamMacroExpr -> String
printStartMacros (LamDef [] _) = ""
printStartMacros (LamDef ((name, e):ms) _) = "def " ++ name ++ " = " ++ printLamExpr e ++ " in " ++ printStartMacros (LamDef ms (LamVar 0))

-- This function will pretty print each of the macros expressions and stores them with their macro
-- [(String, LamExpr)]  =  the macros with their lambda expressions
-- [(String, String)]   =  the macros with their pretty printed expressions
getMacroExpressions :: [(String, LamExpr)] -> [(String, String)]
getMacroExpressions [] = []
getMacroExpressions ((name, e):next) = (name, printLamExpr e) : getMacroExpressions next

-- Given a LamExpr, pretty print it recursively (a case for each expression I need to)
-- LamExpr  =  the expression to pretty print
-- String   =  the pretty printed output of the LamExpr
printLamExpr :: LamExpr -> String
printLamExpr (LamMacro name) = name
printLamExpr (LamApp e1@(LamAbs _ _) e2@(LamApp _ _)) = "(" ++ printLamExpr e1 ++ ") (" ++ printLamExpr e2 ++ ")"
printLamExpr (LamApp e1@(LamAbs _ _) e2) = "(" ++ printLamExpr e1 ++ ") " ++ printLamExpr e2
printLamExpr (LamApp e1 e2@(LamApp _ _)) = printLamExpr e1 ++ " (" ++ printLamExpr e2 ++ ")"
printLamExpr (LamApp e1 e2) = printLamExpr e1 ++ " " ++ printLamExpr e2
printLamExpr (LamAbs n ex) = "\\x" ++ show n ++ " -> " ++ printLamExpr ex
printLamExpr (LamVar n) = "x" ++ show n

-- Given the list of macros and expressions, and the pretty printed main expression, replaces parts of the text with the macro
-- [(String, String)]  =  the macros with their pretty printed expressions
-- String              =  the pretty printed expression
-- String              =  the final expression with all the macro expressions replacing the original ones
replaceMacroText :: [(String, String)] -> String -> String
replaceMacroText [] str = str
replaceMacroText ((name, macro):next) str = replaceMacroText next (replaceString macro name str)

-- Will replace parts of the input string with another string
-- String  =  the string we are going to replace
-- String  =  the string we will replace with
-- String  =  the pretty printed string we will replace
-- String  =  the string with all occurences of "old" replaced with "rep"
replaceString :: String -> String -> String -> String
replaceString rep old str = unpack (replace (pack rep) (pack old) (replace (pack ("(" ++ rep ++ ")")) (pack old) (pack str)))