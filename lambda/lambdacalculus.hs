data LamMacroExpr = LamDef [(String , LamExpr)] LamExpr deriving (Eq,Show,Read)
data LamExpr = LamMacro String | LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Eq, Show, Read)

prettyPrint :: LamMacroExpr -> String
prettyPrint (LamDef [] (LamVar m)) = "x" ++ show m
prettyPrint (LamDef [] (LamAbs m ex)) = "\\" ++ "x" ++ show m ++ " -> " ++ prettyPrint (LamDef [] ex)
prettyPrint (LamDef [] (LamApp ex1 ex2)) = prettyPrint (LamDef [] ex1) ++ " " ++ prettyPrint (LamDef [] ex2)