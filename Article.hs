module Article where


import Text.Pandoc.Readers.MediaWiki
import Data.Default
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Definition
import Data.List


translate :: String -> String
translate s = case readMediaWiki def s of
  Left err -> "parse error"
  Right (Pandoc _ l) -> concatMap printBlock l

printBlocks :: [Block] -> String
printBlocks l = intercalate "\n" $ map printBlock l

printBlock :: Block -> String
printBlock (Plain il) = printInlines il
printBlock (Para il) = "\n\n" ++ printInlines il ++ "\n\n"
printBlock (CodeBlock attr str) = escape str
printBlock (RawBlock fmt str) = escape str
printBlock (BlockQuote blocks) = printBlocks blocks
printBlock (OrderedList attr blocks) = printList "enumerate" blocks
printBlock (BulletList blocks) = printList "itemize" blocks
printBlock (DefinitionList ib) = intercalate "\n\n" $ map printDef ib
printBlock (Header level attr il) = printHeading (level - 2) $ printInlines il
printBlock HorizontalRule = "\n----\n"
printBlock (Table caption alignments widths headers cells) = "todo, table"
printBlock (Div attr blocks) = printBlocks blocks
printBlock Null = ""


printDef :: ([Inline], [[Block]]) -> String
printDef (i, b) = printInlines i ++ "\n\n" ++ intercalate "\n\n" (map printBlocks b)

printList :: String -> [[Block]] -> String
printList s items = "\n@" ++ s ++ "\n" ++ concatMap printItem items ++ "@end " ++ s ++ "\n\n"

printItem :: [Block] -> String
printItem l = "@item\n" ++ printBlocks l ++ "\n"


printHeading :: Int -> String -> String
printHeading n s = "\n\n" ++ if n <= 2
                             then "@" ++ (concat $ take n (repeat "sub")) ++ "section " ++ s
                             else "@strong{" ++ s ++ "}"

printInlines :: [Inline] -> String
printInlines = concatMap printInline

printInline :: Inline -> String
printInline (Str s) = escape s
printInline (Emph inl) = "@emph{" ++ printInlines inl ++ "}"
printInline (Strong inl) = "@strong{" ++ printInlines inl ++ "}"
printInline (Strikeout inl) = printInlines inl
printInline (Superscript inl) = printInlines inl
printInline (Subscript inl) = printInlines inl
printInline (SmallCaps inl) = printInlines inl
printInline (Quoted t inl) = "``" ++ printInlines inl ++ "''"
printInline (Cite t inl) = "``" ++ printInlines inl ++ "''"
printInline (Code attr s) = "`" ++ escape s ++ "`"
printInline Space = " "
printInline LineBreak = "\n"
printInline (Math mt s) = "$" ++ escape s ++ "$"
printInline (RawInline fmt s) = escape s
printInline (Link inl (l, "wikilink")) = printInlines inl ++
                                         " (@ref{" ++ escapeNode l ++ "})"
printInline l@(Link inl other) = escape $ show l
printInline (Image alt t) = "\n[image: " ++ printInlines alt ++ " " ++ (escape $ show t) ++ "]\n"
printInline (Note blocks) = "@footnote{" ++ printBlocks blocks ++ "}"
printInline (Span attr inl) = printInlines inl


escape :: String -> String
escape (x:xs)
  | x `elem` "@{}" = '@' : x : escape xs
  | x `elem` "\1114111" = escape xs
  | otherwise = x : escape xs
escape [] = []

escapeNode :: String -> String
escapeNode (':':xs) = " (colon)" ++ escapeNode xs
escapeNode (',':xs) = " (comma)" ++ escapeNode xs
escapeNode ('{':xs) = " (curly-l)" ++ escapeNode xs
escapeNode ('}':xs) = " (curly-r)" ++ escapeNode xs
escapeNode (c:xs) = c : escapeNode xs
escapeNode [] = []
