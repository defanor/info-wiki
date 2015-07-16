{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as U
import Prelude hiding (readFile)
import Text.XML.Expat.SAX
import Text.XML.Expat.Proc
import System.Environment (getArgs)
import qualified Data.List as L
-- import Article
import Data.Text as T

import Text.Pandoc.Readers.MediaWiki
import Data.Default
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Definition

replace' :: [(Text, Text)] -> Text -> Text
replace' ((p, w):xs) t = replace' xs (replace p w t)
replace' [] t = t

escape :: Text -> Text
escape = id -- replace' [("@", "@@"), ("{", "@{"), ("}", "@}")]

escapeNode :: Text -> Text
escapeNode = replace' [(":", "colon"),
                       (",", "comma"),
                       ("{", "curly-l"),
                       ("}", "curly-r"),
                       ("*", "asterisk")
                      ]

main :: IO ()
main = do
    args <- getArgs
    contents <- BSL.readFile (L.head args)
    let p = parse defaultParseOptions contents :: [SAXEvent Text Text]
        nodes = L.take (read $ args !! 1) $ infoNodes INSNone p
        top = "\US\nFile: simplewiki,  Node: Top,  Up: (dir)\n\nTop\n***\n\nHello there.\n\n"
        tl = L.length top
    writeFile "simplewiki-0" top
    processNodes (2 ^ 20) "simplewiki" (PS tl tl 0 1 "Node: Top\DEL0\n" "simplewiki-0: 0\n") nodes

data PState = PS { bytes :: Int
                 , fileSize :: Int
                 , files :: Int
                 , nodes :: Int
                 , tagTable :: Text
                 , indirect :: Text
                 } deriving (Show)

processNodes :: Int -> Text -> PState -> [InfoNode] -> IO ()
processNodes maxFileSize fileName (PS b fs f n tt ind) (x@(IN t txt):xs) =
  let node = printNode fileName x
      nodeLen = T.length node in
   if nodeLen > maxFileSize
   then putStrLn $ "The node is too big: " ++ unpack t
   else
     let tt' = T.append tt (T.concat ["Node: ", escapeNode t, "\DEL", pack (show b), "\n"])
         b' = b + nodeLen
     in
      if fs + nodeLen <= maxFileSize
      then do
        appendFile (unpack fileName ++ "-" ++ show f) (unpack node)
        processNodes maxFileSize fileName (PS b' (fs + nodeLen) f (n + 1) tt' ind) xs
      else let fn' = T.concat [fileName, "-", pack (show (f + 1))]
               ind' = T.append ind (T.concat [fn', ": ", pack (show b), "\n"]) in do
        writeFile (unpack fn') (unpack node)
        processNodes maxFileSize fileName (PS b' nodeLen (f + 1) (n + 1) tt' ind') xs
processNodes _ fileName (PS _ _ _ _ tt ind) [] =
  writeFile (unpack fileName) (unpack $ T.concat
                               ["INFO-DIR-SECTION Wiki\nSTART-INFO-DIR-ENTRY\n",
                                "* SimpleWiki: (simplewiki).             Simple wiki.\n",
                                "END-INFO-DIR-ENTRY\n\n", "\US\nIndirect:\n", ind,
                                "\US\nTag Table:\n(Indirect)\n", tt, "\US\nEnd Tag Table\n"])

-- appendFile "menu.texi" . unpack

printNode :: Text -> InfoNode -> Text
printNode fn (IN t txt) = T.concat ["\n\US\nFile: ", fn,
                                    ",  Node: ", t,
                                    ",  Up: Top\n\n",
                                    t, "\n", T.replicate (T.length t) "*", "\n\n",
                                    translate txt]


translate :: Text -> Text
translate s = case readMediaWiki def (unpack s) of
  Left err -> "parse error"
  Right (Pandoc _ l) -> printBlocks 0 l

padded :: Int -> Text -> Text
padded n = T.unlines . L.map (T.append $ T.replicate n " ") . T.lines

-- A version that prepends something to the first line
padded' :: Int -> Text -> Text -> Text
padded' n p t = case T.lines t of
  (x:xs) -> T.concat [p, T.replicate (n - T.length p) " ", x, "\n", padded n (T.unlines xs)]
  [] -> ""


printBlocks :: Int -> [Block] -> Text
printBlocks pad l = intercalate "\n\n" $ L.map (printBlock pad) l

printBlock :: Int -> Block -> Text
printBlock pad (CodeBlock attr code) = padded pad $ pack code
printBlock pad (Plain il) = padded pad $ printInlines il
printBlock pad (Para il) = padded pad $ T.append "  " $ printInlines il
printBlock pad (RawBlock fmt str) = padded pad $ escape $ pack str
printBlock pad (BlockQuote blocks) = padded (pad + 2) $ printBlocks pad blocks
printBlock pad (OrderedList attr blocks) = printOrderedList (pad + pad') 1 blocks
  where pad' = (L.length $ show (1 + L.length blocks)) + 2
printBlock pad (BulletList blocks) = printBulletList pad blocks
printBlock pad (DefinitionList ib) = T.intercalate "\n\n" $ L.map (printDef pad) ib
printBlock pad (Header level attr il) = printHeading (level - 1) $ printInlines il
printBlock pad HorizontalRule = T.replicate 80 "-"
printBlock pad (Table caption alignments widths headers cells) = "todo, table"
printBlock pad (Div attr blocks) = printBlocks pad blocks
printBlock pad Null = ""


printHeading :: Int -> Text -> Text
printHeading n s = T.concat [s, "\n", T.replicate (T.length s) hChar]
  where hChar = case n of
                 0 -> "*"
                 1 -> "="
                 2 -> "-"
                 _ -> "."

-- It is assumed that padding includes the padding required for the
-- widest number
printOrderedList :: Int -> Int -> [[Block]] -> Text
printOrderedList pad n (x:xs) = T.concat [pnum,
                                          T.drop pad $ printBlocks pad x,
                                          printOrderedList pad (n + 1) xs]
  where
    tnum = T.pack $ show n ++ ". "
    pnum = T.append (T.replicate (pad - T.length tnum) " ") tnum
printOrderedList _ _ [] = ""

printBulletList :: Int -> [[Block]] -> Text
printBulletList pad (x:xs) = T.concat [T.replicate pad " ", "- ",
                                       T.drop (pad + 2) $ printBlocks (pad + 2) x,
                                       printBulletList pad xs]
printBulletList pad [] = ""

printDef :: Int -> ([Inline], [[Block]]) -> Text
printDef pad (inl, blocks) = T.concat [padded pad (printInlines inl),
                                       printBulletList (pad + 2) blocks]
                             
printInlines :: [Inline] -> Text
printInlines = T.concat . L.map printInline

printInline :: Inline -> Text
printInline (Str s) = pack s
printInline (Emph inl) = T.concat ["_", printInlines inl, "_"]
printInline (Strong inl) = T.concat ["*", printInlines inl, "*"]
printInline (Strikeout inl) = T.concat ["-", printInlines inl, "-"]
printInline (Superscript inl) = printInlines inl
printInline (Subscript inl) = printInlines inl
printInline (SmallCaps inl) = printInlines inl
printInline (Quoted t inl) = T.concat ["‘", printInlines inl, "’"]
printInline (Cite t inl) = T.concat ["‘", printInlines inl, "’"]
printInline (Code attr inl) = T.concat ["`", pack inl, "`"]
printInline Space = " "
printInline LineBreak = "\n"
printInline (Math mt s) = T.concat ["$", escape (pack s), "$"]
printInline (RawInline fmt s) = escape $ pack s
printInline (Link inl (l, "wikilink")) =
  T.concat [printInlines inl, " (*note ", escapeNode $ pack l, "::)"]
printInline l@(Link inl other) = pack $ show l
printInline (Image alt t) = T.concat ["[image: ", printInlines alt, " ", pack $ show t, "]"]
printInline (Note blocks) = T.concat ["note{", printBlocks 0 blocks, "}"]
printInline (Span attr inl) = printInlines inl

data InfoNode = IN Text Text

data INState = INSNone
             | INSPage
             | INSTitle
             | INSTitleText Text
             | INSTextWait Text
             | INSText Text
             | INSTextCollect Text Text

infoNodes :: INState -> [SAXEvent Text Text] -> [InfoNode]
infoNodes INSNone ((StartElement "page" _):xs) = infoNodes INSPage xs
infoNodes INSPage ((EndElement "page"):xs) = infoNodes INSNone xs
infoNodes INSPage ((StartElement "title" _):xs) = infoNodes INSTitle xs
infoNodes INSTitle ((CharacterData t):xs)
  | isPrefixOf "Wikipedia:" t = infoNodes INSNone xs -- some of those are huge and useless
  | otherwise = infoNodes (INSTitleText t) xs
infoNodes (INSTitleText t) ((CharacterData t'):xs) = infoNodes (INSTitleText $ append t t') xs
infoNodes (INSTitleText t) ((EndElement "title"):xs) = infoNodes (INSTextWait t) xs
infoNodes (INSTextWait t) ((StartElement "text" _):xs) = infoNodes (INSText t) xs
infoNodes (INSText t) ((CharacterData txt):xs) = infoNodes (INSTextCollect t txt) xs
infoNodes (INSTextCollect t txt) ((CharacterData ""):xs) =
  infoNodes (INSTextCollect t txt) xs
infoNodes (INSTextCollect t txt) ((CharacterData txt'):xs) =
  infoNodes (INSTextCollect t (append txt txt')) xs
infoNodes (INSTextCollect t txt) ((EndElement "text"):xs) = IN t txt : infoNodes INSPage xs
infoNodes s (_:xs) = infoNodes s xs
infoNodes _ _ = []
