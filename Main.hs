{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as U
import Prelude hiding (readFile)
import Text.XML.Expat.SAX
import Text.XML.Expat.Proc
import System.Environment (getArgs)
import qualified Data.List as L
import Data.Text as T

import Text.Pandoc.Readers.MediaWiki
import Data.Default
import Text.Pandoc.Writers.Texinfo
import Text.Pandoc.Definition
import Control.Monad.State.Lazy

import Control.Applicative
import Data.Text.IO as TIO

replace' :: [(Text, Text)] -> Text -> Text
replace' ((p, w):xs) t = replace' xs (replace p w t)
replace' [] t = t

escapeNode :: Text -> Text
escapeNode = replace' [(":", "colon"),
                       (",", "comma"),
                       ("{", "curly-l"),
                       ("}", "curly-r"),
                       ("*", "asterisk"),
                       ("_", " ")
                      ]

main :: IO ()
main = do
    args <- getArgs
    contents <- BSL.readFile (L.head args)
    let p = parse defaultParseOptions contents :: [SAXEvent Text Text]
        nodes = (if (L.length args >= 2)
                 then (L.take (read $ args !! 1))
                 else id) $ infoNodes INSNone p
        top = "\US\nFile: simplewiki,  Node: Top,  Up: (dir)\n\nTop\n***\n\nHello there.\n\n"
        tl = L.length top
    Prelude.writeFile "simplewiki-0" top
    processNodes (2 ^ 24) "simplewiki" (PS tl tl 0 1 ["Node: Top\DEL0\n"] ["simplewiki-0: 0\n"]) nodes

data PState = PS { bytes :: Int
                 , fileSize :: Int
                 , files :: Int
                 , nodes :: Int
                 , tagTable :: [Text]
                 , indirect :: [Text]
                 } deriving (Show)

processNodes :: Int -> Text -> PState -> [InfoNode] -> IO ()
processNodes maxFileSize fileName (PS b fs f n tt ind) (x@(IN t txt):xs) =
  let node = printNode fileName x
      nodeLen = T.length node in
   if nodeLen > maxFileSize
   then Prelude.putStrLn $ "The node is too big: " ++ unpack t
   else
     let tt' = T.concat ["Node: ", escapeNode t, "\DEL", pack (show b), "\n"] : tt
         b' = b + nodeLen
     in
      if fs + nodeLen <= maxFileSize
      then do
        TIO.appendFile (unpack fileName ++ "-" ++ show f) node
        processNodes maxFileSize fileName (PS b' (fs + nodeLen) f (n + 1) tt' ind) xs
      else let fn' = T.concat [fileName, "-", pack (show (f + 1))]
               ind' = T.concat [fn', ": ", pack (show b), "\n"] : ind in do
        TIO.writeFile (unpack fn') node
        processNodes maxFileSize fileName (PS b' nodeLen (f + 1) (n + 1) tt' ind') xs
processNodes _ fileName (PS _ _ _ _ tt ind) [] = let fn = unpack fileName
                                                     app = TIO.appendFile fn in do
  TIO.writeFile fn $ T.concat
    ["INFO-DIR-SECTION Wiki\nSTART-INFO-DIR-ENTRY\n",
     "* SimpleWiki: (simplewiki).             Simple wiki.\n",
     "END-INFO-DIR-ENTRY\n\n", "\US\nIndirect:\n"]
  app $ T.concat $ L.reverse ind
  app "\US\nTag Table:\n(Indirect)\n"
  app $ T.concat $ L.reverse tt
  app "\US\nEnd Tag Table\n"

printNode :: Text -> InfoNode -> Text
printNode fn (IN t txt) = T.concat ["\n\US\nFile: ", fn,
                                    ",  Node: ", escapeNode t,
                                    ",  Up: Top\n\n",
                                    t, "\n", T.replicate (T.length t) "*", "\n\n",
                                    fst $ runState (translate txt) []]

translate :: Text -> State [[Block]] Text
translate s = case readMediaWiki def (unpack s) of
  Left err -> return "parse error"
  Right (Pandoc _ l) -> do
    b <- printBlocks 0 l
    n <- printNotes
    return $ T.append b n

adjustWidth :: Int -> Text -> [Text]
adjustWidth max t = adjustWidth' (T.splitOn " " t) ""
   where
    adjustWidth' :: [Text] -> Text -> [Text]
    adjustWidth' (x:xs) acc
      -- accumulate
      | T.length x + T.length acc + 1 <= max =
        if acc == ""
        then adjustWidth' xs x
        else adjustWidth' xs (T.concat [acc, " ", x])
      -- deal with long words
      | T.length x > max =
        if acc == ""
        then x : adjustWidth' xs ""
        else acc : adjustWidth' (x:xs) ""
      -- flush
      | otherwise = acc : adjustWidth' xs x
    adjustWidth' [] acc = [acc]

padded :: Int -> Text -> State [[Block]] Text
padded n = return . T.unlines . L.concatMap padLine . T.lines
  where
    padLine = L.map (T.append $ T.replicate n " ") . adjustWidth (80 - n)

printNotes :: State [[Block]] Text
printNotes = do
  blocks <- get
  if L.length blocks > 0
    then T.append "\n\n   ---------- Footnotes ----------\n\n" <$>
         T.intercalate "\n\n" <$>
         (mapM (printBlocks 3) (L.map addNumber (L.zip [1..] blocks)))
    else return ""
  where addNumber (n, b) = Plain [Str $ "(" ++ show n ++ ")"] : b
          

printBlocks :: Int -> [Block] -> State [[Block]] Text
printBlocks pad l = intercalate "\n\n" <$> mapM (printBlock pad) l

printBlock :: Int -> Block -> State [[Block]] Text
printBlock pad (CodeBlock attr code) = padded pad $ pack code
printBlock pad (Plain il) = printInlines il >>= padded pad
printBlock pad (Para il) = T.append "  " <$> printInlines il >>= padded pad
printBlock pad (RawBlock fmt str) = padded pad $ pack str
printBlock pad (BlockQuote blocks) = printBlocks (pad + 2) blocks
printBlock pad (OrderedList attr blocks) = printOrderedList (pad + pad') 1 blocks
  where pad' = (L.length $ show (1 + L.length blocks)) + 2
printBlock pad (BulletList blocks) = printBulletList pad blocks
printBlock pad (DefinitionList ib) = T.intercalate "\n\n" <$> mapM (printDef pad) ib
printBlock pad (Header level attr il) = printInlines il >>= printHeading (level - 1)
printBlock pad HorizontalRule = return $ T.replicate 80 "-"
printBlock pad (Table caption alignments widths headers cells) = return "todo, table"
printBlock pad (Div attr blocks) = printBlocks pad blocks
printBlock pad Null = return ""


printHeading :: Int -> Text -> State [[Block]] Text
printHeading n s = return $ T.concat [s, "\n", T.replicate (T.length s) hChar]
  where hChar = case n of
                 0 -> "*"
                 1 -> "="
                 2 -> "-"
                 _ -> "."

-- It is assumed that padding includes the padding required for the
-- widest number
printOrderedList :: Int -> Int -> [[Block]] -> State [[Block]] Text
printOrderedList pad n (x:xs) = do
  b <- printBlocks pad x
  ol <- printOrderedList pad (n + 1) xs
  return $ T.concat [pnum, T.drop pad b, ol]
  where
    tnum = T.pack $ show n ++ ". "
    pnum = T.append (T.replicate (pad - T.length tnum) " ") tnum
printOrderedList _ _ [] = return ""

printBulletList :: Int -> [[Block]] -> State [[Block]] Text
printBulletList pad (x:xs) = do
  b <- printBlocks (pad + 2) x
  bl <- printBulletList pad xs
  return $ T.concat [T.replicate pad " ", "- ", T.drop (pad + 2) b, bl]
printBulletList pad [] = return ""

printDef :: Int -> ([Inline], [[Block]]) -> State [[Block]] Text
printDef pad ([], blocks) = printBulletList (pad + 2) blocks
printDef pad (inl, blocks) = do
  il <- printInlines inl >>= padded pad
  bl <- printBulletList (pad + 2) blocks
  return $ T.concat [il, "\n", bl]
printInlines :: [Inline] -> State [[Block]] Text
printInlines = (<$>) T.concat . mapM printInline

printInline :: Inline -> State [[Block]] Text
printInline (Str s) = return $ pack s
printInline (Emph inl) = (\x -> T.concat ["_", x, "_"]) <$> printInlines inl
printInline (Strong inl) = (\x -> T.concat ["*", x, "*"]) <$> printInlines inl
printInline (Strikeout inl) = (\x -> T.concat ["-", x, "-"]) <$> printInlines inl
printInline (Superscript inl) = printInlines inl
printInline (Subscript inl) = printInlines inl
printInline (SmallCaps inl) = printInlines inl
printInline (Quoted t inl) = (\x -> T.concat ["‘", x, "’"]) <$> printInlines inl
printInline (Cite t inl) = (\x -> T.concat ["‘", x, "’"]) <$> printInlines inl
printInline (Code attr inl) = return $ T.concat ["`", pack inl, "`"]
printInline Space = return " "
printInline LineBreak = return "\n"
printInline (Math mt s) = return $ T.concat ["$", pack s, "$"]
printInline (RawInline fmt s) = return $ pack s
printInline (Link inl (l, "wikilink")) =
  (\x -> T.concat [x, " (*note ", escapeNode $ pack l, "::)"]) <$> printInlines inl
printInline (Link inl (url, "")) = do
  il <- printInlines inl
  return $ case il of
   "" -> T.concat ["<", pack url, ">"]
   i -> T.concat [i, " (", pack url, ")"]
printInline (Image alt t) =
  (\x -> T.concat ["[image: ", x, " ", pack $ show t, "]"]) <$> printInlines alt
printInline (Note blocks) = do
  b <- get
  put $ b ++ [blocks]
  return $ T.concat ["(", (pack . show $ L.length b + 1), ")"]
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
