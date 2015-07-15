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
  Right (Pandoc _ l) -> printBlocks l

adjustWidth :: Int -> Text -> Text
adjustWidth max t = intercalate "\n" $ adjustWidth' (T.words t) ""
  where
    adjustWidth' :: [Text] -> Text -> [Text]
    adjustWidth' (x:xs) acc
      | T.length x + T.length acc + 1 <= max =
        if acc == ""
        then adjustWidth' xs x
        else adjustWidth' xs (T.concat [acc, " ", x])
      | T.length x > max =
        if acc == ""
        then x : adjustWidth' xs ""
        else acc : adjustWidth' (x:xs) ""
      | otherwise = acc : adjustWidth' xs x
    adjustWidth' [] acc = [acc]

printBlocks :: [Block] -> Text
printBlocks l = intercalate "\n\n" $ L.map printBlock l

printBlock :: Block -> Text
printBlock (CodeBlock attr code) = pack code
printBlock other = adjustWidth 80 $ printBlock' other
  where
    printBlock' :: Block -> Text
    printBlock' (Plain il) = printInlines il
    printBlock' (Para il) = T.append "  " $ printInlines il
    printBlock' (RawBlock fmt str) = escape $ pack str
    printBlock' (BlockQuote blocks) = printBlocks blocks
    printBlock' other = pack $ show other

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
printInline other = pack $ show other

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
