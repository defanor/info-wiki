import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.UTF8 as U
import Prelude hiding (readFile)
import Text.XML.Expat.SAX
import Text.XML.Expat.Proc
import System.Environment (getArgs)
import Data.List
import Article

main :: IO ()
main = do
    args <- getArgs
    contents <- BSL.readFile (head args)
    let p = parse defaultParseOptions contents :: [SAXEvent String String]
        nodes = take (read $ args !! 1) $ infoNodes INSNone p
    putStrLn "@menu\n@include menu.texi\n@end menu\n\n"
    mapM_ processNode nodes
    putStrLn "@bye"

processNode :: InfoNode -> IO ()
processNode n@(IN t txt) = do
  appendFile "menu.texi" $ "* " ++ escapeNode t ++ ":: " ++ t ++ ".\n"
  putStrLn $ "@node " ++ escapeNode t ++ "\n" ++
    "@chapter " ++ escape t ++ "\n\n" ++ translate txt ++ "\n\n"


data InfoNode = IN String String

data INState = INSNone
             | INSPage
             | INSTitle
             | INSTitleText String
             | INSTextWait String
             | INSText String
             | INSTextCollect String String

infoNodes :: INState -> [SAXEvent String String] -> [InfoNode]
infoNodes INSNone ((StartElement "page" _):xs) = infoNodes INSPage xs
infoNodes INSPage ((EndElement "page"):xs) = infoNodes INSNone xs
infoNodes INSPage ((StartElement "title" _):xs) = infoNodes INSTitle xs
infoNodes INSTitle ((CharacterData t):xs)
  | isPrefixOf "Wikipedia:" t = infoNodes INSNone xs -- some of those are huge and useless
  | otherwise = infoNodes (INSTitleText t) xs
infoNodes (INSTitleText t) ((CharacterData t'):xs) = infoNodes (INSTitleText $ t ++ t') xs
infoNodes (INSTitleText t) ((EndElement "title"):xs) = infoNodes (INSTextWait t) xs
infoNodes (INSTextWait t) ((StartElement "text" _):xs) = infoNodes (INSText t) xs
infoNodes (INSText t) ((CharacterData txt):xs) = infoNodes (INSTextCollect t txt) xs
infoNodes (INSTextCollect t txt) ((CharacterData ""):xs) =
  infoNodes (INSTextCollect t txt) xs
infoNodes (INSTextCollect t txt) ((CharacterData txt'):xs) =
  infoNodes (INSTextCollect t (txt ++ txt')) xs
infoNodes (INSTextCollect t txt) ((EndElement "text"):xs) = IN t txt : infoNodes INSPage xs
infoNodes s (_:xs) = infoNodes s xs
infoNodes _ _ = []
