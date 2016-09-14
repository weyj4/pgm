module IO where

import System.Environment (getArgs)
import System.IO (withBinaryFile, hClose, hGetContents, IOMode(..))
import qualified Data.ByteString as B (hGetContents, map, ByteString, writeFile, append, take, unpack, sort, pack)
import qualified Data.ByteString.Char8 as B8 (pack, unpack)
import PNM (parseP5, Greymap(..))
import Data.Maybe (fromMaybe)
import Data.List (group, sortBy)
import Data.Word8

main = do
  let inp = "lena.pgm"
  let outp = "outlena2.pgm"
  withBinaryFile inp ReadMode $ \pgmHandle -> do
    args <- getArgs
    content <- B.hGetContents pgmHandle
    case parseP5 content of
      Nothing -> putStrLn "invalid content"
      Just pgm -> do
        writeGreymap outp (topN 25 pgm)

alterPGM :: (Greymap, B.ByteString) -> (B.ByteString -> B.ByteString) -> Greymap
alterPGM (Greymap w h m d, _) f = Greymap w h m (f d)

halve :: B.ByteString -> B.ByteString
halve = B.map check 
  where check word | word < 128 = 128
                   | otherwise  = word

modulo :: B.ByteString -> B.ByteString
modulo = B.map check 
  where check word = (word + 100) `mod` 128

writeGreymap :: FilePath -> Greymap -> IO ()
writeGreymap file gm = B.writeFile file $ B.append (B8.pack meta) (greyData gm)
  where meta = "P5\n" ++ show (greyWidth gm) ++ " " ++ show (greyHeight gm) ++ "\n" ++ show (greyMax gm) ++ "\n"

getFrequencies :: Int -> Greymap -> [(Word8, Int)]
getFrequencies n gm = take n $ map (\d -> (head d, length d)) $ reverse $ sortBy (\ a b -> compare (length a) (length b)) groupedData
  where groupedData = group $ B.unpack $ B.sort $ greyData gm

topN :: Int -> (Greymap, B.ByteString) -> Greymap
topN n gm@(Greymap w h m d, _) = Greymap w h m (f d)
  where top = map fst $ getFrequencies n $ fst gm
        f = B.pack . map g . B.unpack
        g w | elem w top = fromIntegral 0
            | otherwise  = w
