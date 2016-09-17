module PNM where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8
import Data.Char(isSpace)
import Data.Int
import Data.Word(Word8(..))

data Greymap = Greymap {
    greyWidth :: Int
  , greyHeight :: Int
  , greyMax :: Int
  , greyData :: B.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap w h m d) = "Greymap " ++ show w ++ "x" ++ show h ++ " " ++ show m ++ "\n" ++ show (take 100 (B.unpack d))


matchHeader :: B.ByteString -> B.ByteString -> Maybe B.ByteString
matchHeader prefix str
    | prefix `B8.isPrefixOf` str = Just (B8.dropWhile isSpace (B.drop (B.length prefix) str))
    | otherwise = Nothing

getNat :: B.ByteString -> Maybe (Int, B.ByteString)
getNat s = case B8.readInt s of
            Nothing -> Nothing
            Just (num, rest)
                | num <= 0 -> Nothing
                | otherwise -> Just (fromIntegral num, rest)

getBytes :: Int -> B.ByteString -> Maybe (B.ByteString, B.ByteString)
getBytes n str = let count = fromIntegral n
                     both@(prefix,_) = B.splitAt count str
                 in if B.length prefix < count
                    then Nothing
                    else Just both

skipSpace :: (a, B.ByteString) -> Maybe (a, B.ByteString)
skipSpace (a, s) = Just (a, B8.dropWhile isSpace s)

(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

parseP5 :: B.ByteString -> Maybe (Greymap, B.ByteString)
parseP5 s =
  matchHeader (B8.pack "P5") s        >>?
  \s -> getNat s                      >>?
  skipSpace                           >>?
  \(width, s2) -> getNat s2           >>?
  skipSpace                           >>?
  \(height, s3) -> getNat s3          >>?
  \a@(maxGrey, s4) -> skipSpace a     >>?
  (getBytes (width * height) . snd)   >>?
  \(bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)
