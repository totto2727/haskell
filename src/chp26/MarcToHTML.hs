{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString               as B
import           Data.Maybe
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as E
import qualified Data.Text.IO                  as TIO

type Author = T.Text

type Title = T.Text

data Book = Book
    { title  :: Title
    , author :: Author
    }
    deriving Show

book1 :: Book
book1 = Book "The conspiracy Against the Human Race" "Ligotti, Thomas"

book2 :: Book
book2 = Book "A Short History of Decay" "Citran, Emil"

book3 :: Book
book3 = Book "The Tears of Eros" "Baraille,George"

myBooks :: [Book]
myBooks = [book1, book2, book3]

type Html = T.Text

bookToHtml :: Book -> Html
bookToHtml book =
    let titleInTags  = mconcat ["<strong>", title book, "</strong>\n"]
        authorInTags = mconcat ["<em>", author book, "</em>\n"]
    in  mconcat ["<p>\n", titleInTags, authorInTags, "</p>\n"]

booksToHtml :: [Book] -> Html
booksToHtml books = mconcat
    [ "<html>\n"
    , "<head>\n<title>books</title>\n"
    , "<meta charset='utf-8'/>\n"
    , "</head>\n"
    , "<body>\n"
    , booksHtml
    , "\n</body>\n"
    , "</html>"
    ]
    where booksHtml = mconcat . map bookToHtml $ books

type MarcRecordRaw = B.ByteString
type MarcLeaderRaw = B.ByteString
type MarcDirectoryRaw = B.ByteString
type MarcDirectoryEntryRaw = B.ByteString

leaderLength :: Int
leaderLength = 24

recordLengthLength :: Int
recordLengthLength = 5

baseAddressStart :: Int
baseAddressStart = 12

baseAddressLength :: Int
baseAddressLength = 5

directoryEntryLength :: Int
directoryEntryLength = 12

getLeader :: MarcRecordRaw -> MarcLeaderRaw
getLeader = B.take leaderLength

rawToInt :: B.ByteString -> Int
rawToInt = read . T.unpack . E.decodeUtf8

getRecordLength :: MarcLeaderRaw -> Int
getRecordLength = rawToInt . B.take recordLengthLength

getBaseAddress :: MarcLeaderRaw -> Int
getBaseAddress = rawToInt . B.take baseAddressLength . B.drop baseAddressStart

getDirectoryLength :: MarcLeaderRaw -> Int
getDirectoryLength leader = getBaseAddress leader - leaderLength - 1

getDirectory :: MarcRecordRaw -> MarcDirectoryRaw
getDirectory marcStream =
    let afterLeader     = B.drop leaderLength marcStream
        directoryLength = getDirectoryLength . getLeader $ marcStream
    in  B.take directoryLength afterLeader

nextAndRest :: B.ByteString -> (MarcRecordRaw, B.ByteString)
nextAndRest marcStream = B.splitAt recordLength marcStream
    where recordLength = getRecordLength marcStream

allRecord :: B.ByteString -> [MarcRecordRaw]
allRecord marcStream | marcStream == B.empty = []
                     | otherwise             = next : allRecord rest
    where (next, rest) = nextAndRest marcStream

splitDirectory :: MarcDirectoryRaw -> [MarcDirectoryEntryRaw]
splitDirectory directory | directory == B.empty = []
                         | otherwise = nextEntry : splitDirectory restEntries
    where (nextEntry, restEntries) = B.splitAt directoryEntryLength directory

data FieldMetaData = FieldMetaData
    { tag         :: T.Text
    , fieldLength :: Int
    , fieldStart  :: Int
    }
    deriving Show
type FieldText = T.Text

tagLength :: Int
tagLength = 3

fieldLengthLength :: Int
fieldLengthLength = 4

fieldDelimiter :: Char
fieldDelimiter = toEnum 31

titleTag :: T.Text
titleTag = "245"

titleSubField :: Char
titleSubField = 'a'

authorTag :: T.Text
authorTag = "100"

authorSubField :: Char
authorSubField = 'a'

makeFileMetaData :: MarcDirectoryEntryRaw -> FieldMetaData
makeFileMetaData entry =
    let (rawTag   , rest    ) = B.splitAt tagLength entry
        (rawLength, rawStart) = B.splitAt fieldLengthLength rest
        theTag                = E.decodeUtf8 rawTag
        theLength             = rawToInt rawLength
        theStart              = rawToInt rawStart
    in  FieldMetaData theTag theLength theStart

getFieldMetaData :: [MarcDirectoryEntryRaw] -> [FieldMetaData]
getFieldMetaData = map makeFileMetaData

getFieldText :: MarcRecordRaw -> FieldMetaData -> FieldText
getFieldText record fieldMetaData =
    let recordLength    = getRecordLength record
        baseAddress     = getBaseAddress record
        baseRecord      = B.drop baseAddress record
        baseAtEntry     = B.drop (fieldStart fieldMetaData) baseRecord
        byteStringValue = B.take (fieldLength fieldMetaData) baseAtEntry
    in  E.decodeUtf8 byteStringValue

lookupFieldMetaData :: T.Text -> MarcRecordRaw -> Maybe FieldMetaData
lookupFieldMetaData aTag record =
    let metaData = getFieldMetaData . splitDirectory . getDirectory $ record
        results  = filter ((== aTag) . tag) metaData
    in  if null results then Nothing else Just (head results)

lookupSubField :: Maybe FieldMetaData -> Char -> MarcRecordRaw -> Maybe T.Text
lookupSubField Nothing _ _ = Nothing
lookupSubField (Just fieldMetaData) subField record =
    let fieldText = getFieldText record fieldMetaData
        subFields = T.split (== fieldDelimiter) fieldText
        result    = filter ((== subField) . T.head) subFields
    in  if null result then Nothing else Just . T.drop 1 . head $ result

lookupValue :: T.Text -> Char -> MarcRecordRaw -> Maybe T.Text
lookupValue aTag subField record =
    let metaData = lookupFieldMetaData aTag record
    in  lookupSubField metaData subField record

lookupTitle :: MarcRecordRaw -> Maybe T.Text
lookupTitle = lookupValue titleTag titleSubField

lookupAuthor :: MarcRecordRaw -> Maybe T.Text
lookupAuthor = lookupValue authorTag authorSubField

marcToPairs :: B.ByteString -> [(Maybe Title, Maybe Author)]
marcToPairs marcStream =
    let records = allRecord marcStream
        titles  = map lookupTitle records
        authors = map lookupAuthor records
    in  zip titles authors

pairsToBooks :: [(Maybe Title, Maybe Author)] -> [Book]
pairsToBooks pairs =
    let justPairs =
            filter (\(title, author) -> isJust title && isJust author) pairs
    in  map (\(title, author) -> Book (fromJust title) (fromJust author))
            justPairs

processRecords :: Int -> B.ByteString -> Html
processRecords n = booksToHtml . pairsToBooks . take n . marcToPairs

main :: IO ()
main = do
    marcData <- B.readFile "sample.mrc"
    let html = processRecords 500 marcData
    TIO.writeFile "books.html" html

    --let marcRecords = allRecord marcData
    --print . length $ marcRecords

    -- TIO.writeFile "books.html" . booksToHtml $ myBooks
