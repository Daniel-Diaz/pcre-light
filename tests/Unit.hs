{-# LANGUAGE OverloadedStrings #-}

import Text.Regex.PCRE.Light (compile,compileM,match,captureNames,captureCount)
import qualified Text.Regex.PCRE.Light.Char8 as String (compile,compileM,match)
import Text.Regex.PCRE.Light.Base

import qualified Data.ByteString.Char8 as S
import System.IO

import Test.HUnit
import System.Exit (exitFailure)
import Control.Monad (when)
import System.IO
import Data.Either
import qualified Data.Map as M

import System.IO.Unsafe
import Control.Exception
import Control.Monad.Except

assertBool' :: S.ByteString -> Bool -> Assertion
assertBool'  s = assertBool  (S.unpack s)

assertEqual' :: (Eq a, Show a) => S.ByteString -> a -> a -> Assertion
assertEqual' s = assertEqual (S.unpack s)

testLabel :: S.ByteString -> Test -> Test
testLabel  s = TestLabel (S.unpack s)

testRegex :: S.ByteString
     -> [PCREOption]
     -> [S.ByteString]
     -> [Maybe [S.ByteString]]
     -> Test

testRegex regex options inputs outputs = testLabel regex $
  TestCase $ do
     assertEqual' "Input/Output Length Check" (length inputs) (length outputs)

     assertBool' "ByteString regex compile" =<<
      case compile regex options of
        r -> return $
            and [ match r i [] == o
                | (i,o) <- zip inputs outputs ]

     assertBool' "ByteString regex compileM" =<<
      case compileM regex options of
        Left s -> do S.hPutStrLn stderr ("ERROR in ByteString in compileM " `S.append` (S.pack s))
                     return False
        Right r -> return $
            and [ match r i [] == o
                | (i,o) <- zip inputs outputs ]

     assertBool' "String regex" =<<
      case String.compile (S.unpack regex) options of
        r -> return $
            and [ String.match r i [] == o
                | (i,o) <- zip (map (S.unpack) inputs)
                               (map (fmap (map S.unpack)) outputs) ]

     assertBool' "String regex" =<<
      case String.compileM (S.unpack regex) options of
        Left s -> do S.hPutStrLn stderr ("ERROR in String compileM: " `S.append` (S.pack s))
                     return False
        Right r -> return $
            and [ String.match r i [] == o
                | (i,o) <- zip (map (S.unpack) inputs)
                               (map (fmap (map S.unpack)) outputs) ]

testCaptures :: Int
     -> S.ByteString
     -> [(S.ByteString, Int)]
     -> Test
testCaptures expectedTotalCaptures regex expectedCaptureNames = testLabel regex $
    TestCase $ do
        r <- case compileM regex [] of
            Left s -> assertFailure ("ERROR in ByteString in compileM " ++ s)
            Right r -> return r
        assertEqual' "Capture group name mismatch" expectedCaptureNames (captureNames r)
        assertEqual' "Capture count mismatch" expectedTotalCaptures (captureCount r)

main :: IO ()
main = do counts <- runTestTT tests
          when (errors counts > 0 || failures counts > 0) exitFailure

tests :: Test
tests = TestList

    [ testRegex "the quick brown fox" []
           [ "the quick brown fox"
           , "The quick brown FOX"
           , "What do you know about the quick brown fox?"
           , "What do you know about THE QUICK BROWN FOX?"
           ]
           [ Just ["the quick brown fox"]
           , Nothing
           , Just ["the quick brown fox"]
           , Nothing
           ]

    , testLabel "compile failure" $
            TestCase $ (assertBool' "compile failure" $
                Left ("nothing to repeat" ) == compileM "*" [])

    , testLabel "compile failure" $
            TestCase $ (assertEqual' "compile failure"
                            (Just ("Text.Regex.PCRE.Light: Error in regex: nothing to repeat"))
                            (fmap (head . S.lines) . unsafePerformIO $ do
                                handle (\e -> return (Just (S.pack $ displayException (e :: SomeException))))
                                    (compile "*" [] `seq` return Nothing)))

--  , testRegex "\0*" [] -- the embedded null in the pattern seems to be a problem
--      ["\0\0\0\0"]
--      [Just ["\0\0\0\0"]]

    , testRegex "\1*" [] -- the embedded null in the pattern seems to be a problem
        ["\1\1\1\1"]
        [Just ["\1\1\1\1"]]

    , testRegex "The quick brown fox" [caseless]
           ["the quick brown fox"
           ,"The quick brown FOX"
           ,"What do you know about the quick brown fox?"
           ,"What do you know about THE QUICK BROWN FOX?"
           ]
           [ Just ["the quick brown fox"]
           , Just ["The quick brown FOX"]
           , Just ["the quick brown fox"]
           , Just ["THE QUICK BROWN FOX"]
           ]

     , testRegex "a*abc?xyz+pqr{3}ab{2,}xy{4,5}pq{0,6}AB{0,}zz" []
           ["abxyzpqrrrabbxyyyypqAzz"
           ,"abxyzpqrrrabbxyyyypqAzz"
           ,"aabxyzpqrrrabbxyyyypqAzz"
           ,"aaabxyzpqrrrabbxyyyypqAzz"
           ,"aaaabxyzpqrrrabbxyyyypqAzz"
           ,"abcxyzpqrrrabbxyyyypqAzz"
           ,"aabcxyzpqrrrabbxyyyypqAzz"
           ,"aaabcxyzpqrrrabbxyyyypAzz"
           ,"aaabcxyzpqrrrabbxyyyypqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqqqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqqqqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqqqqqAzz"
           ,"aaaabcxyzpqrrrabbxyyyypqAzz"
           ,"abxyzzpqrrrabbxyyyypqAzz"
           ,"aabxyzzzpqrrrabbxyyyypqAzz"
           ,"aaabxyzzzzpqrrrabbxyyyypqAzz"
           ,"aaaabxyzzzzpqrrrabbxyyyypqAzz"
           ,"abcxyzzpqrrrabbxyyyypqAzz"
           ,"aabcxyzzzpqrrrabbxyyyypqAzz"
           ,"aaabcxyzzzzpqrrrabbxyyyypqAzz"
           ,"aaaabcxyzzzzpqrrrabbxyyyypqAzz"
           ,"aaaabcxyzzzzpqrrrabbbxyyyypqAzz"
           ,"aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"
           ,"aaabcxyzpqrrrabbxyyyypABzz"
           ,"aaabcxyzpqrrrabbxyyyypABBzz"
           ,">>>aaabxyzpqrrrabbxyyyypqAzz"
           ,">aaaabxyzpqrrrabbxyyyypqAzz"
           ,">>>>abcxyzpqrrrabbxyyyypqAzz"
           ,"abxyzpqrrabbxyyyypqAzz"
           ,"abxyzpqrrrrabbxyyyypqAzz"
           ,"abxyzpqrrrabxyyyypqAzz"
           ,"aaaabcxyzzzzpqrrrabbbxyyyyyypqAzz"
           ,"aaaabcxyzzzzpqrrrabbbxyyypqAzz"
           ,"aaabcxyzpqrrrabbxyyyypqqqqqqqAzz"
           ]

           [ Just ["abxyzpqrrrabbxyyyypqAzz"]
           , Just ["abxyzpqrrrabbxyyyypqAzz"]
           , Just ["aabxyzpqrrrabbxyyyypqAzz"]
           , Just ["aaabxyzpqrrrabbxyyyypqAzz"]
           , Just ["aaaabxyzpqrrrabbxyyyypqAzz"]
           , Just ["abcxyzpqrrrabbxyyyypqAzz"]
           , Just ["aabcxyzpqrrrabbxyyyypqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqqqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqqqqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqqqqqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypqqqqqqAzz"]
           , Just ["aaaabcxyzpqrrrabbxyyyypqAzz"]
           , Just ["abxyzzpqrrrabbxyyyypqAzz"]
           , Just ["aabxyzzzpqrrrabbxyyyypqAzz"]
           , Just ["aaabxyzzzzpqrrrabbxyyyypqAzz"]
           , Just ["aaaabxyzzzzpqrrrabbxyyyypqAzz"]
           , Just ["abcxyzzpqrrrabbxyyyypqAzz"]
           , Just ["aabcxyzzzpqrrrabbxyyyypqAzz"]
           , Just ["aaabcxyzzzzpqrrrabbxyyyypqAzz"]
           , Just ["aaaabcxyzzzzpqrrrabbxyyyypqAzz"]
           , Just ["aaaabcxyzzzzpqrrrabbbxyyyypqAzz"]
           , Just ["aaaabcxyzzzzpqrrrabbbxyyyyypqAzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypABzz"]
           , Just ["aaabcxyzpqrrrabbxyyyypABBzz"]
           , Just ["aaabxyzpqrrrabbxyyyypqAzz"]
           , Just ["aaaabxyzpqrrrabbxyyyypqAzz"]
           , Just ["abcxyzpqrrrabbxyyyypqAzz"]
           , Nothing
           , Nothing
           , Nothing
           , Nothing
           , Nothing
           , Nothing
           ]

    , testRegex "^(abc){1,2}zz" []
        ["abczz"
        ,"abcabczz"
        ,"zz"
        ,"abcabcabczz"
        ,">>abczz"]
        [ Just ["abczz","abc"]
        , Just ["abcabczz", "abc"]
        , Nothing
        , Nothing
        , Nothing ]

    , testRegex "^(b+?|a){1,2}?c" []
        ["bc",
         "bbc",
         "bbbc",
         "bac",
         "bbac",
         "aac",
         "abbbbbbbbbbbc",
         "bbbbbbbbbbbac",
         "aaac",
         "abbbbbbbbbbbac"]

        [Just ["bc", "b"],
         Just ["bbc", "b"],
         Just ["bbbc", "bb"],
         Just ["bac", "a"],
         Just ["bbac", "a"],
         Just ["aac", "a"],
         Just ["abbbbbbbbbbbc", "bbbbbbbbbbb"],
         Just ["bbbbbbbbbbbac", "a"],
         Nothing,
         Nothing]

    , testRegex "^(b+|a){1,2}c" []
        ["bc",
         "bbc",
         "bbbc",
         "bac",
         "bbac",
         "aac",
         "abbbbbbbbbbbc",
         "bbbbbbbbbbbac",
         "aaac",
         "abbbbbbbbbbbac"]
        [Just ["bc", "b"],
         Just ["bbc", "bb"],
         Just ["bbbc", "bbb"],
         Just ["bac", "a"],
         Just ["bbac", "a"],
         Just ["aac", "a"],
         Just ["abbbbbbbbbbbc", "bbbbbbbbbbb"],
         Just ["bbbbbbbbbbbac", "a"],
         Nothing,
         Nothing]

    , testRegex "^(b+|a){1,2}?bc" []
        ["bbc"]
        [Just ["bbc", "b"]]

    , testRegex "^(b*|ba){1,2}?bc" []
        ["babc",
         "bbabc",
         "bababc",
         "bababbc",
         "babababc"]
        [Just ["babc","ba"],
         Just ["bbabc","ba"],
         Just ["bababc","ba"],
         Nothing,
         Nothing]

    , testRegex "^(ba|b*){1,2}?bc" []
        ["babc",
         "bbabc",
         "bababc",
         "bababbc",
         "babababc"]
        [Just ["babc","ba"],
         Just ["bbabc","ba"],
         Just ["bababc","ba"],
         Nothing,
         Nothing]

    , testRegex "^[ab\\]cde]" []
        ["athing",
         "bthing",
         "]thing",
         "cthing",
         "dthing",
         "ething",
         "fthing",
         "[thing",
         "\\\\thing"]
        [Just ["a"],
         Just ["b"],
         Just ["]"],
         Just ["c"],
         Just ["d"],
         Just ["e"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^[]cde]" []
        ["]thing",
         "cthing",
         "dthing",
         "ething",
         "athing",
         "fthing"]
        [Just ["]"],
         Just ["c"],
         Just ["d"],
         Just ["e"],
         Nothing,
         Nothing]

    , testRegex "^[^ab\\]cde]" []
        ["fthing",
         "[thing",
         "\\\\thing",
         "athing",
         "bthing",
         "]thing",
         "cthing",
         "dthing",
         "ething"]
        [Just ["f"],
         Just ["["],
         Just ["\\"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^\129" []
        ["\129"]
        [Just ["\x81"]]

    , testRegex "^\255" []
        ["\255"]
        [Just ["\xff"]]

    , testRegex "^[0-9]+$" []
        ["0",
         "1",
         "2",
         "3",
         "4",
         "5",
         "6",
         "7",
         "8",
         "9",
         "10",
         "100",
         "abc"]
        [Just ["0"],
         Just ["1"],
         Just ["2"],
         Just ["3"],
         Just ["4"],
         Just ["5"],
         Just ["6"],
         Just ["7"],
         Just ["8"],
         Just ["9"],
         Just ["10"],
         Just ["100"],
         Nothing]

    , testRegex "^.*nter" []
        ["enter",
         "inter",
         "uponter"]
        [Just ["enter"],
         Just ["inter"],
         Just ["uponter"]]

    , testRegex "^xxx[0-9]+$" []
        ["xxx0",
         "xxx1234",
         "xxx"]
        [Just ["xxx0"],
         Just ["xxx1234"],
         Nothing]

    , testRegex "^.+[0-9][0-9][0-9]$" []
        ["x123",
         "xx123",
         "123456",
         "123",
         "x1234"]
        [Just ["x123"],
         Just ["xx123"],
         Just ["123456"],
         Nothing,
         Just ["x1234"]]

    , testRegex "^.+?[0-9][0-9][0-9]$" []
        ["x123",
         "xx123",
         "123456",
         "123",
         "x1234"]
        [Just ["x123"],
         Just ["xx123"],
         Just ["123456"],
         Nothing,
         Just ["x1234"]]

    -- test matching more than 1 subpattern
    , testRegex "^([^!]+)!(.+)=apquxz\\.ixr\\.zzz\\.ac\\.uk$" []
        ["abc!pqr=apquxz.ixr.zzz.ac.uk",
         "!pqr=apquxz.ixr.zzz.ac.uk",
         "abc!=apquxz.ixr.zzz.ac.uk",
         "abc!pqr=apquxz:ixr.zzz.ac.uk",
         "abc!pqr=apquxz.ixr.zzz.ac.ukk"]
        [Just ["abc!pqr=apquxz.ixr.zzz.ac.uk", "abc", "pqr"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex ":" []
        ["Well, we need a colon: somewhere",
         "*** Fail if we don't"]
        [Just [":"],
         Nothing]

    , testRegex "([\\da-f:]+)$" [caseless]
        ["0abc",
         "abc",
         "fed",
         "E",
         "::",
         "5f03:12C0::932e",
         "fed def",
         "Any old stuff",
         "*** Failers",
         "0zzz",
         "gzzz",
         "fed\x20",
         "Any old rubbish"]
        [Just ["0abc", "0abc"],
         Just ["abc", "abc"],
         Just ["fed", "fed"],
         Just ["E", "E"],
         Just ["::", "::"],
         Just ["5f03:12C0::932e", "5f03:12C0::932e"],
         Just ["def", "def"],
         Just ["ff", "ff"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^.*\\.(\\d{1,3})\\.(\\d{1,3})\\.(\\d{1,3})$" []
        [".1.2.3",
         "A.12.123.0",
         ".1.2.3333",
         "1.2.3",
         "1234.2.3"]
        [Just [".1.2.3", "1", "2", "3"],
         Just ["A.12.123.0", "12", "123", "0"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^(\\d+)\\s+IN\\s+SOA\\s+(\\S+)\\s+(\\S+)\\s*\\(\\s*$" []
        ["1 IN SOA non-sp1 non-sp2(",
         "1    IN    SOA    non-sp1    non-sp2   (",
         "1IN SOA non-sp1 non-sp2("]
        [Just ["1 IN SOA non-sp1 non-sp2(", "1", "non-sp1", "non-sp2"],
         Just ["1    IN    SOA    non-sp1    non-sp2   (", "1", "non-sp1", "non-sp2"],
         Nothing]

    , testRegex "^[a-zA-Z\\d][a-zA-Z\\d\\-]*(\\.[a-zA-Z\\d][a-zA-z\\d\\-]*)*\\.$" []
        ["a.",
         "Z.",
         "2.",
         "ab-c.pq-r.",
         "sxk.zzz.ac.uk.",
         "x-.y-.",
         "*** Failers",
         "-abc.peq."]
        [Just ["a."],
         Just ["Z."],
         Just ["2."],
         Just ["ab-c.pq-r.", ".pq-r"],
         Just ["sxk.zzz.ac.uk.", ".uk"],
         Just ["x-.y-.", ".y-"],
         Nothing,
         Nothing]

    , testRegex "^\\*\\.[a-z]([a-z\\-\\d]*[a-z\\d]+)?(\\.[a-z]([a-z\\-\\d]*[a-z\\d]+)?)*$" []
        ["*.a",
         "*.b0-a",
         "*.c3-b.c",
         "*.c-a.b-c",
         "*** Failers",
         "*.0",
         "*.a-",
         "*.a-b.c-",
         "*.c-a.0-c"]
        [Just ["*.a"],
         Just ["*.b0-a", "0-a"],
         Just ["*.c3-b.c", "3-b", ".c"],
         Just ["*.c-a.b-c", "-a", ".b-c", "-c"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^(?=ab(de))(abd)(e)" []
        ["abde"]
        [Just ["abde", "de", "abd", "e"]]

    , testRegex "^(?!(ab)de|x)(abd)(f)" []
        ["abdf"]
        [Just ["abdf", "", "abd", "f"]]

    , testRegex "^(?=(ab(cd)))(ab)" []
        ["abcd"]
        [Just ["ab", "abcd", "cd", "ab"]]

    , testRegex "^[\\da-f](\\.[\\da-f])*$" [caseless]
        ["a.b.c.d",
         "A.B.C.D",
         "a.b.c.1.2.3.C"]
        [Just ["a.b.c.d", ".d"],
         Just ["A.B.C.D", ".D"],
         Just ["a.b.c.1.2.3.C", ".C"]]

    , testRegex "^\".*\"\\s*(;.*)?$" []
        ["\"1234\"",
         "\"abcd\" ;",
         "\"\" ; rhubarb",
         "*** Failers",
         "\\\"1234\\\" : things"]
        [Just ["\"1234\""],
         Just ["\"abcd\" ;", ";"],
         Just ["\"\" ; rhubarb", "; rhubarb"],
         Nothing,
         Nothing]

    , testRegex "^$" []
        ["",
         "*** Failers"]
        [Just [""],
         Nothing]

    , testRegex "   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)" [extended]
        ["ab c",
         "*** Failers",
         "abc",
         "ab cde"]
        [Just ["ab c"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "(?x)   ^    a   (?# begins with a)  b\\sc (?# then b c) $ (?# then end)" []
        ["ab c",
         "*** Failers",
         "abc",
         "ab cde"]
        [Just ["ab c"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^   a\\ b[c ]d       $" [extended]
        ["a bcd",
         "a b d",
         "*** Failers",
         "abcd",
         "ab d"]
        [Just ["a bcd"],
         Just ["a b d"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^(a(b(c)))(d(e(f)))(h(i(j)))(k(l(m)))$" []
        ["abcdefhijklm"]
        [Just ["abcdefhijklm",
               "abc", "bc",
               "c", "def",
               "ef", "f",
               "hij", "ij",
               "j", "klm",
               "lm", "m"]]

    , testRegex "^(?:a(b(c)))(?:d(e(f)))(?:h(i(j)))(?:k(l(m)))$" []
        ["abcdefhijklm"]
        [Just ["abcdefhijklm",
             "bc", "c", "ef", "f", "ij", "j", "lm", "m"]]

    , testRegex "^[.^$|()*+?{,}]+" []
        [".^$(*+)|{?,?}"]
        [Just [".^$(*+)|{?,?}"]]

    , testRegex "^a*\\w" []
        ["z",
         "az",
         "aaaz",
         "a",
         "aa",
         "aaaa",
         "a+",
         "aa+"]
        [Just ["z"],
         Just ["az"],
         Just ["aaaz"],
         Just ["a"],
         Just ["aa"],
         Just ["aaaa"],
         Just ["a"],
         Just ["aa"]]

    , testRegex "^a*?\\w" []
        ["z",
         "az",
         "aaaz",
         "a",
         "aa",
         "aaaa",
         "a+",
         "aa+"]
        [Just ["z"],
         Just ["a"],
         Just ["a"],
         Just ["a"],
         Just ["a"],
         Just ["a"],
         Just ["a"],
         Just ["a"]]

    , testRegex "^a+\\w" []
        ["az",
         "aaaz",
         "aa",
         "aaaa",
         "aa+"]
        [Just ["az"],
         Just ["aaaz"],
         Just ["aa"],
         Just ["aaaa"],
         Just ["aa"]]

    , testRegex "^a+?\\w" []
        ["az",
         "aaaz",
         "aa",
         "aaaa",
         "aa+"]
        [Just ["az"],
         Just ["aa"],
         Just ["aa"],
         Just ["aa"],
         Just ["aa"]]

    , testRegex "^\\d{8}\\w{2,}" []
        ["1234567890",
         "12345678ab",
         "12345678__",
         "*** Failers",
         "1234567"]
        [Just ["1234567890"],
         Just ["12345678ab"],
         Just ["12345678__"],
         Nothing,
         Nothing]

    , testRegex "^[aeiou\\d]{4,5}$" []
        ["uoie",
         "1234",
         "12345",
         "aaaaa",
         "*** Failers",
         "123456"]
        [Just ["uoie"],
         Just ["1234"],
         Just ["12345"],
         Just ["aaaaa"],
         Nothing,
         Nothing]

    , testRegex "^[aeiou\\d]{4,5}?" []
        ["uoie",
         "1234",
         "12345",
         "aaaaa",
         "123456"]
        [Just ["uoie"],
         Just ["1234"],
         Just ["1234"],
         Just ["aaaa"],
         Just ["1234"]]

    , testRegex "\\A(abc|def)=(\\1){2,3}\\Z" []
        ["abc=abcabc",
         "def=defdefdef",
         "*** Failers",
         "abc=defdef"]
        [Just ["abc=abcabc", "abc", "abc"],
         Just ["def=defdefdef", "def", "def"],
         Nothing,
         Nothing]

    , testRegex "^(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\\11*(\\3\\4)\\1(?#)2$" []
        ["abcdefghijkcda2",
         "abcdefghijkkkkcda2"]
        [Just ["abcdefghijkcda2", "a", "b",
         "c", "d", "e", "f", "g", "h", "i", "j", "k", "cd"],

         Just ["abcdefghijkkkkcda2", "a", "b", "c", "d",
             "e", "f", "g", "h", "i", "j", "k", "cd"]]

    , testRegex "(cat(a(ract|tonic)|erpillar)) \\1()2(3)" []
        ["cataract cataract23",
         "catatonic catatonic23",
         "caterpillar caterpillar23"]

        [Just ["cataract cataract23", "cataract", "aract", "ract", "", "3"],

         Just ["catatonic catatonic23", "catatonic", "atonic", "tonic", "", "3"],

         Just ["caterpillar caterpillar23", "caterpillar", "erpillar", "", "", "3"]]

    , testRegex "^From +([^ ]+) +[a-zA-Z][a-zA-Z][a-zA-Z] +[a-zA-Z][a-zA-Z][a-zA-Z] +[0-9]?[0-9] +[0-9][0-9]:[0-9][0-9]" []
        ["From abcd  Mon Sep 01 12:33:02 1997"]
        [Just ["From abcd  Mon Sep 01 12:33", "abcd"]]

    , testRegex "^From\\s+\\S+\\s+([a-zA-Z]{3}\\s+){2}\\d{1,2}\\s+\\d\\d:\\d\\d" []
        ["From abcd  Mon Sep 01 12:33:02 1997",
         "From abcd  Mon Sep  1 12:33:02 1997",
         "*** Failers",
         "From abcd  Sep 01 12:33:02 1997"]
        [Just ["From abcd  Mon Sep 01 12:33", "Sep "],
         Just ["From abcd  Mon Sep  1 12:33", "Sep  "],
         Nothing,
         Nothing]

    , testRegex "\\w+(?=\t)" []
        ["the quick brown\t fox"]
        [Just ["brown"]]

    , testRegex "foo(?!bar)(.*)" []
        ["foobar is foolish see?"]
        [Just ["foolish see?", "lish see?"]]

    , testRegex "(?:(?!foo)...|^.{0,2})bar(.*)" []
        ["foobar crowbar etc",
         "barrel",
         "2barrel",
         "A barrel"]
        [Just ["rowbar etc", " etc"],
         Just ["barrel",     "rel"],
         Just ["2barrel",    "rel"],
         Just ["A barrel",   "rel"]]

    , testRegex "^(\\D*)(?=\\d)(?!123)" []
        ["abc456",
         "*** Failers",
         "abc123"]
        [Just ["abc", "abc"],
         Nothing,
         Nothing]

    , testRegex "^(a)\\1{2,3}(.)" []
        ["aaab",
         "aaaab",
         "aaaaab",
         "aaaaaab"]
        [Just ["aaab", "a","b"],
         Just ["aaaab","a","b"],
         Just ["aaaaa","a","a"],
         Just ["aaaaa","a","a"]]

    , testRegex "(?!^)abc" []
        ["the abc",
         "*** Failers",
         "abc"]
        [Just ["abc"],
         Nothing,
         Nothing]

    , testRegex "(?=^)abc" []
        ["abc",
         "*** Failers",
         "the abc"]
        [Just ["abc"],
         Nothing,
         Nothing]

    , testRegex "^[ab]{1,3}(ab*|b)" []
        ["aabbbbb"]
        [Just ["aabb", "b"]]

    , testRegex "^[ab]{1,3}?(ab*|b)" []
        ["aabbbbb"]
        [Just ["aabbbbb", "abbbbb"]]

    , testRegex "^[ab]{1,3}?(ab*?|b)" []
        ["aabbbbb"]
        [Just ["aa", "a"]]

    , testRegex "^[ab]{1,3}(ab*?|b)" []
        ["aabbbbb"]
        [Just ["aabb", "b"]]

    , testRegex "^(cow|)\\1(bell)" []
        ["cowcowbell",
         "bell",
         "*** Failers",
         "cowbell"]
        [Just ["cowcowbell", "cow", "bell"],
         Just ["bell", "", "bell"],
         Nothing,
         Nothing]

    , testRegex "^\\s" []
        ["\o40abc",
         "\nabc",
         "\rabc",
         "\tabc",
         "abc"]
        [Just [" "],
         Just ["\x0a"],
         Just ["\x0d"],
         Just ["\x09"],
         Nothing]

    , testRegex "^(a|)\\1*b" []
        ["ab",
         "aaaab",
         "b",
         "acb"]
        [Just ["ab", "a"],
         Just ["aaaab", "a"],
         Just ["b", ""],
         Nothing]

    , testRegex "^(a|)\\1+b" []
        ["aab",
         "aaaab",
         "b",
         "*** Failers",
         "ab"]
        [Just ["aab", "a"],
         Just ["aaaab", "a"],
         Just ["b", ""],
         Nothing,
         Nothing]

    , testRegex "^(a|)\\1?b" []
        ["ab",
         "aab",
         "b",
         "acb"]
        [Just ["ab", "a"],
         Just ["aab", "a"],
         Just ["b", ""],
         Nothing]

    , testRegex "^(a|)\\1{2}b" []
        ["aaab",
         "b",
         "ab",
         "aab",
         "aaaab"]
        [Just ["aaab", "a"],
         Just ["b", ""],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^(a|)\\1{2,3}b" []
        ["aaab",
         "aaaab",
         "b",
         "ab",
         "aab",
         "aaaaab"]
        [Just ["aaab", "a"],
         Just ["aaaab", "a"],
         Just ["b", ""],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "ab{1,3}bc" []
        ["abbbbc",
         "abbbc",
         "abbc",
         "abc",
         "abbbbbc"]
        [Just ["abbbbc"],
         Just ["abbbc"],
         Just ["abbc"],
         Nothing,
         Nothing]

    , testRegex "([^.]*)\\.([^:]*):[T ]+(.*)" []
        ["track1.title:TBlah blah blah"]
        [Just ["track1.title:TBlah blah blah", "track1", "title", "Blah blah blah"]]

    , testRegex "([^.]*)\\.([^:]*):[T ]+(.*)" [caseless]
        ["track1.title:TBlah blah blah"]
        [Just ["track1.title:TBlah blah blah", "track1", "title", "Blah blah blah"]]

    , testRegex "([^.]*)\\.([^:]*):[t ]+(.*)" [caseless]
        ["track1.title:TBlah blah blah"]
        [Just ["track1.title:TBlah blah blah", "track1", "title", "Blah blah blah"]]

    , testRegex "^[W-c]+$" []
        ["WXY_^abc",
         "wxy"]
        [Just ["WXY_^abc"],
         Nothing]

    , testRegex "^[W-c]+$" [caseless]
        ["WXY_^abc",
         "wxy_^ABC"]
        [Just ["WXY_^abc"],
         Just ["wxy_^ABC"]]

    , testRegex "^[\\x3f-\\x5F]+$" [caseless]
        ["WXY_^abc",
         "wxy_^ABC"]
        [Just ["WXY_^abc"],
         Just ["wxy_^ABC"]]

    , testRegex "^abc$" []
        ["abc",
         "qqq\\nabc",
         "abc\\nzzz",
         "qqq\\nabc\\nzzz"]
        [Just ["abc"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "(?:b)|(?::+)" []
        ["b::c",
         "c::b"]
        [Just ["b"],
         Just ["::"]]

    , testRegex "[-az]+" []
        ["az-",
         "*** Failers",
         "b"]
        [Just ["az-"],
         Just ["a"],
         Nothing]

    , testRegex "[az-]+" []
        ["za-",
         "*** Failers",
         "b"]
        [Just ["za-"],
         Just ["a"],
         Nothing]

    , testRegex "[a\\-z]+" []
        ["a-z",
         "*** Failers",
         "b"]
        [Just ["a-z"],
         Just ["a"],
         Nothing]

    , testRegex "[a-z]+" []
        ["abcdxyz"]
        [Just ["abcdxyz"]]

    , testRegex "[\\d-]+" []
        ["12-34",
         "aaa"]
        [Just ["12-34"],
         Nothing]
    , testRegex "[\\d-z]+" []
        ["12-34z",
         "aaa"]
        [Just ["12-34z"],
         Nothing]

    , testRegex "\\x20Z" []
        ["the Zoo",
         "*** Failers",
         "Zulu"]
        [Just [" Z"],
         Nothing,
         Nothing]

    , testRegex "(abc)\\1" [caseless]
        ["abcabc",
         "ABCabc",
         "abcABC"]
        [Just ["abcabc", "abc"],
         Just ["ABCabc", "ABC"],
         Just ["abcABC", "abc"]]

    , testRegex "ab{3cd" []
        ["ab{3cd"]
        [Just ["ab{3cd"]]

    , testRegex "ab{3,cd" []
        ["ab{3,cd"]
        [Just ["ab{3,cd"]]

    , testRegex "ab{3,4a}cd" []
        ["ab{3,4a}cd"]
        [Just ["ab{3,4a}cd"]]

    , testRegex "{4,5a}bc" []
        ["{4,5a}bc"]
        [Just ["{4,5a}bc"]]

    , testRegex "abc$" []
        ["abc",
         "abc\n",
         "*** Failers",
         "abc\ndef"]
        [Just ["abc"],
         Just ["abc"],
         Nothing,
         Nothing]

    , testRegex "(abc)\\123" []
        ["abc\x53"]
        [Just ["abcS", "abc"]]

    , testRegex "(abc)\\223" []
        ["abc\x93"]
        [Just ["abc\x93", "abc"]]

    , testRegex "(abc)\\323" []
        ["abc\xd3"]
        [Just ["abc\xd3", "abc"]]

    , testRegex "(abc)\\100" []
        ["abc\x40",
         "abc\o100"]
        [Just ["abc@", "abc"],
         Just ["abc@", "abc"]]

    , testRegex "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)(l)\\12\\123" []
        ["abcdefghijkllS"]
        [Just ["abcdefghijkllS",
             "a",
             "b",
             "c",
             "d",
             "e",
             "f",
             "g",
             "h",
             "i",
             "j",
             "k",
             "l"]]

     , testRegex "(a)(b)(c)(d)(e)(f)(g)(h)(i)(j)(k)\\12\\123" []
        ["abcdefghijk\o12S"]
        [Just ["abcdefghijk\x0aS",
             "a",
             "b",
             "c",
             "d",
             "e",
             "f",
             "g",
             "h",
             "i",
             "j",
             "k"]]

    , testRegex "ab\\idef" []
        ["abidef"]
        [Just ["abidef"]]

    , testRegex "a{0}bc" []
        ["bc"]
        [Just ["bc"]]

    , testRegex "(a|(bc)){0,0}?xyz" []
        ["xyz"]
        [Just ["xyz"]]

    , testRegex "(?s)a.b" []
        ["a\nb"]
        [Just ["a\nb"]]

    , testRegex "^([^a])([^\\b])([^c]*)([^d]{3,4})" []
        ["baNOTccccd",
         "baNOTcccd",
         "baNOTccd",
         "bacccd",
         "anything",
         "b\bc   ",
         "baccd"]
        [Just ["baNOTcccc", "b", "a", "NOT", "cccc"],
         Just ["baNOTccc", "b", "a", "NOT", "ccc"],
         Just ["baNOTcc", "b", "a", "NO", "Tcc"],
         Just ["baccc", "b", "a", "", "ccc"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "^\\d{8,}\\@.+[^k]$" []
        ["12345678@a.b.c.d",
         "123456789@x.y.z",
         "*** Failers",
         "12345678@x.y.uk",
         "1234567@a.b.c.d       "]
        [Just ["12345678@a.b.c.d"],
         Just ["123456789@x.y.z"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "(a)\\1{8,}" []
        ["aaaaaaaaa",
         "aaaaaaaaaa",
         "*** Failers",
         "aaaaaaa   "]
        [Just ["aaaaaaaaa", "a"],
         Just ["aaaaaaaaaa", "a"],
         Nothing,
         Nothing]

    , testRegex "[^a]" []
        ["aaaabcd",
         "aaAabcd "]
        [Just ["b"],
         Just ["A"]]

    , testRegex "[^a]" [caseless]
        ["aaaabcd",
         "aaAabcd "]
        [Just ["b"],
         Just ["b"]]

    , testRegex "[^az]" []
        ["aaaabcd",
         "aaAabcd "]
        [Just ["b"],
         Just ["A"]]

    , testRegex "[^az]" [caseless]
        ["aaaabcd",
         "aaAabcd "]
        [Just ["b"],
         Just ["b"]]

    , testRegex "P[^*]TAIRE[^*]{1,6}?LL" []
        ["xxxxxxxxxxxPSTAIREISLLxxxxxxxxx"]
        [Just ["PSTAIREISLL"]]

    , testRegex "P[^*]TAIRE[^*]{1,}?LL" []
        ["xxxxxxxxxxxPSTAIREISLLxxxxxxxxx"]
        [Just ["PSTAIREISLL"]]

    , testRegex "(.*?)(\\d+)" []
        ["I have 2 numbers: 53147"]
        [Just ["I have 2", "I have ", "2"]]

    , testRegex "(.*)(\\d+)$" []
        ["I have 2 numbers: 53147"]
        [Just ["I have 2 numbers: 53147", "I have 2 numbers: 5314", "7"]]

    , testRegex "(.*?)(\\d+)$" []
        ["I have 2 numbers: 53147"]
        [Just ["I have 2 numbers: 53147", "I have 2 numbers: ", "53147"]]

    , testRegex "(.*)\\b(\\d+)$" []
        ["I have 2 numbers: 53147"]
        [Just ["I have 2 numbers: 53147", "I have 2 numbers: ", "53147"]]

    , testRegex "(.*\\D)(\\d+)$" []
        ["I have 2 numbers: 53147"]
        [Just ["I have 2 numbers: 53147", "I have 2 numbers: ", "53147"]]

    , testRegex "word (?:[a-zA-Z0-9]+ ){0,10}otherword" []
        ["word cat dog elephant mussel cow horse canary baboon snake shark otherword",
         "word cat dog elephant mussel cow horse canary baboon snake shark"]
        [Just ["word cat dog elephant mussel cow horse canary baboon snake shark otherword"],
         Nothing]

    , testRegex "word (?:[a-zA-Z0-9]+ ){0,300}otherword" []
        ["word cat dog elephant mussel cow horse canary baboon snake shark the quick brown fox and the lazy dog and several other words getting close to thirty by now I hope"]
        [Nothing]
    , testRegex "^(a){0,0}" []
        ["bcd",
         "abc",
         "aab     "]
        [Just [""],
         Just [""],
         Just [""]]

    , testRegex "^(a){0,1}" []
        ["bcd",
         "abc",
         "aab  "]
        [Just [""],
         Just ["a", "a"],
         Just ["a", "a"]]

    , testRegex "^(a){0,2}" []
        ["bcd",
         "abc",
         "aab  "]
        [Just [""],
         Just ["a", "a"],
         Just ["aa", "a"]]

    , testRegex "^(a){0,3}" []
        ["bcd",
         "abc",
         "aab",
         "aaa   "]
        [Just [""],
         Just ["a", "a"],
         Just ["aa", "a"],
         Just ["aaa", "a"]]

    , testRegex "^(a){0,3}" []
        ["bcd",
         "abc",
         "aab",
         "aaa   "]
        [Just [""],
         Just ["a", "a"],
         Just ["aa", "a"],
         Just ["aaa", "a"]]

    , testRegex "^(a){0,}" []
        ["bcd",
         "abc",
         "aab",
         "aaa",
         "aaaaaaaa    "]
        [Just [""],
         Just ["a", "a"],
         Just ["aa", "a"],
         Just ["aaa", "a"],
         Just ["aaaaaaaa", "a"]]

    , testRegex "^(a){1,1}" []
        ["bcd",
         "abc",
         "aab  "]
        [Nothing,
         Just ["a", "a"],
         Just ["a", "a"]]

    , testRegex "^(a){1,2}" []
        ["bcd",
         "abc",
         "aab  "]
        [Nothing,
         Just ["a", "a"],
         Just ["aa", "a"]]

    , testRegex "^(a){1,3}" []
        ["bcd",
         "abc",
         "aab",
         "aaa   "]
        [Nothing,
         Just ["a", "a"],
         Just ["aa", "a"],
         Just ["aaa", "a"]]

    , testRegex ".*\\.gif" []
        ["borfle\nbib.gif\nno"]
        [Just ["bib.gif"]]

    , testRegex ".{0,}\\.gif" []
        ["borfle\nbib.gif\nno"]
        [Just ["bib.gif"]]

    , testRegex ".*\\.gif" [multiline]
        ["borfle\nbib.gif\nno"]
        [Just ["bib.gif"]]

    , testRegex ".*\\.gif" [dotall]
        ["borfle\nbib.gif\nno"]
        [Just ["borfle\nbib.gif"]]

    , testRegex ".*$" [multiline]
        ["borfle\nbib.gif\nno"]
        [Just ["borfle"]]

    , testRegex ".*$" [dotall]
        ["borfle\nbib.gif\nno"]
        [Just ["borfle\nbib.gif\nno"]]

    , testRegex ".*$" [multiline]
        ["borfle\nbib.gif\nno\\n"]
        [Just ["borfle"]]

    , testRegex "(?ms)^B" []
        ["abc\nB"]
        [Just ["B"]]

    , testRegex "(?s)B$" []
        ["B\n"]
        [Just ["B"]]

    , testRegex "^[abcdefghijklmnopqrstuvwxy0123456789]" []
        ["n",
         "z "]
        [Just ["n"],
         Nothing]

    , testRegex "abcde{0,0}" []
        ["abcd",
         "abce  "]
        [Just ["abcd"],
         Nothing]

    , testRegex "^(b+?|a){1,2}?c" []
        ["bac",
         "bbac",
         "bbbac",
         "bbbbac",
         "bbbbbac "]
        [Just ["bac","a"],
         Just ["bbac","a"],
         Just ["bbbac","a"],
         Just ["bbbbac","a"],
         Just ["bbbbbac","a"]]

    , testRegex "(AB)*?\\1" []
        ["ABABAB"]
        [Just ["ABAB", "AB"]]

    , testRegex "(.*(.)?)*" []
        ["abcd"]
        [Just ["abcd", ""]]

{-
    , testRegex "(?:a|)*\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]
        [Nothing,
         Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]]
-}

    , testRegex "^(?:a(?:(?:))+)+" []
        ["aaaa"]
        [Just ["aaaa"]]

    , testRegex "^(a()+)+" []
        ["aaaa"]
        [Just ["aaaa", "a", ""]]

    , testRegex "^(?:a(?:(?:))*)*" []
        ["aaaa"]
        [Just ["aaaa"]]

    , testRegex "^(a()*)*" []
        ["aaaa"]
        [Just ["aaaa", "a", ""]]

    , testRegex "^(a){1,}" []
        ["bcd",
         "abc",
         "aab",
         "aaa",
         "aaaaaaaa    "]
        [Nothing,
         Just ["a", "a"],
         Just ["aa", "a"],
         Just ["aaa", "a"],
         Just ["aaaaaaaa", "a"]]

    , testRegex "(?s)(.*X|^B)" []
        ["abcde\n1234Xyz",
         "BarFoo ",
         "*** Failers ",
         "abcde\nBar  "]
        [Just ["abcde\n1234X", "abcde\n1234X"],
         Just ["B", "B"],
         Nothing,
         Nothing]

    , testRegex "(?s:.*X|^B)" []
        ["abcde\n1234Xyz",
         "BarFoo ",
         "*** Failers ",
         "abcde\nBar  "]
        [Just ["abcde\n1234X"],
         Just ["B"],
         Nothing,
         Nothing]

    , testRegex "\\w{3}(?<!bar)foo" []
        ["catfood",
         "*** Failers",
         "foo",
         "barfoo",
         "towbarfoo"]
        [Just ["catfoo"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex "(?>(\\.\\d\\d[1-9]?))\\d+" []
        ["1.230003938",
         "1.875000282",
         "1.235 "]
        [Just [".230003938", ".23"],
         Just [".875000282", ".875"],
         Nothing]

    , testRegex "^((?>\\w+)|(?>\\s+))*$" []
        ["now is the time for all good men to come to the aid of the party",
         "this is not a line with only words and spaces!"]
        [Just ["now is the time for all good men to come to the aid of the party", "party"],
         Nothing]

    , testRegex "((?>\\d+))(\\w)" []
        ["12345a",
         "12345+ "]
        [Just ["12345a", "12345", "a"],
         Nothing]

    , testRegex "(?>a+)b" []
        ["aaab"]
        [Just ["aaab"]]

    , testRegex "((?>a+)b)" []
        ["aaab"]
        [Just ["aaab", "aaab"]]

    , testRegex "(?>(a+))b" []
        ["aaab"]
        [Just ["aaab", "aaa"]]
    , testRegex "(?>b)+" []
        ["aaabbbccc"]
        [Just ["bbb"]]

    , testRegex "(?>a+|b+|c+)*c" []
        ["aaabbbbccccd"]
        [Just ["aaabbbbc"]]

    , testRegex "(?:(a)|b)(?(1)A|B)" []
        ["aA",
         "bB",
         "aB",
         "bA    "]
        [Just ["aA", "a"],
         Just ["bB"],
         Nothing,
         Nothing]

    , testRegex "^(a)?(?(1)a|b)+$" []
        ["aa",
         "b",
--       "bb  ", -- ?
         "ab   "]
        [Just ["aa", "a"],
         Just ["b"],
--       Just ["bb"],
         Nothing]

    , testRegex "^(?(?=abc)\\w{3}:|\\d\\d)$" []
        ["abc:",
         "12",
         "123",
         "xyz    "]
        [Just ["abc:"],
         Just ["12"],
         Nothing,
         Nothing]

    , testRegex "(?(?<!foo)cat|bar)" []
        ["foobar",
         "cat",
         "fcat",
         "focat   ",
         "foocat  "]
        [Just ["bar"],
         Just ["cat"],
         Just ["cat"],
         Just ["cat"],
         Nothing]

    , testRegex "^(?(2)a|(1)(2))+$" []
        ["12",
         "12a",
         "12aa",
         "*** Failers",
         "1234    "]
        [Just ["12", "1", "2"],
         Just ["12a", "1", "2"],
         Just ["12aa", "1", "2"],
         Nothing,
         Nothing]

    , testRegex "(?<=foo\\n)^bar" [multiline]
        ["foo\nbar",
         "*** Failers",
         "bar",
         "baz\nbar   "]
        [Just ["bar"],
         Nothing,
         Nothing,
         Nothing]

    , testRegex "(?<=(?<!foo)bar)baz" []
        ["barbaz",
         "barbarbaz ",
         "koobarbaz ",
         "*** Failers",
         "baz",
         "foobarbaz "]
        [Just ["baz"],
         Just ["baz"],
         Just ["baz"],
         Nothing,
         Nothing,
         Nothing]

{-
    , testRegex "^(a\\1?){4}$" []
        ["a",
         "aa",
         "aaa",
         "aaaa",
         "aaaaa",
         "aaaaaaa",
         "aaaaaaaa",
         "aaaaaaaaa",
         "aaaaaaaaaa",
         "aaaaaaaaaaa",
         "aaaaaaaaaaaa",
         "aaaaaaaaaaaaa",
         "aaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaa               "]
        [Nothing,
         Nothing,
         Nothing,
         Just ["aaaa", "a"],
         Just ["aaaaa", "a"],
         Just ["aaaaaaa", "a"],
         Nothing,
         Nothing,
         Just ["aaaaaaaaaa", "aaaa"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]
-}

    , testRegex "abc" []
        ["abc",
         "xabcy",
         "ababc",
         "*** Failers",
         "xbc",
         "axc",
         "abx"]
        [Just ["abc"],
         Just ["abc"],
         Just ["abc"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    , testRegex "ab*c" []
        ["abc"]
        [Just ["abc"]]
    , testRegex "ab*bc" []
        ["abc",
         "abbc",
         "abbbbc"]
        [Just ["abc"],
         Just ["abbc"],
         Just ["abbbbc"]]

    , testRegex ".{1}" []
        ["abbbbc"]
        [Just ["a"]]

    , testRegex ".{3,4}" []
        ["abbbbc"]
        [Just ["abbb"]]

    , testRegex "ab{0,}bc" []
        ["abbbbc"]
        [Just ["abbbbc"]]

    , testRegex "ab+bc" []
        ["abbc",
         "abc",
         "abq"]
        [Just ["abbc"],
         Nothing,
         Nothing]

    , testRegex "ab{1,}bc" []
        []
        []

    , testRegex "ab+bc" []
        ["abbbbc"]
        [Just ["abbbbc"]]

    , testRegex "ab{1,}bc" []
        ["abbbbc"]
        [Just ["abbbbc"]]

    , testRegex "ab{1,3}bc" []
        ["abbbbc"]
        [Just ["abbbbc"]]

    , testRegex "ab{3,4}bc" []
        ["abbbbc"]
        [Just ["abbbbc"]]

    , testRegex "ab{4,5}bc" []
        ["*** Failers",
         "abq",
         "abbbbc"]
        [Nothing,
         Nothing,
         Nothing]

    , testRegex "ab?bc" []
        ["abbc",
         "abc"]
        [Just ["abbc"],
         Just ["abc"]]

    , testRegex "ab{0,1}bc" []
        ["abc"]
        [Just ["abc"]]

    , testRegex "ab?bc" []
        []
        []

    , testRegex "ab?c" []
        ["abc"]
        [Just ["abc"]]

    , testRegex "ab{0,1}c" []
        ["abc"]
        [Just ["abc"]]

    , testRegex "^abc$" []
        ["abc",
         "abbbbc",
         "abcc"]
        [Just ["abc"],
         Nothing,
         Nothing]

    , testRegex "^abc" []
        ["abcc"]
        [Just ["abc"]]

    , testRegex "^abc$" []
        []
        []

    , testRegex "abc$" []
        ["aabc",
         "*** Failers",
         "aabc",
         "aabcd"]
        [Just ["abc"],
         Nothing,
         Just ["abc"],
         Nothing]

    , testRegex "^" []
        ["abc"]
        [Just [""]]
    ,

    testRegex "$" []
        ["abc"]
        [Just [""]]
    ,

    testRegex "a.c" []
        ["abc",
         "axc"]
        [Just ["abc"],
         Just ["axc"]]
    ,

    testRegex "a.*c" []
        ["axyzc"]
        [Just ["axyzc"]]
    ,

    testRegex "a[bc]d" []
        ["abd",
         "*** Failers",
         "axyzd",
         "abc"]
        [Just ["abd"],
         Nothing,
         Nothing,
         Nothing]
    ,

    testRegex "a[b-d]e" []
        ["ace"]
        [Just ["ace"]]
    ,

    testRegex "a[b-d]" []
        ["aac"]
        [Just ["ac"]]
    ,

    testRegex "a[-b]" []
        ["a-"]
        [Just ["a-"]]
    ,

    testRegex "a[b-]" []
        ["a-"]
        [Just ["a-"]]
    ,


    testRegex "a]" []
        ["a]"]
        [Just ["a]"]]
    ,


    testRegex "a[]]b" []
        ["a]b"]
        [Just ["a]b"]]
    ,

    testRegex "a[^bc]d" []
        ["aed",
         "*** Failers",
         "abd",
         "abd"]
        [Just ["aed"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "a[^-b]c" []
        ["adc"]
        [Just ["adc"]]
    ,

    testRegex "a[^]b]c" []
        ["adc",
         "*** Failers",
         "a-c",
         "a]c"]
        [Just ["adc"],
         Nothing,
         Just ["a-c"],
         Nothing]
    ,


    testRegex "\\ba\\b" []
        ["a-",
         "-a",
         "-a-"]
        [Just ["a"],
         Just ["a"],
         Just ["a"]]
    ,


    testRegex "\\by\\b" []
        ["*** Failers",
         "xy",
         "yz",
         "xyz"]
        [Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "\\Ba\\B" []
        ["*** Failers",
         "a-",
         "-a",
         "-a-"]
        [Just ["a"],
         Nothing,
         Nothing,
         Nothing]
    ,

    testRegex "\\By\\b" []
        ["xy"]
        [Just ["y"]]
    ,


    testRegex "\\by\\B" []
        ["yz"]
        [Just ["y"]]
    ,


    testRegex "\\By\\B" []
        ["xyz"]
        [Just ["y"]]
    ,

    testRegex "\\w" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "\\W" []
        ["-",
         "*** Failers",
         "-",
         "a"]
        [Just ["-"],
         Just ["*"],
         Just ["-"],
         Nothing]
    ,


    testRegex "a\\sb" []
        ["a b"]
        [Just ["a b"]]
    ,


    testRegex "a\\Sb" []
        ["a-b",
         "*** Failers",
         "a-b",
         "a b"]
        [Just ["a-b"],
         Nothing,
         Just ["a-b"],
         Nothing]
    ,


    testRegex "\\d" []
        ["1"]
        [Just ["1"]]
    ,


    testRegex "\\D" []
        ["-",
         "*** Failers",
         "-",
         "1"]
        [Just ["-"],
         Just ["*"],
         Just ["-"],
         Nothing]
    ,


    testRegex "[\\w]" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "[\\W]" []
        ["-",
         "*** Failers",
         "-",
         "a"]
        [Just ["-"],
         Just ["*"],
         Just ["-"],
         Nothing]
    ,


    testRegex "a[\\s]b" []
        ["a b"]
        [Just ["a b"]]
    ,


    testRegex "a[\\S]b" []
        ["a-b",
         "*** Failers",
         "a-b",
         "a b"]
        [Just ["a-b"],
         Nothing,
         Just ["a-b"],
         Nothing]
    ,

    testRegex "[\\d]" []
        ["1"]
        [Just ["1"]]
    ,


    testRegex "[\\D]" []
        ["-",
         "*** Failers",
         "-",
         "1"]
        [Just ["-"],
         Just ["*"],
         Just ["-"],
         Nothing]
    ,


    testRegex "ab|cd" []
        ["abc",
         "abcd"]
        [Just ["ab"],
         Just ["ab"]]
    ,



    testRegex "$b" []
        []
        []
    ,


    testRegex "a\\(b" []
        ["a(b"]
        [Just ["a(b"]]
    ,


    testRegex "a\\(*b" []
        ["ab",
         "a((b"]
        [Just ["ab"],
         Just ["a((b"]]
    ,


    testRegex "((a))" []
        ["abc"]
        [Just ["a", "a", "a"]]
    ,


    testRegex "(a)b(c)" []
        ["abc"]
        [Just ["abc", "a", "c"]]

    ,
    testRegex "a+b+c" []
        ["aabbabc"]
        [Just ["abc"]]
    ,


    testRegex "a{1,}b{1,}c" []
        ["aabbabc"]
        [Just ["abc"]]
    ,


    testRegex "a.+?c" []
        ["abcabc"]
        [Just ["abc"]]
    ,


    testRegex "(a+|b)*" []
        ["ab"]
        [Just ["ab", "b"]]
    ,


    testRegex "(a+|b){0,}" []
        ["ab"]
        [Just ["ab", "b"]]
    ,


    testRegex "(a+|b)+" []
        ["ab"]
        [Just ["ab","b"]]
    ,


    testRegex "(a+|b){1,}" []
        ["ab"]
        [Just ["ab", "b"]]
    ,


    testRegex "(a+|b)?" []
        ["ab"]
        [Just ["a", "a"]]
    ,


    testRegex "(a+|b){0,1}" []
        ["ab"]
        [Just ["a", "a"]]
    ,


    testRegex "[^ab]*" []
        ["cde"]
        [Just ["cde"]]

    ,

    testRegex "abc" []
        ["b"]
        [Nothing]
    ,


    testRegex "a*" []
        [""]
        [Just [""]]
    ,


    testRegex "([abc])*d" []
        ["abbbcd"]
        [Just ["abbbcd", "c"]]
    ,


    testRegex "([abc])*bcd" []
        ["abcd"]
        [Just ["abcd", "a"]]
    ,


    testRegex "a|b|c|d|e" []
        ["e"]
        [Just ["e"]]
    ,


    testRegex "(a|b|c|d|e)f" []
        ["ef"]
        [Just ["ef", "e"]]
    ,


    testRegex "abcd*efg" []
        ["abcdefg"]
        [Just ["abcdefg"]]
    ,


    testRegex "ab*" []
        ["xabyabbbz",
         "xayabbbz"]
        [Just ["ab"],
         Just ["a"]]
    ,


    testRegex "(ab|cd)e" []
        ["abcde"]
        [Just ["cde", "cd"]]
    ,


    testRegex "[abhgefdc]ij" []
        ["hij"]
        [Just ["hij"]]

    , testRegex "^(ab|cd)e" []
        []
        []
    ,

    testRegex "(abc|)ef" []
        ["abcdef"]
        [Just ["ef", ""]]
    ,


    testRegex "(a|b)c*d" []
        ["abcd"]
        [Just ["bcd", "b"]]
    ,


    testRegex "(ab|ab*)bc" []
        ["abc"]
        [Just ["abc", "a"]]
    ,


    testRegex "a([bc]*)c*" []
        ["abc"]
        [Just ["abc", "bc"]]
    ,


    testRegex "a([bc]*)(c*d)" []
        ["abcd"]
        [Just ["abcd", "bc", "d"]]
    ,


    testRegex "a([bc]+)(c*d)" []
        ["abcd"]
        [Just ["abcd", "bc", "d"]]
    ,


    testRegex "a([bc]*)(c+d)" []
        ["abcd"]
        [Just ["abcd", "b", "cd"]]
    ,


    testRegex "a[bcd]*dcdcde" []
        ["adcdcde"]
        [Just ["adcdcde"]]
    ,


    testRegex "a[bcd]+dcdcde" []
        ["*** Failers",
         "abcde",
         "adcdcde"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(ab|a)b*c" []
        ["abc"]
        [Just ["abc", "ab"]]

    ,

    testRegex "((a)(b)c)(d)" []
        ["abcd"]
        [Just ["abcd", "abc", "a", "b", "d"]]
    ,


    testRegex "[a-zA-Z_][a-zA-Z0-9_]*" []
        ["alpha"]
        [Just ["alpha"]]
    ,


    testRegex "^a(bc+|b[eh])g|.h$" []
        ["abh"]
        [Just ["bh"]]
    ,


    testRegex "(bc+d$|ef*g.|h?i(j|k))" []
        ["effgz",
         "ij",
         "reffgz",
         "*** Failers",
         "effg",
         "bcdd"]
        [Just ["effgz", "effgz"],
         Just ["ij", "ij", "j"],
         Just ["effgz", "effgz"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((((((((((a))))))))))" []
        ["a"]
        [Just ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"]]
    ,


    testRegex "((((((((((a))))))))))\\10" []
        ["aa"]
        [Just ["aa", "a", "a", "a", "a", "a", "a", "a", "a", "a", "a"]]
    ,


    testRegex "(((((((((a)))))))))" []
        ["a"]
        [Just ["a", "a", "a", "a", "a", "a", "a", "a", "a", "a"]]
    ,


    testRegex "multiple words of text" []
        ["*** Failers",
         "aa",
         "uh-uh"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "multiple words" []
        ["multiple words, yeah"]
        [Just ["multiple words"]]
    ,


    testRegex "(.*)c(.*)" []
        ["abcde"]
        [Just ["abcde", "ab", "de"]]
    ,

    testRegex "\\((.*), (.*)\\)" []
        ["(a, b)"]
        [Just ["(a, b)", "a", "b"]]
    ,


    testRegex "[k]" []
        []
        []
    ,


    testRegex "abcd" []
        ["abcd"]
        [Just ["abcd"]]
    ,


    testRegex "a(bc)d" []
        ["abcd"]
        [Just ["abcd", "bc"]]
    ,


    testRegex "a[-]?c" []
        ["ac"]
        [Just ["ac"]]
    ,


    testRegex "(abc)\\1" []
        ["abcabc"]
        [Just ["abcabc", "abc"]]
    ,


    testRegex "([a-c]*)\\1" []
        ["abcabc"]
        [Just ["abcabc", "abc"]]
    ,


    testRegex "(a)|\\1" []
        ["a",
         "*** Failers",
         "ab",
         "x"]
        [Just ["a", "a"],
         Just ["a", "a"],
         Just ["a", "a"],
         Nothing]
    ,


    testRegex "(([a-c])b*?\\2)*" []
        ["ababbbcbc"]
        [Just ["ababb", "bb", "b"]]
    ,


    testRegex "(([a-c])b*?\\2){3}" []
        ["ababbbcbc"]
        [Just ["ababbbcbc", "cbc", "c"]]
    ,
    testRegex "((\\3|b)\\2(a)x)+" []
        ["aaaxabaxbaaxbbax"]
        [Just ["bbax", "bbax", "b", "a"]]
    ,


    testRegex "((\\3|b)\\2(a)){2,}" []
        ["bbaababbabaaaaabbaaaabba"]
        [Just ["bbaaaabba", "bba", "b", "a"]]
    ,


    testRegex "abc" [caseless]
        ["ABC",
         "XABCY",
         "ABABC",
         "*** Failers",
         "aaxabxbaxbbx",
         "XBC",
         "AXC",
         "ABX"]
        [Just ["ABC"],
         Just ["ABC"],
         Just ["ABC"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "ab*c" [caseless]
        ["ABC"]
        [Just ["ABC"]]
    ,


    testRegex "ab*bc" [caseless]
        ["ABC",
         "ABBC"]
        [Just ["ABC"],
         Just ["ABBC"]]
    ,


    testRegex "ab*?bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab{0,}?bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab+?bc" [caseless]
        ["ABBC"]
        [Just ["ABBC"]]
    ,


    testRegex "ab+bc" [caseless]
        ["*** Failers",
         "ABC",
         "ABQ"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "ab{1,}bc" [caseless]
        []
        []
    ,

    testRegex "ab+bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab{1,}?bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab{1,3}?bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab{3,4}?bc" [caseless]
        ["ABBBBC"]
        [Just ["ABBBBC"]]
    ,


    testRegex "ab{4,5}?bc" [caseless]
        ["*** Failers",
         "ABQ",
         "ABBBBC"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "ab??bc" [caseless]
        ["ABBC",
         "ABC"]
        [Just ["ABBC"],
         Just ["ABC"]]
    ,


    testRegex "ab{0,1}?bc" [caseless]
        ["ABC"]
        [Just ["ABC"]]
    ,


    testRegex "ab??bc" [caseless]
        []
        []
    ,


    testRegex "ab??c" [caseless]
        ["ABC"]
        [Just ["ABC"]]
    ,

    testRegex "ab{0,1}?c" [caseless]
        ["ABC"]
        [Just ["ABC"]]
    ,
    testRegex "^abc$" [caseless]
        ["ABC",
         "*** Failers",
         "ABBBBC",
         "ABCC"]
        [Just ["ABC"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "^abc" [caseless]
        ["ABCC"]
        [Just ["ABC"]]
    ,


    testRegex "^abc$" [caseless]
        []
        []
    ,


    testRegex "abc$" [caseless]
        ["AABC"]
        [Just ["ABC"]]
    ,


    testRegex "^" [caseless]
        ["ABC"]
        [Just [""]]
    ,


    testRegex "$" [caseless]
        ["ABC"]
        [Just [""]]
    ,


    testRegex "a.c" [caseless]
        ["ABC",
         "AXC"]
        [Just ["ABC"],
         Just ["AXC"]]
    ,


    testRegex "a.*?c" [caseless]
        ["AXYZC"]
        [Just ["AXYZC"]]
    ,


    testRegex "a.*c" [caseless]
        ["*** Failers",
         "AABC",
         "AXYZD"]
        [Nothing,
         Just ["AABC"],
         Nothing]
    ,


    testRegex "a[bc]d" [caseless]
        ["ABD"]
        [Just ["ABD"]]
    ,

    testRegex "a[b-d]e" [caseless]
        ["ACE",
         "ABC",
         "ABD"]
        [Just ["ACE"],
         Nothing,
         Nothing]
    ,


    testRegex "a[b-d]" [caseless]
        ["AAC"]
        [Just ["AC"]]
    ,


    testRegex "a[-b]" [caseless]
        ["A-"]
        [Just ["A-"]]
    ,


    testRegex "a[b-]" [caseless]
        ["A-"]
        [Just ["A-"]]
    ,


    testRegex "a]" [caseless]
        ["A]"]
        [Just ["A]"]]
    ,


    testRegex "a[]]b" [caseless]
        ["A]B"]
        [Just ["A]B"]]
    ,


    testRegex "a[^bc]d" [caseless]
        ["AED"]
        [Just ["AED"]]
    ,


    testRegex "a[^-b]c" [caseless]
        ["ADC",
         "ABD",
         "A-C"]
        [Just ["ADC"],
         Nothing,
         Nothing]
    ,


    testRegex "a[^]b]c" [caseless]
        ["ADC"]
        [Just ["ADC"]]
    ,


    testRegex "ab|cd" [caseless]
        ["ABC",
         "ABCD"]
        [Just ["AB"],
         Just ["AB"]]

    ,
    testRegex "()ef" [caseless]
        ["DEF"]
        [Just ["EF", ""]]
    ,


    testRegex "$b" [caseless]
        ["A]C",
         "B"]
        [Nothing,
         Nothing]
    ,


    testRegex "a\\(b" [caseless]
        ["A(B"]
        [Just ["A(B"]]
    ,


    testRegex "a\\(*b" [caseless]
        ["AB",
         "A((B"]
        [Just ["AB"],
         Just ["A((B"]]
    ,



    testRegex "((a))" [caseless]
        ["ABC"]
        [Just ["A", "A", "A"]]
    ,


    testRegex "(a)b(c)" [caseless]
        ["ABC"]
        [Just ["ABC", "A", "C"]]
    ,


    testRegex "a+b+c" [caseless]
        ["AABBABC"]
        [Just ["ABC"]]
    ,


    testRegex "a{1,}b{1,}c" [caseless]
        ["AABBABC"]
        [Just ["ABC"]]
    ,


    testRegex "a.+?c" [caseless]
        ["ABCABC"]
        [Just ["ABC"]]
    ,

    testRegex "a.*?c" [caseless]
        ["ABCABC"]
        [Just ["ABC"]]
    ,


    testRegex "a.{0,5}?c" [caseless]
        ["ABCABC"]
        [Just ["ABC"]]
    ,


    testRegex "(a+|b)*" [caseless]
        ["AB"]
        [Just ["AB", "B"]]
    ,


    testRegex "(a+|b){0,}" [caseless]
        ["AB"]
        [Just ["AB", "B"]]
    ,


    testRegex "(a+|b)+" [caseless]
        ["AB"]
        [Just ["AB","B"]]
    ,


    testRegex "(a+|b){1,}" [caseless]
        ["AB"]
        [Just ["AB", "B"]]
    ,


    testRegex "(a+|b)?" [caseless]
        ["AB"]
        [Just ["A", "A"]]
    ,


    testRegex "(a+|b){0,1}" [caseless]
        ["AB"]
        [Just ["A", "A"]]
    ,


    testRegex "(a+|b){0,1}?" [caseless]
        ["AB"]
        [Just [""]]
    ,


    testRegex "[^ab]*" [caseless]
        ["CDE"]
        [Just ["CDE"]]
    ,

    testRegex "abc" [caseless]
        []
        []
    ,


    testRegex "a*" [caseless]
        [""]
        [Just [""]]
    ,


    testRegex "([abc])*d" [caseless]
        ["ABBBCD"]
        [Just ["ABBBCD", "C"]]
    ,


    testRegex "([abc])*bcd" [caseless]
        ["ABCD"]
        [Just ["ABCD","A"]]
    ,


    testRegex "a|b|c|d|e" [caseless]
        ["E"]
        [Just ["E"]]
    ,


    testRegex "(a|b|c|d|e)f" [caseless]
        ["EF"]
        [Just ["EF", "E"]]
    ,


    testRegex "abcd*efg" [caseless]
        ["ABCDEFG"]
        [Just ["ABCDEFG"]]
    ,


    testRegex "ab*" [caseless]
        ["XABYABBBZ",
         "XAYABBBZ"]
        [Just ["AB"],
         Just ["A"]]
    ,


    testRegex "(ab|cd)e" [caseless]
        ["ABCDE"]
        [Just ["CDE", "CD"]]
    ,


    testRegex "[abhgefdc]ij" [caseless]
        ["HIJ"]
        [Just ["HIJ"]]
    ,


    testRegex "^(ab|cd)e" [caseless]
        ["ABCDE"]
        [Nothing]
    ,


    testRegex "(abc|)ef" [caseless]
        ["ABCDEF"]
        [Just ["EF", ""]]
    ,


    testRegex "(a|b)c*d" [caseless]
        ["ABCD"]
        [Just ["BCD", "B"]]
    ,


    testRegex "(ab|ab*)bc" [caseless]
        ["ABC"]
        [Just ["ABC", "A"]]
    ,


    testRegex "a([bc]*)c*" [caseless]
        ["ABC"]
        [Just ["ABC", "BC"]]
    ,


    testRegex "a([bc]*)(c*d)" [caseless]
        ["ABCD"]
        [Just ["ABCD","BC","D"]]
    ,


    testRegex "a([bc]+)(c*d)" [caseless]
        ["ABCD"]
        [Just ["ABCD","BC","D"]]
    ,


    testRegex "a([bc]*)(c+d)" [caseless]
        ["ABCD"]
        [Just ["ABCD","B","CD"]]
    ,


    testRegex "a[bcd]*dcdcde" [caseless]
        ["ADCDCDE"]
        [Just ["ADCDCDE"]]
    ,

    testRegex "a[bcd]+dcdcde" [caseless]
        []
        []
    ,

    testRegex "(ab|a)b*c" [caseless]
        ["ABC"]
        [Just ["ABC", "AB"]]
    ,


    testRegex "((a)(b)c)(d)" [caseless]
        ["ABCD"]
        [Just ["ABCD", "ABC", "A", "B", "D"]]
    ,


    testRegex "[a-zA-Z_][a-zA-Z0-9_]*" [caseless]
        ["ALPHA"]
        [Just ["ALPHA"]]
    ,


    testRegex "^a(bc+|b[eh])g|.h$" [caseless]
        ["ABH"]
        [Just ["BH"]]
    ,


    testRegex "(bc+d$|ef*g.|h?i(j|k))" [caseless]
        ["EFFGZ",
         "IJ",
         "REFFGZ",
         "*** Failers",
         "ADCDCDE",
         "EFFG",
         "BCDD"]
        [Just ["EFFGZ", "EFFGZ"],
         Just ["IJ", "IJ", "J"],
         Just ["EFFGZ", "EFFGZ"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((((((((((a))))))))))" [caseless]
        ["A"]
        [Just ["A", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"]]
    ,


    testRegex "((((((((((a))))))))))\\10" [caseless]
        ["AA"]
        [Just ["AA", "A", "A", "A", "A", "A", "A", "A", "A", "A", "A"]]
    ,


    testRegex "(((((((((a)))))))))" [caseless]
        ["A"]
        [Just ["A", "A", "A", "A", "A", "A", "A", "A", "A", "A"]]
    ,


    testRegex "(?:(?:(?:(?:(?:(?:(?:(?:(?:(a))))))))))" [caseless]
        ["A"]
        [Just ["A", "A"]]
    ,


    testRegex "(?:(?:(?:(?:(?:(?:(?:(?:(?:(a|b|c))))))))))" [caseless]
        ["C"]
        [Just ["C", "C"]]
    ,


    testRegex "multiple words of text" [caseless]
        ["AA",
         "UH-UH"]
        [Nothing,
         Nothing]
    ,


    testRegex "multiple words" [caseless]
        ["MULTIPLE WORDS, YEAH"]
        [Just ["MULTIPLE WORDS"]]
    ,


    testRegex "(.*)c(.*)" [caseless]
        ["ABCDE"]
        [Just ["ABCDE", "AB", "DE"]]
    ,


    testRegex "\\((.*), (.*)\\)" [caseless]
        ["(A, B)"]
        [Just ["(A, B)", "A", "B"]]
    ,


    testRegex "[k]" [caseless]
        []
        []
    ,


    testRegex "abcd" [caseless]
        ["ABCD"]
        [Just ["ABCD"]]
    ,


    testRegex "a(bc)d" [caseless]
        ["ABCD"]
        [Just ["ABCD", "BC"]]
    ,


    testRegex "a[-]?c" [caseless]
        ["AC"]
        [Just ["AC"]]
    ,


    testRegex "(abc)\\1" [caseless]
        ["ABCABC"]
        [Just ["ABCABC", "ABC"]]
    ,


    testRegex "([a-c]*)\\1" [caseless]
        ["ABCABC"]
        [Just ["ABCABC", "ABC"]]
    ,

    testRegex "a(?!b)." []
        ["abad"]
        [Just ["ad"]]
    ,


    testRegex "a(?=d)." []
        ["abad"]
        [Just ["ad"]]
    ,


    testRegex "a(?=c|d)." []
        ["abad"]
        [Just ["ad"]]
    ,


    testRegex "a(?:b|c|d)(.)" []
        ["ace"]
        [Just ["ace", "e"]]
    ,


    testRegex "a(?:b|c|d)*(.)" []
        ["ace"]
        [Just ["ace", "e"]]
    ,


    testRegex "a(?:b|c|d)+?(.)" []
        ["ace",
         "acdbcdbe"]
        [Just ["ace", "e"],
         Just ["acd", "d"]]
    ,


    testRegex "a(?:b|c|d)+(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdbe", "e"]]
    ,


    testRegex "a(?:b|c|d){2}(.)" []
        ["acdbcdbe"]
        [Just ["acdb", "b"]]
    ,


    testRegex "a(?:b|c|d){4,5}(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdb", "b"]]
    ,


    testRegex "a(?:b|c|d){4,5}?(.)" []
        ["acdbcdbe"]
        [Just ["acdbcd", "d"]]
    ,


    testRegex "((foo)|(bar))*" []
        ["foobar"]
        [Just ["foobar", "bar", "foo", "bar"]]
    ,


    testRegex "a(?:b|c|d){6,7}(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdbe", "e"]]
    ,


    testRegex "a(?:b|c|d){6,7}?(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdbe", "e"]]
    ,


    testRegex "a(?:b|c|d){5,6}(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdbe", "e"]]
    ,


    testRegex "a(?:b|c|d){5,6}?(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdb", "b"]]
    ,


    testRegex "a(?:b|c|d){5,7}(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdbe", "e"]]
    ,


    testRegex "a(?:b|c|d){5,7}?(.)" []
        ["acdbcdbe"]
        [Just ["acdbcdb", "b"]]
    ,


    testRegex "a(?:b|(c|e){1,2}?|d)+?(.)" []
        ["ace"]
        [Just ["ace", "c", "e"]]
    ,


    testRegex "^(.+)?B" []
        ["AB"]
        [Just ["AB", "A"]]
    ,


    testRegex "^([^a-z])|(\\^)$" []
        ["."]
        [Just [".", "."]]
    ,

    testRegex "^[<>]&" []
        ["<&OUT"]
        [Just ["<&"]]
    ,


    testRegex "^(a\\1?){4}$" []
        ["aaaaaaaaaa",
         "*** Failers",
         "AB",
         "aaaaaaaaa",
         "aaaaaaaaaaa"]
        [Just ["aaaaaaaaaa", "aaaa"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "^(a(?(1)\\1)){4}$" []
        ["aaaaaaaaaa",
         "*** Failers",
         "aaaaaaaaa",
         "aaaaaaaaaaa"]
        [Just ["aaaaaaaaaa", "aaaa"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(?:(f)(o)(o)|(b)(a)(r))*" []
        ["foobar"]
        [Just ["foobar", "f", "o", "o", "b", "a", "r"]]
    ,


    testRegex "(?<=a)b" []
        ["ab",
         "*** Failers",
         "cb",
         "b"]
        [Just ["b"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(?<!c)b" []
        ["ab",
         "b",
         "b"]
        [Just ["b"],
         Just ["b"],
         Just ["b"]]
    ,


    testRegex "(?:..)*a" []
        ["aba"]
        [Just ["aba"]]
    ,


    testRegex "(?:..)*?a" []
        ["aba"]
        [Just ["a"]]
    ,


    testRegex "^(?:b|a(?=(.)))*\\1" []
        ["abc"]
        [Just ["ab", "b"]]
    ,


    testRegex "^(){3,5}" []
        ["abc"]
        [Just ["", ""]]
    ,


    testRegex "^(a+)*ax" []
        ["aax"]
        [Just ["aax", "a"]]
    ,


    testRegex "^((a|b)+)*ax" []
        ["aax"]
        [Just ["aax", "a", "a"]]
    ,


    testRegex "^((a|bc)+)*ax" []
        ["aax"]
        [Just ["aax", "a", "a"]]
    ,


    testRegex "(a|x)*ab" []
        ["cab"]
        [Just ["ab"]]
    ,


    testRegex "(a)*ab" []
        ["cab"]
        [Just ["ab"]]
    ,


    testRegex "(?:(?i)a)b" []
        ["ab"]
        [Just ["ab"]]
    ,


    testRegex "((?i)a)b" []
        ["ab"]
        [Just ["ab", "a"]]
    ,


    testRegex "(?:(?i)a)b" []
        ["Ab"]
        [Just ["Ab"]]
    ,


    testRegex "((?i)a)b" []
        ["Ab"]
        [Just ["Ab", "A"]]
    ,


    testRegex "(?:(?i)a)b" []
        ["*** Failers",
         "cb",
         "aB"]
        [Nothing,
         Nothing,
         Nothing]
    ,
    testRegex "((?i)a)b" []
        []
        []
    ,


    testRegex "(?i:a)b" []
        ["ab"]
        [Just ["ab"]]
    ,


    testRegex "((?i:a))b" []
        ["ab"]
        [Just ["ab", "a"]]
    ,


    testRegex "(?i:a)b" []
        ["Ab"]
        [Just ["Ab"]]
    ,


    testRegex "((?i:a))b" []
        ["Ab"]
        [Just ["Ab", "A"]]
    ,


    testRegex "(?i:a)b" []
        ["*** Failers",
         "aB",
         "aB"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?i:a))b" []
        []
        []
    ,


    testRegex "(?:(?-i)a)b" [caseless]
        ["ab"]
        [Just ["ab"]]
    ,


    testRegex "((?-i)a)b" [caseless]
        ["ab"]
        [Just ["ab", "a"]]
    ,


    testRegex "(?:(?-i)a)b" [caseless]
        ["aB"]
        [Just ["aB"]]
    ,


    testRegex "((?-i)a)b" [caseless]
        ["aB"]
        [Just ["aB", "a"]]
    ,


    testRegex "(?:(?-i)a)b" [caseless]
        ["*** Failers",
         "aB",
         "Ab"]
        [Nothing,
         Just ["aB"],
         Nothing]
    ,


    testRegex "((?-i)a)b" [caseless]
        []
        []
    ,


    testRegex "(?:(?-i)a)b" [caseless]
        ["aB"]
        [Just ["aB"]]
    ,


    testRegex "((?-i)a)b" [caseless]
        ["aB"]
        [Just ["aB", "a"]]
    ,


    testRegex "(?:(?-i)a)b" [caseless]
        ["*** Failers",
         "Ab",
         "AB"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?-i)a)b" [caseless]
        []
        []
    ,


    testRegex "(?-i:a)b" [caseless]
        ["ab"]
        [Just ["ab"]]
    ,


    testRegex "((?-i:a))b" [caseless]
        ["ab"]
        [Just ["ab", "a"]]
    ,


    testRegex "(?-i:a)b" [caseless]
        ["aB"]
        [Just ["aB"]]
    ,


    testRegex "((?-i:a))b" [caseless]
        ["aB"]
        [Just ["aB", "a"]]
    ,


    testRegex "(?-i:a)b" [caseless]
        ["*** Failers",
         "AB",
         "Ab"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?-i:a))b" [caseless]
        []
        []
    ,


    testRegex "(?-i:a)b" [caseless]
        ["aB"]
        [Just ["aB"]]
    ,


    testRegex "((?-i:a))b" [caseless]
        ["aB"]
        [Just ["aB", "a"]]
    ,


    testRegex "(?-i:a)b" [caseless]
        ["*** Failers",
         "Ab",
         "AB"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?-i:a))b" [caseless]
        []
        []
    ,


    testRegex "((?-i:a.))b" [caseless]
        ["*** Failers",
         "AB",
         "a\nB"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?s-i:a.))b" [caseless]
        ["a\nB"]
        [Just ["a\nB", "a\n"]]
    ,


    testRegex "(?:c|d)(?:)(?:a(?:)(?:b)(?:b(?:))(?:b(?:)(?:b)))" []
        ["cabbbb"]
        [Just ["cabbbb"]]
    ,

    testRegex "(?:c|d)(?:)(?:aaaaaaaa(?:)(?:bbbbbbbb)(?:bbbbbbbb(?:))(?:bbbbbbbb(?:)(?:bbbbbbbb)))" []
        ["caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]
        [Just ["caaaaaaaabbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"]]
    ,


    testRegex "(ab)\\d\\1" [caseless]
        ["Ab4ab",
         "ab4Ab"]
        [Just ["Ab4ab", "Ab"],
         Just ["ab4Ab", "ab"]]
    ,


    testRegex "foo\\w*\\d{4}baz" []
        ["foobar1234baz"]
        [Just ["foobar1234baz"]]
    ,


    testRegex "x(~~)*(?:(?:F)?)?" []
        ["x~~"]
        [Just ["x~~", "~~"]]
    ,


    testRegex "^a(?#xxx){3}c" []
        ["aaac"]
        [Just ["aaac"]]
    ,


    testRegex "^a (?#xxx) (?#yyy) {3}c" [extended]
        ["aaac"]
        [Just ["aaac"]]
    ,


    testRegex "(?<![cd])b" []
        ["*** Failers",
         "B\nB",
         "dbcb"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(?<![cd])[ab]" []
        ["dbaacb"]
        [Just ["a"]]
    ,


    testRegex "(?<!(c|d))b" []
        []
        []
    ,


    testRegex "(?<!(c|d))[ab]" []
        ["dbaacb"]
        [Just ["a"]]
    ,


    testRegex "(?<!cd)[ab]" []
        ["cdaccb"]
        [Just ["b"]]
    ,

{-
    testRegex "^(?:a?b?)*$" []
        ["a",
         "ab",
         "aaa   ",
         "*** Failers",
         "dbcb",
         "a--",
         "aa-- "]
        [Just ["a"],
         Just ["ab"],
         Just ["aaa"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,
    -}


    testRegex "((?s)^a(.))((?m)^b$)" []
        ["a\nb\nc\n"]
        [Just ["a\nb", "a\n", "\n", "b"]]
    ,


    testRegex "((?m)^b$)" []
        ["a\nb\nc\n"]
        [Just ["b", "b"]]
    ,


    testRegex "(?m)^b" []
        ["a\nb\n"]
        [Just ["b"]]
    ,


    testRegex "(?m)^(b)" []
        ["a\nb\n"]
        [Just ["b", "b"]]
    ,


    testRegex "((?m)^b)" []
        ["a\nb\n"]
        [Just ["b", "b"]]
    ,


    testRegex "\\n((?m)^b)" []
        ["a\nb\n"]
        [Just ["\nb", "b"]]
    ,


    testRegex "((?s).)c(?!.)" []
        ["a\nb\nc\n",
         "a\nb\nc\n"]
        [Just ["\nc", "\n"],
         Just ["\nc", "\n"]]
    ,


    testRegex "((?s)b.)c(?!.)" []
        ["a\nb\nc\n",
         "a\nb\nc\n"]
        [Just ["b\nc", "b\n"],
         Just ["b\nc", "b\n"]]
    ,


    testRegex "^b" []
        []
        []
    ,


    testRegex "()^b" []
        ["*** Failers",
         "a\nb\nc\n",
         "a\nb\nc\n"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "((?m)^b)" []
        ["a\nb\nc\n"]
        [Just ["b", "b"]]
    ,

{-
    testRegex "(?(1)a|b)" []
        []
        []
    ,


    testRegex "(?(1)b|a)" []
        ["a"]
        [Just ["a"]]
    ,

-}

    testRegex "(x)?(?(1)a|b)" []
        ["*** Failers",
         "a",
         "a"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(x)?(?(1)b|a)" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "()?(?(1)b|a)" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "()(?(1)b|a)" []
        []
        []
    ,


    testRegex "()?(?(1)a|b)" []
        ["a"]
        [Just ["a", ""]]
    ,


    testRegex "^(\\()?blah(?(1)(\\)))$" []
        ["(blah)",
         "blah",
         "a",
         "blah)",
         "(blah"]
        [Just ["(blah)", "(", ")"],
         Just ["blah"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "^(\\(+)?blah(?(1)(\\)))$" []
        ["(blah)",
         "blah",
         "*** Failers",
         "blah)",
         "(blah"]
        [Just ["(blah)", "(", ")"],
         Just ["blah"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(?(?!a)a|b)" []
        []
        []
    ,


    testRegex "(?(?!a)b|a)" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "(?(?=a)b|a)" []
        ["*** Failers",
         "a",
         "a"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(?(?=a)a|b)" []
        ["a"]
        [Just ["a"]]
    ,


    testRegex "(?=(a+?))(\\1ab)" []
        ["aaab"]
        [Just ["aab", "a", "aab"]]
    ,


    testRegex "^(?=(a+?))\\1ab" []
        []
        []
    ,


    testRegex "(\\w+:)+" []
        ["one:"]
        [Just ["one:", "one:"]]
    ,


    testRegex "$(?<=^(a))" []
        ["a"]
        [Just ["", "a"]]
    ,


    testRegex "(?=(a+?))(\\1ab)" []
        ["aaab"]
        [Just ["aab", "a", "aab"]]
    ,


    testRegex "^(?=(a+?))\\1ab" []
        ["*** Failers",
         "aaab",
         "aaab"]
        [Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "([\\w:]+::)?(\\w+)$" []
        ["abcd",
         "xy:z:::abcd"]
        [Just ["abcd", "", "abcd"],
         Just ["xy:z:::abcd", "xy:z:::", "abcd"]]
    ,


    testRegex "^[^bcd]*(c+)" []
        ["aexycd"]
        [Just ["aexyc", "c"]]
    ,


    testRegex "(a*)b+" []
        ["caab"]
        [Just ["aab", "aa"]]
    ,


    testRegex "([\\w:]+::)?(\\w+)$" []
        ["abcd",
         "xy:z:::abcd",
         "*** Failers",
         "abcd:",
         "abcd:"]
        [Just ["abcd", "", "abcd"],
         Just ["xy:z:::abcd", "xy:z:::", "abcd"],
         Just ["Failers", "", "Failers"],
         Nothing,
         Nothing]
    ,


    testRegex "^[^bcd]*(c+)" []
        ["aexycd"]
        [Just ["aexyc", "c"]]
    ,


    testRegex "(>a+)ab" []
        []
        []
    ,


    testRegex "(?>a+)b" []
        ["aaab"]
        [Just ["aaab"]]
    ,


    testRegex "([[:]+)" []
        ["a:[b]:"]
        [Just [":[", ":["]]

    ,
    testRegex "([[=]+)" []
        ["a=[b]="]
        [Just ["=[", "=["]]
    ,


    testRegex "([[.]+)" []
        ["a.[b]."]
        [Just [".[", ".["]]
    ,


    testRegex "((?>a+)b)" []
        ["aaab"]
        [Just ["aaab", "aaab"]]
    ,


    testRegex "(?>(a+))b" []
        ["aaab"]
        [Just ["aaab", "aaa"]]
    ,


    testRegex "((?>[^()]+)|\\([^()]*\\))+" []
        ["((abc(ade)ufh()()x"]
        [Just ["abc(ade)ufh()()x", "x"]]
    ,


    testRegex "a\\Z" []
        ["aaab",
         "a\nb\n"]
        [Nothing,
         Nothing]
    ,


    testRegex "b\\Z" []
        ["a\nb\n"]
        [Just ["b"]]
    ,


    testRegex "b\\z" []
        []
        []
    ,


    testRegex "b\\Z" []
        ["a\\nb"]
        [Just ["b"]]
    ,

    testRegex "(?>.*)(?<=(abcd|wxyz))" []
        ["alphabetabcd",
         "endingwxyz",
         "*** Failers",
         "a rather long string that doesn't end with one of them"]
        [Just ["alphabetabcd", "abcd"],
         Just ["endingwxyz", "wxyz"],
         Nothing,
         Nothing]
    ,


    testRegex "word (?>(?:(?!otherword)[a-zA-Z0-9]+ ){0,30})otherword" []
        ["word cat dog elephant mussel cow horse canary baboon snake shark otherword",
         "word cat dog elephant mussel cow horse canary baboon snake shark"
         ]
        [Just ["word cat dog elephant mussel cow horse canary baboon snake shark otherword"],
         Nothing]
    ,

    testRegex "((Z)+|A)*" []
        ["ZABCDEFG"]
        [Just ["ZA", "A", "Z"]]
    ,


    testRegex "(Z()|A)*" []
        ["ZABCDEFG"]
        [Just ["ZA", "A", ""]]
    ,


    testRegex "(Z(())|A)*" []
        ["ZABCDEFG"]
        [Just ["ZA", "A", "", ""]]
    ,

    testRegex "((?>Z)+|A)*" []
        ["ZABCDEFG"]
        [Just ["ZA", "A"]]
    ,


    testRegex "((?>)+|A)*" []
        ["ZABCDEFG"]
        [Just ["", ""]]
    ,

    -- testRegex "^[a-\\d]" []
    --     ["abcde",
    --      "-things",
    --      "0digit",
    --      "*** Failers",
    --      "bcdef    "]
    --     [Just ["a"],
    --      Just ["-"],
    --      Just ["0"],
    --      Nothing,
    --      Nothing]
    -- ,

    testRegex "\\Qabc\\$xyz\\E" []
        ["abc\\$xyz"]
        [Just ["abc\\$xyz"]]
    ,
    testRegex "\\Qabc\\E\\$\\Qxyz\\E" []
        ["abc$xyz"]
        [Just ["abc$xyz"]]
    ,
    testRegex "\\Gabc" []
        ["abc",
         "*** Failers",
         "xyzabc  "]
        [Just ["abc"],
         Nothing,
         Nothing]
    ,

    testRegex "a(?x: b c )d" []
        ["XabcdY",
         "*** Failers ",
         "Xa b c d Y "]
        [Just ["abcd"],
         Nothing,
         Nothing]
    ,


    testRegex "((?x)x y z | a b c)" []
        ["XabcY",
         "AxyzB "]
        [Just ["abc", "abc"],
         Just ["xyz", "xyz"]]
    ,


    testRegex "(?i)AB(?-i)C" []
        ["XabCY",
         "*** Failers",
         "XabcY  "]
        [Just ["abC"],
         Nothing,
         Nothing]
    ,


    testRegex "((?i)AB(?-i)C|D)E" []
        ["abCE",
         "DE",
         "*** Failers",
         "abcE",
         "abCe  ",
         "dE",
         "De    "]
        [Just ["abCE", "abC"],
         Just ["DE", "D"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(.*)\\d+\\1" []
        ["abc123abc",
         "abc123bc "]
        [Just ["abc123abc", "abc"],
         Just ["bc123bc", "bc"]]
    ,

    testRegex "[z\\Qa-d]\\E]" []
        ["z",
         "a",
         "-",
         "d",
         "] ",
         "*** Failers",
         "b     "]
        [Just ["z"],
         Just ["a"],
         Just ["-"],
         Just ["d"],
         Just ["]"],
         Just ["a"],
         Nothing]

    , testRegex "(?<=Z)X." []
        ["\\x84XAZXB"]
        [Just ["XB"]]
    ,

    testRegex "ab cd (?x) de fg" []
        ["ab cd defg"]
        [Just ["ab cd defg"]]
    ,


    testRegex "ab cd(?x) de fg" []
        ["ab cddefg",
         "** Failers ",
         "abcddefg"]
        [Just ["ab cddefg"],
         Nothing,
         Nothing]
    ,


    testRegex "(?<![^f]oo)(bar)" []
        ["foobarX ",
         "** Failers ",
         "boobarX"]
        [Just ["bar", "bar"],
         Nothing,
         Nothing]
    ,


    testRegex "(?<![^f])X" []
        ["offX",
         "** Failers",
         "onyX  "]
        [Just ["X"],
         Nothing,
         Nothing]
    ,


    testRegex "(?<=[^f])X" []
        ["onyX",
         "** Failers",
         "offX "]
        [Just ["X"],
         Nothing,
         Nothing]
    ,



    testRegex "(?:(?(1)a|b)(X))+" []
        ["bXaX"]
        [Just ["bXaX", "X"]]
    ,

    testRegex "(?:(?(1)\\1a|b)(X|Y))+" []
        ["bXXaYYaY",
         "bXYaXXaX  "]
        [Just ["bXXaYYaY", "Y"],
         Just ["bX", "X"]]
    ,


    testRegex "()()()()()()()()()(?:(?(10)\\10a|b)(X|Y))+" []
        ["bXXaYYaY"]
        [Just ["bX", "", "", "", "", "", "", "", "", "", "X"]]
    ,


    testRegex "[[,abc,]+]" []
        ["abc]",
         "a,b]",
         "[a,b,c]  "]
        [Just ["abc]"],
         Just ["a,b]"],
         Just ["[a,b,c]"]]
    ,




    testRegex "a*b*\\w" []
        ["aaabbbb",
         "aaaa",
         "a"]
        [Just ["aaabbbb"],
         Just ["aaaa"],
         Just ["a"]]
    ,


    testRegex "a*b?\\w" []
        ["aaabbbb",
         "aaaa",
         "a"]
        [Just ["aaabb"],
         Just ["aaaa"],
         Just ["a"]]
    ,


    testRegex "a*b{0,4}\\w" []
        ["aaabbbb",
         "aaaa",
         "a"]
        [Just ["aaabbbb"],
         Just ["aaaa"],
         Just ["a"]]
    ,


    testRegex "(?=(\\w+))\\1:" []
        ["abcd:"]
        [Just ["abcd:", "abcd"]]
    ,


    testRegex "^(?=(\\w+))\\1:" []
        ["abcd:"]
        [Just ["abcd:", "abcd"]]
    ,



    testRegex "^[a\\E\\E-\\Ec]" []
        ["b",
         "** Failers",
         "-",
         "E    "]
        [Just ["b"],
         Nothing,
         Nothing,
         Nothing]
    ,


    testRegex "(a){0,3}(?(1)b|(c|))*D" []
        ["abbD",
         "ccccD",
         "D  "]
        [Just ["abbD", "a"],
         Just ["ccccD", "", ""],
         Just ["D", "", ""]]
    ,


    -- WARNING: at around this point, the file starts segfaulting ghci....
    testRegex "(a|)*\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]
        [Nothing,
         Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", ""]]

    ,
    testRegex "(?>a|)*\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]
        [Nothing,
         Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]]

    ,

    testRegex "(?=(\\w+))\\1:" []
        ["abcd:"]
        [Just ["abcd:", "abcd"]]
    ,


    testRegex "^(?=(\\w+))\\1:" []
        ["abcd:"]
        [Just ["abcd:", "abcd"]]
    ,



    testRegex "^[a\\E\\E-\\Ec]" []
        ["b",
         "** Failers",
         "-",
         "E    "]
        [Just ["b"],
         Nothing,
         Nothing,
         Nothing]
    ,







    testRegex "(a){0,3}(?(1)b|(c|))*D" []
        ["abbD",
         "ccccD",
         "D  "]
        [Just ["abbD", "a"],
         Just ["ccccD", "", ""],
         Just ["D", "", ""]]
    ,


    testRegex "(a|)*\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]
        [Nothing,
         Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4", ""]]
    ,


    testRegex "(?>a|)*\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]
        [Nothing,
         Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa4"]]
    ,

    testRegex "( (A | (?(1)0|) )*   )" [extended]
        ["abcd"]
        [Just ["", "", ""]]

    ,
    testRegex "^[\\w]+" []
        ["*** Failers",
         "\201cole"]
        [Nothing,
         Nothing]



    ,
    testRegex "^[\\w]+" []
        ["*** Failers",
         "\201cole"]
        [Nothing,
         Nothing]

{-
    ,

    testRegex "(a.b(?s)c.d|x.y)p.q" []
        ["a+bc+dp+q",
         "a+bc\ndp+q",
         "x\nyp+q ",
         "a\nbc\ndp+q",
         "a+bc\ndp\nq",
         "x\nyp\nq "
         ]
        [Just ["a+bc+dp+q"],
         Just ["a+bc\ndp+q"],
         Just ["x\nyp+q"],
         Nothing,
         Nothing,
         Nothing
         ]
         -}

    ,
    testRegex "a\\d\\z" []
        ["ba0",
         "*** Failers",
         "ba0\n",
         "ba0\ncd   "]
        [Just ["a0"],
         Nothing,
         Nothing,
         Nothing]

    ,
    testRegex "a\\d\\Z" []
        ["ba0",
         "ba0\n",
         "ba0\ncd   "]
        [Just ["a0"],
         Just ["a0"],
         Nothing]

    ,
    testRegex "a\\d$" []
        ["ba0",
         "ba0\n",
         "*** Failers",
         "ba0\ncd   "]
        [Just ["a0"],
         Just ["a0"],
         Nothing,
         Nothing]
    ,
    testRegex "a+" []
        ["aaaa"]
        [Just ["aaaa"]]



    ,
    testRegex "^\\d{2,3}X" []
        ["12X",
         "123X",
         "*** Failers",
         "X",
         "1X",
         "1234X     "]
        [Just ["12X"],
         Just ["123X"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]


    ,
    testRegex "^[abcd]\\d" []
        ["a45",
         "b93",
         "c99z",
         "d04",
         "*** Failers",
         "e45",
         "abcd      ",
         "abcd1234",
         "1234  "]
        [Just ["a4"],
         Just ["b9"],
         Just ["c9"],
         Just ["d0"],
         Nothing,
         Nothing,
         Nothing,
         Nothing,
         Nothing]


{-
    ,
    testRegex "^(a*\\w|ab)=(a*\\w|ab)" []
        ["ab=ab"]
        [Just ["ab=ab", "ab"]]
        -}


    ,
    testRegex "^(a*\\w|ab)=(?1)" []
        ["ab=ab"]
        [Just ["ab=ab", "ab"]]


{-
    ,
    testRegex "^([^()]|\\((?1)*\\))*$" []
        ["abc",
         "a(b)c",
         "a(b(c))d  ",
         "*** Failers)",
         "a(b(c)d  "]
        [Just ["abc"],
         Just ["a(b)c"],
         Just ["a(b(c))d"],
         Nothing,
         Nothing]

    ,
    testRegex "^>abc>([^()]|\\((?1)*\\))*<xyz<$" []
        [">abc>123<xyz<",
         ">abc>1(2)3<xyz<",
         ">abc>(1(2)3)<xyz<"]
        [Just [">abc>123<xyz<"],
         Just [">abc>1(2)3<xyz<"],
         Just [">abc>(1(2)3)<xyz<"]]

-}

    ,
    testRegex "^(?>a*)\\d" []
        ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9876",
         "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"]
        [Just ["aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa9"],
         Nothing]





    ,
    testRegex "(?<=abc|xy)123" []
        ["abc12345",
         "wxy123z",
         "*** Failers",
         "123abc"]
        [Just ["123"],
         Just ["123"],
         Nothing,
         Nothing]


    ,
    testRegex "(?<!abc|xy)123" []
        ["123abc",
         "mno123456 ",
         "*** Failers",
         "abc12345",
         "wxy123z"]
        [Just ["123"],
         Just ["123"],
         Nothing,
         Nothing,
         Nothing]

    ,
    testRegex "abc(?C1)xyz" []
        ["abcxyz",
         "123abcxyz999 "]
        [ Just ["abcxyz"],
         Just ["abcxyz"]]





{-
    ,
    testRegex "\\Gabc" []
        ["abcdef",
         "defabcxyz\\>3 ",
         "defabcxyz"]
        [Just ["abc"],
         Just ["abc"],
         Nothing]
         -}

    ,

    testRegex "[\\xFF]" []
        [">\xff<"]
        [Just ["\xff"]]

    ,
    testRegex "[^\\xFF]" []
        ["XYZ"]
        [Just ["X"]]


    ,
    testRegex "^\\pN{2,3}X" []
        ["12X",
         "123X",
         "*** Failers",
         "X",
         "1X",
         "1234X     "]
        [Just ["12X"],
         Just ["123X"],
         Nothing,
         Nothing,
         Nothing,
         Nothing]

    -- named capture group tests
    , -- Handles cases with no capture groups
    testCaptures 0 "no capture groups" []

    , -- Properly labels capture groups
    testCaptures 1 "(?<first>first capture group)" [("first", 0)]

    , -- Doesn't return labels for unnamed groups
    testCaptures 1 "(doesn't return unnamed groups)" []

    , -- Counts but doesn't return unnamed groups
    testCaptures 3 "(?<one>abc) (def) (?<three>ghi)" [("one", 0), ("three", 2)]

    , -- Doesn't count non-capturing groups
    testCaptures 1 "(?:abc) (?<named>def)" [("named", 0)]

    , -- Doesn't count named back-references
    testCaptures 2 "(?<first>abc) (?P=first) (?<second>def)" [("first", 0), ("second", 1)]

    , -- Test alternate group naming syntaxes
    testCaptures 3 "(?'first'abc) (?P<second>def) (?<third>ghi)" [("first", 0), ("second", 1), ("third", 2)]

    , -- Groups are returned in numeric order
    testCaptures 3 "(?'c'0) (?P<b>1) (?<a>2)" [("c", 0), ("b", 1), ("a", 2)]

    , -- Handles alternation between groups
    testCaptures 2 "(?<optionA>abc)|(?<optionB>def)" [("optionA", 0), ("optionB", 1)]

    , -- Labels optional groups
    testCaptures 2 "(?<named>abc)(?<optional>def)?" [("named", 0), ("optional", 1)]

    , -- Handles nested named groups
    testCaptures 3 "(?<named>a(?<nested>b)c) (?<unnested>d)" [("named", 0), ("nested", 1), ("unnested", 2)]

    , testLabel "compile failure on duplicate named groups" $
            TestCase $ (assertEqual' "compile failure on duplicate named groups"
                         (Left ("two named subpatterns have the same name"))
                         (compileM "(?<dup>abc) (?<dup>def)" []))

 ]
