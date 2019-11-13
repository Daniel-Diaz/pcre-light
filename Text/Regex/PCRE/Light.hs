{-# LANGUAGE CPP #-}
--------------------------------------------------------------------
-- |
-- Module   : Text.Regex.PCRE.Light
-- Copyright: Copyright (c) 2007-2008, Don Stewart
-- License  : BSD3
--
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  experimental
-- Portability: H98 + CPP
--
--------------------------------------------------------------------
--
-- A simple, portable binding to perl-compatible regular expressions
-- (PCRE) via strict ByteStrings.
--

module Text.Regex.PCRE.Light (

        -- * The abstract PCRE Regex type
          Regex

        -- * ByteString interface
        , compile, compileM
        , match
        , captureCount
        , captureNames

        -- * Regex types and constructors externally visible

        -- ** PCRE compile-time bit flags
        , PCREOption

        , anchored
        , auto_callout
        {-, bsr_anycrlf-}
        {-, bsr_unicode-}
        , caseless
        , dollar_endonly
        , dotall
        , dupnames
        , extended
        , extra
        , firstline
        , multiline
        {-, newline_any-}
        {-, newline_anycrlf-}
        , newline_cr
        , newline_crlf
        , newline_lf
        , no_auto_capture
        , ungreedy
        , utf8
        , no_utf8_check

        -- ** PCRE exec-time bit flags
        , PCREExecOption

        , exec_anchored
        {-, exec_newline_any     -}
        {-, exec_newline_anycrlf -}
        , exec_newline_cr
        , exec_newline_crlf
        , exec_newline_lf
        , exec_notbol
        , exec_noteol
        , exec_notempty
        , exec_no_utf8_check
        , exec_partial

    ) where

import Text.Regex.PCRE.Light.Base

-- Strings
import qualified Data.ByteString          as S

#if __GLASGOW_HASKELL__ >= 608
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe   as S
#else
import qualified Data.ByteString.Base     as S
#endif

import System.IO.Unsafe (unsafePerformIO)
import Data.List (sortBy)
import Data.Function (on)

-- Foreigns
import Foreign (newForeignPtr, withForeignPtr)
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String
import Foreign.Storable
import Foreign.Marshal.Alloc

-- | 'compile'
--
-- Compile a perl-compatible regular expression stored in a strict bytestring.
--
-- An example
--
-- > let r = compile (pack "^(b+|a){1,2}?bc") []
--
-- Or using GHC's -XOverloadedStrings flag, and importing
-- Data.ByteString.Char8, we can avoid the pack:
--
-- > let r = compile "^(b+|a){1,2}?bc" []
--
-- If the regular expression is invalid, an exception is thrown.
-- If this is unsuitable, 'compileM' is availlable, which returns failure
-- in a monad.
--
-- To do case insentive matching,
--
-- > compile "^(b+|a){1,2}?bc" [caseless]
--
-- Other flags are documented below.
--
-- The resulting abstract regular expression can be passed to 'match'
-- for matching against a subject string.
--
-- The arguments are:
--
-- * 'pat': A ByteString containing the regular expression to be compiled.
--
-- * 'flags', optional bit flags. If 'Nothing' is provided, defaults are used.
--
-- Valid compile-time flags are:
--
-- * 'anchored'        - Force pattern anchoring
--
-- * 'auto_callout'    - Compile automatic callouts
--
-- * 'bsr_anycrlf'     - \\R matches only CR, LF, or CRLF
--
-- * 'bsr_unicode'     - \\R matches all Unicode line endings
--
-- * 'caseless'        - Do caseless matching
--
-- * 'dollar_endonly'  - '$' not to match newline at end
--
-- * 'dotall'          - matches anything including NL
--
-- * 'dupnames'        - Allow duplicate names for subpatterns
--
-- * 'extended'        - Ignore whitespace and # comments
--
-- * 'extra'           - PCRE extra features (not much use currently)
--
-- * 'firstline'       - Force matching to be  before  newline
--
-- * 'multiline'       - '^' and '$' match newlines within data
--
-- * 'newline_any'     - Recognize any Unicode newline sequence
--
-- * 'newline_anycrlf' - Recognize CR, LF, and CRLF as newline sequences
--
-- * 'newline_cr'      - Set CR as the newline sequence
--
-- * 'newline_crlf'    - Set CRLF as the newline sequence
--
-- * 'newline_lf'      - Set LF as the newline sequence
--
-- * 'no_auto_capture' - Disable numbered capturing parentheses (named ones available)
--
-- * 'ungreedy'        - Invert greediness of quantifiers
--
-- * 'utf8'            - Run in UTF-8 mode
--
-- * 'no_utf8_check'   - Do not check the pattern for UTF-8 validity
--
-- The regex is allocated via malloc on the C side, and will be
-- deallocated by the runtime when the Haskell value representing it
-- goes out of scope.
--
-- See 'man pcreapi for more details.
--
-- Caveats: patterns with embedded nulls, such as "\0*" seem to be
-- mishandled, as this won't currently match the subject "\0\0\0".
--
compile :: S.ByteString -> [PCREOption] -> Regex
compile s o = case compileM s o of
    Right r -> r
    Left  e -> error ("Text.Regex.PCRE.Light: Error in regex: " ++ e)

------------------------------------------------------------------------

-- | 'compileM'
-- A safe version of 'compile' with failure wrapped in an Either.
--
-- Examples,
--
-- > > compileM ".*" [] :: Either String Regex
-- > Right (Regex 0x000000004bb5b980 ".*")
--
-- > > compileM "*" [] :: Either String Regex
-- > Left "nothing to repeat"
--
compileM :: S.ByteString -> [PCREOption] -> Either String Regex
compileM str os = unsafePerformIO $
  S.useAsCString str $ \pattern -> do
    alloca $ \errptr       -> do
    alloca $ \erroffset    -> do
        pcre_ptr <- c_pcre_compile pattern (combineOptions os) errptr erroffset nullPtr
        if pcre_ptr == nullPtr
            then do
                err <- peekCString =<< peek errptr
                return (Left err)
            else do
                reg <- newForeignPtr finalizerFree pcre_ptr -- release with free()
                return (Right (Regex reg str))

-- Possible improvements: an 'IsString' instance could be defined
-- for 'Regex', which would allow the compiler to insert calls to
-- 'compile' based on the type:
--
-- The following would be valid:
--
-- > match "a.*b" "abcdef" []
--
-- and equivalent to:
--
-- > match (either error id (compile "a.*b")) "abcdef" []

-- | 'match'
--
-- Matches a compiled regular expression against a given subject string,
-- using a matching algorithm that is similar to Perl's. If the subject
-- string doesn't match the regular expression, 'Nothing' is returned,
-- otherwise the portion of the string that matched is returned, along
-- with any captured subpatterns.
--
-- The arguments are:
--
-- * 'regex', a PCRE regular expression value produced by compile
--
-- * 'subject', the subject string to match against
--
-- * 'options', an optional set of exec-time flags to exec.
--
-- Available runtime options are:
--
-- * 'exec_anchored'        - Match only at the first position
--
-- * 'exec_newline_any'     - Recognize any Unicode newline sequence
--
-- * 'exec_newline_anycrlf' - Recognize CR, LF, and CRLF as newline sequences
--
-- * 'exec_newline_cr'      - Set CR as the newline sequence
--
-- * 'exec_newline_crlf'    - Set CRLF as the newline sequence
--
-- * 'exec_newline_lf'      - Set LF as the newline sequence
--
-- * 'exec_notbol'          - Subject is not the beginning of a line
--
-- * 'exec_noteol'          - Subject is not the end of a line
--
-- * 'exec_notempty'        - An empty string is not a valid match
--
-- * 'exec_no_utf8_check'   - Do not check the subject for UTF-8
--
-- * 'exec_partial'         - Return PCRE_ERROR_PARTIAL for a partial match
--
-- The result value, and any captured subpatterns, are returned.
-- If the regex is invalid, or the subject string is empty, Nothing
-- is returned.
--
match :: Regex -> S.ByteString -> [PCREExecOption] -> Maybe [S.ByteString]
match (Regex pcre_fp _) subject os = unsafePerformIO $ do
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    n_capt <- fullInfoInt pcre_ptr info_capturecount

    -- The smallest  size  for ovector that will allow for n captured
    -- substrings, in addition to the offsets  of  the  substring
    -- matched by the whole pattern, is (n+1)*3. (man pcreapi)

    let ovec_size = (n_capt + 1) * 3
        ovec_bytes = ovec_size * size_of_cint

    allocaBytes ovec_bytes $ \ovec -> do

        let (str_fp, off, len) = S.toForeignPtr subject
        withForeignPtr str_fp $ \cstr -> do
            r <- c_pcre_exec
                         pcre_ptr
                         nullPtr
                         (cstr `plusPtr` off) -- may contain binary zero bytes.
                         (fromIntegral len)
                         0
                         (combineExecOptions os)
                         ovec
                         (fromIntegral ovec_size)

            if r < 0 -- errors, or error_no_match
                then return Nothing
                else let loop n o acc =
                            if n == r
                              then return (Just (reverse acc))
                              else do
                                    i <- peekElemOff ovec $! o
                                    j <- peekElemOff ovec (o+1)
                                    let s = substring i j subject
                                    s `seq` loop (n+1) (o+2) (s : acc)
                     in loop 0 0 []

    -- The  first  two-thirds  of ovec is used to pass back captured
    -- substrings When  a  match  is  successful, information about captured
    -- substrings is returned in pairs of integers,  starting  at the
    -- beginning of ovector, and continuing up to two-thirds of its length at
    -- the most.  The first pair, ovector[0] and ovector[1], identify the
    -- portion of the subject string matched  by  the entire pattern.  The next
    -- pair is used for the first capturing subpattern,  and  so on.  The
    -- value returned  by pcre_exec() is one more than the highest num- bered
    -- pair that has been set. For  example,  if  two  sub- strings  have been
    -- captured, the returned value is 3.

  where
    -- The first element of a pair is set  to  the offset of the first
    -- character in a substring, and the second is set to the offset of the
    -- first character after  the  end of a substring.
    substring :: CInt -> CInt -> S.ByteString -> S.ByteString
    substring x y _ | x == y = S.empty -- XXX an unset subpattern
    substring a b s = end -- note that we're not checking...
        where
            start = S.unsafeDrop (fromIntegral a) s
            end   = S.unsafeTake (fromIntegral (b-a)) start


-- Wrapper around c_pcre_fullinfo for integer values
fullInfoInt pcre_ptr what =
  alloca $ \n_ptr -> do
    c_pcre_fullinfo pcre_ptr nullPtr what n_ptr
    return . fromIntegral =<< peek (n_ptr :: Ptr CInt)


-- | 'captureCount'
--
-- Returns the number of captures in a 'Regex'. Correctly ignores non-capturing groups
-- like @(?:abc)@.
--
-- >>> captureCount (compile "(?<one>abc) (def) (?:non-captured) (?<three>ghi)" [])
-- 3
captureCount :: Regex -> Int
captureCount (Regex pcre_fp _) = unsafePerformIO $
  withForeignPtr pcre_fp $ \pcre_ptr ->
    fullInfoInt pcre_ptr info_capturecount


-- | 'captureNames'
--
-- Returns the names and numbers of all named subpatterns in the regular
-- expression. Groups are zero-indexed. Unnamed groups are counted, but don't appear in the
-- result list.
--
-- >>> captureNames (compile "(?<one>abc) (def) (?<three>ghi)")
-- [("one", 0), ("three", 2)]
captureNames :: Regex -> [(S.ByteString, Int)]
captureNames (Regex pcre_fp _) = unsafePerformIO $
  withForeignPtr pcre_fp $ \pcre_ptr -> do
    count     <- fullInfoInt pcre_ptr info_namecount
    entrysize <- fullInfoInt pcre_ptr info_nameentrysize

    buf <- alloca $ \n_ptr -> do
      c_pcre_fullinfo pcre_ptr nullPtr info_nametable n_ptr
      buf <- peek n_ptr
      S.packCStringLen (buf, count*entrysize)

    let results = split entrysize buf
        zeroIndexed = fmap (subtract 1) <$> results
        sorted = sortBy (compare `on` snd) zeroIndexed
    return sorted

  where
    -- Split the nametable buffer into entries. Each entry has a fixed size in
    -- bytes. The first two bytes in each entry store the pattern number in
    -- big-endian format, the bytes following that contain the nul-terminated
    -- name of the subpattern.
    split :: Int -> S.ByteString -> [(S.ByteString, Int)]
    split entrysize buf
      | S.null buf = []
      | otherwise =
        let
          (entry, tail) = S.splitAt entrysize buf
          idx = fromIntegral . S.index entry
          num = idx 0 * 256 + idx 1
          name = S.takeWhile (/= 0) $ S.drop 2 entry
        in (name, num) : split entrysize tail
