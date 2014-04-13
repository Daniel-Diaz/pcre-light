{-# LANGUAGE CPP, ForeignFunctionInterface, GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module   : Text.Regex.PCRE.Light.Base
-- Copyright: Copyright (c) 2007-2008, Don Stewart
--
-- Documentation based on /man pcreapi/, written by Philip Hazel, 2007.
--
-- License   : BSD3
-- Maintainer:  Don Stewart <dons@galois.com>
-- Stability :  experimental
-- Portability: CPP, FFI
-- Tested with: GHC 6.8.2
--
-- Raw FFI bindings to PCRE functions and constants. 
--

module Text.Regex.PCRE.Light.Base (

        -- * A PCRE structure
          PCRE
        , Regex(..)

        -- * C exports
        , c_pcre_compile
        , c_pcre_exec
        , c_pcre_fullinfo

        ------------------------------------------------------------------------

        -- * PCRE Options, an abstract newtyped Num wrapper over a CInt
        , PCREOption
        , combineOptions

        , anchored , auto_callout {-, bsr_anycrlf-}
        {-, bsr_unicode-} , caseless , dollar_endonly
        , dotall , dupnames , extended
        , extra , firstline , multiline
        {-, newline_any-} {-, newline_anycrlf-} , newline_cr
        , newline_crlf , newline_lf , no_auto_capture
        , ungreedy , utf8 , no_utf8_check

        -- * PCRE exec-time options, an abstract, newtyped Num wrapper over CInt
        , PCREExecOption
        , combineExecOptions

        , exec_anchored {-, exec_newline_any , exec_newline_anycrlf-}
        , exec_newline_cr , exec_newline_crlf , exec_newline_lf
        , exec_notbol , exec_noteol , exec_notempty
        , exec_no_utf8_check , exec_partial

        ------------------------------------------------------------------------ 
        -- * PCRE Errors
        , PCREError

        , error_nomatch , error_null , error_badoption
        , error_badmagic {-, error_unknown_opcode-} , error_unknown_node
        , error_nomemory , error_nosubstring , error_matchlimit
        , error_callout , error_badutf8 , error_badutf8_offset
        , error_partial , error_badpartial , error_internal
        , error_badcount , error_dfa_uitem , error_dfa_ucond
        , error_dfa_umlimit , error_dfa_wssize , error_dfa_recurse
        , error_recursionlimit {-, error_nullwslimit-} {-, error_badnewline-}

        -- * PCRE Info
        , PCREInfo

        , info_options , info_size , info_capturecount
        , info_backrefmax , info_firstbyte , info_firstchar
        , info_firsttable , info_lastliteral , info_nameentrysize
        , info_namecount , info_nametable , info_studysize
        , info_default_tables {-, info_okpartial-} {-, info_jchanged-}
        {-, info_hascrorlf-}

        -- * PCRE Configuration
        , PCREConfig

        , config_utf8 , config_newline , config_link_size
        , config_posix_malloc_threshold , config_match_limit
        , config_stackrecurse , config_unicode_properties
        , config_match_limit_recursion {-, config_bsr-}

        -- * PCRE Extra
        , PCREExtraFlags

        , extra_study_data , extra_match_limit , extra_callout_data
        , extra_tables , extra_match_limit_recursion,

        ------------------------------------------------------------------------

        size_of_cint

    ) where

-- Foreigns
import Foreign
import Foreign.Ptr
import Foreign.C.Types
import Foreign.C.String

import qualified Data.ByteString.Char8 as S

#include <pcre.h>

-- | Get sizeof CInt from hsc2hs
size_of_cint :: Int
size_of_cint = #const (sizeof(int))

------------------------------------------------------------------------
-- Types

-- | An abstract pointer to a compiled PCRE Regex structure
-- The structure allocated by the PCRE library will be deallocated
-- automatically by the Haskell storage manager.
--
data Regex = Regex {-# UNPACK #-} !(ForeignPtr PCRE)
                   {-# UNPACK #-} !S.ByteString
        deriving (Eq, Ord, Show)

type PCRE = ()

------------------------------------------------------------------------

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
--
newtype PCREOption = PCREOption { unPCREOption :: PCREOption_ }
#if __GLASGOW_HASKELL__
    deriving (Eq,Ord,Show,Read)
#endif

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0

-- Now follows the user-visible options to _exec and _compile.
-- To avoid type errors, we newtype the underlying CInts, and 
-- statically differentiate PCREOptions from non-PCREOptions
--
-- The safety can still be defeated using numeric literals though,
-- and other Num operations. We could do more to protect against this.
-- (a smart constructor for .|.)

-- | 'anchored'
--
-- If this bit is set, the pattern is forced to be /anchored/, that is,
-- it is constrained to match only at the first matching point in the
-- string that is being searched (the /subject string/). This effect can
-- also be achieved by appropriate constructs in the pattern itself, which
-- is the only way to do it in Perl.  
--
anchored           :: PCREOption
anchored           = PCREOption anchored_cint

-- | 'auto_callout'
--
-- If this bit is set, "compile" automatically inserts callout
-- items, all with number 255, before each pattern item. For discussion
-- of the callout facility, see the man pcrecallout documentation
--
auto_callout       :: PCREOption
auto_callout       = PCREOption auto_callout_cint

-- | 'bsr_anycrlf' and 'bsr_unicode'
--
-- These options (which are mutually exclusive) control what the \\R escape
-- sequence matches. The choice is either to match only CR, LF, or CRLF, or to
-- match any Unicode new- line sequence. The default is specified when PCRE is
-- built. It can be overridden from within the pattern, or by setting an option
-- when a compiled pattern is matched.
--
-- bsr_anycrlf        :: PCREOption
-- bsr_anycrlf        = PCREOption bsr_anycrlf_cint

-- | 'bsr_unicode'. See 'bse_anycrlf'
--
-- bsr_unicode        :: PCREOption
-- bsr_unicode        = PCREOption bsr_unicode_cint

-- | 'caseless'
--
-- If this bit is set, letters in the pattern match both upper and lower case
-- letters. It is equivalent to Perl's \/i option, and it can be changed within a
-- pattern by a (?i) option setting. In UTF-8 mode, PCRE always understands the
-- concept of case for characters whose values are less than 128, so caseless
-- matching is always possible. For characters with higher values, the concept of
-- case is supported if PCRE is compiled with Unicode property sup- port, but not
-- otherwise. If you want to use caseless matching for characters 128 and above,
-- you must ensure that PCRE is compiled with Unicode property support as well as
-- with UTF-8 support.
-- 
caseless           :: PCREOption
caseless           = PCREOption caseless_cint

-- | 'dollar_endonly'
--
-- If this bit is set, a dollar metacharacter in the pattern matches only at
-- the end of the subject string. Without this option, a dollar also matches
-- immediately before a newline at the end of the string (but not before any other
-- newlines). The 'dollar_endonly' option is ignored if 'multiline'
-- is set.  There is no equivalent to this option in Perl, and no way to set it
-- within a pattern.
--
dollar_endonly     :: PCREOption
dollar_endonly     = PCREOption dollar_endonly_cint

-- | 'dotall'
--
-- If this bit is set, a dot metacharater in the pattern matches all
-- characters, including those that indicate newline. Without it, a dot does
-- not match when the current position is at a newline. This option is
-- equivalent to Perl's \/s option, and it can be changed within a pattern by a
-- (?s) option setting. A negative class such as [^a] always matches newline
-- characters, independent of the setting of this option.
--
dotall             :: PCREOption
dotall             = PCREOption dotall_cint

-- | 'dupnames'
--
-- If this bit is set, names used to identify capturing subpatterns need not be
-- unique. This can be helpful for certain types of pattern when it is known
-- that only one instance of the named subpattern can ever be matched. There are
-- more details of named subpatterns in the /man pcreapi/ documentation.
--
dupnames           :: PCREOption
dupnames           = PCREOption dupnames_cint

-- | 'extended'
--
-- If this bit is set, whitespace data characters in the pattern are totally
-- ignored except when escaped or inside a character class. Whitespace does not
-- include the VT character (code 11). In addition, characters between an
-- unescaped \# outside a character class and the next newline, inclusive, are
-- also ignored. This is equivalent to Perl's \/x option, and it can be changed
--within a pattern by a (?x) option setting.
--
-- This option makes it possible to include comments inside complicated
-- patterns. Note, however, that this applies only to data characters. Whitespace
-- characters may never appear within special character sequences in a pattern,
-- for example within the sequence (?( which introduces a conditional subpattern.
--
extended           :: PCREOption
extended           = PCREOption extended_cint

-- | 'extra'
--
-- This option was invented in order to turn on additional functionality of
-- PCRE that is incompatible with Perl, but it is currently of very little use.
-- When set, any backslash in a pattern that is followed by a letter that has no
-- special meaning causes an error, thus reserving these combinations for future
-- expansion. By default, as in Perl, a backslash followed by a letter with no
-- special meaning is treated as a literal. (Perl can, however, be persuaded to
-- give a warning for this.) There are at present no other features controlled by
-- this option. It can also be set by a (?X) option setting within a pattern. 
--
extra              :: PCREOption
extra              = PCREOption extra_cint

-- | 'firstline'
--
-- If this option is set, an unanchored pattern is required to match before or
-- at the first newline in the subject string, though the matched text may
--continue over the newline.
--
firstline          :: PCREOption
firstline          = PCREOption firstline_cint

-- | 'multiline'
--
--  By default, PCRE treats the subject string as consisting of a single line
-- of characters (even if it actually contains newlines). The /start of line/
-- metacharacter (^) matches only at the start of the string, while the /end of line/
--  metacharacter ($) matches only at the end of the string, or before a
-- terminating newline (unless 'dollar_endonly' is set). This is the same
-- as Perl.
--
-- When 'multiline' it is set, the /start of line/ and /end of line/
-- constructs match immediately following or immediately before internal newlines
-- in the subject string, respectively, as well as at the very start and end. This
-- is equivalent to Perl's \/m option, and it can be changed within a pattern by a
-- (?m) option setting. If there are no newlines in a subject string, or no occur-
-- rences of ^ or $ in a pattern, setting PCRE_MULTILINE has no effect.
-- 
multiline          :: PCREOption
multiline          = PCREOption multiline_cint

-- | newline_cr', 'newline_lf', 'newline_crlf',
-- 'newline_anycrlf', 'newline_any'
--
-- These options override the default newline definition that
-- was chosen when PCRE was built. Setting the first or the
-- second specifies that a newline is indicated by a single
-- character (CR or LF, respectively). Setting 'newline_crlf' specifies
-- that a newline is indicated by the two-character CRLF sequence.
-- Setting 'newline_anycrlf'
-- specifies that any of the three preceding sequences should
-- be recognized. Setting 'newline_any' specifies that any
-- Unicode newline sequence should be recognized. The Unicode
-- newline sequences are the three just mentioned, plus the
-- single characters VT (vertical tab, U+000B), FF (formfeed,
-- U+000C), NEL (next line, U+0085), LS (line separator,
-- U+2028), and PS (paragraph separator, U+2029). The last
-- two are recognized only in UTF-8 mode.
-- 
-- The newline setting in the options word uses three bits
-- that are treated as a number, giving eight possibilities.
-- Currently only six are used (default plus the five values
-- above). This means that if you set more than one newline
-- option, the combination may or may not be sensible. For
-- example, 'newline_cr' with 'newline_lf' is equivalent to
-- 'newline_crlf', but other combinations may yield unused numbers and
-- cause an error.
-- 
-- The only time that a line break is specially recognized
-- when compiling a pattern is if 'extended' is set, and
-- an unescaped \# outside a character class is encountered.
-- This indicates a comment that lasts until after the next
-- line break sequence. In other circumstances, line break
-- sequences are treated as literal data, except that in
-- 'extended' mode, both CR and LF are treated as whitespace characters
-- and are therefore ignored.  -- 
--
-- The newline option that is set at compile time becomes the
-- default that is used for 'exec' but it can be overridden.
-- 
-- newline_any        :: PCREOption
-- newline_any        = PCREOption newline_any_cint

-- | 'newline_anycrlf', see 'newline_any'
-- newline_anycrlf    :: PCREOption
-- newline_anycrlf    = PCREOption newline_anycrlf_cint

-- | 'newline_cr', see 'newline_any'
newline_cr         :: PCREOption
newline_cr         = PCREOption newline_cr_cint

-- | 'newline_crlf', see 'newline_any'
newline_crlf       :: PCREOption
newline_crlf       = PCREOption newline_crlf_cint

-- | 'newline_lf', see 'newline_any'
newline_lf         :: PCREOption
newline_lf         = PCREOption newline_lf_cint

-- | 'no_auto_capture'
--
-- If this option is set, it disables the use of numbered
-- capturing parentheses in the pattern. Any opening paren-
-- thesis that is not followed by ? behaves as if it were
-- followed by ?: but named parentheses can still be used for
-- capturing (and they acquire numbers in the usual way).
-- There is no equivalent of this option in Perl.
--
no_auto_capture    :: PCREOption
no_auto_capture    = PCREOption no_auto_capture_cint

-- | 'ungreedy'
--
-- This option inverts the /greediness/ of the quantifiers so
-- that they are not greedy by default, but become greedy if
-- followed by /?/. It is not compatible with Perl. It can
-- also be set by a (?U) option setting within the pattern.
--
ungreedy           :: PCREOption
ungreedy           = PCREOption ungreedy_cint

-- | 'utf8'
--
-- This option causes PCRE to regard both the pattern and the
-- subject as strings of UTF-8 characters instead of single-byte character
-- strings. However, it is available only when 
-- PCRE is built to include UTF-8 support. If not, the use of
-- this option provokes an error. Details of how this option
-- changes the behaviour of PCRE are given in the section on
-- UTF-8 support in the main pcre page.
--
utf8               :: PCREOption
utf8               = PCREOption utf8_cint

-- | 'no_utf8_check'
--
-- When PCRE_UTF8 is set, the validity of the pattern as a
-- UTF-8 string is automatically checked. There is a discussion 
-- about the validity of UTF-8 strings in the main pcre
-- page. If an invalid UTF-8 sequence of bytes is found,
-- compile() returns an error. If you already know that
-- your pattern is valid, and you want to skip this check for
-- performance reasons, you can set the 'no_utf8_check'
-- option. When it is set, the effect of passing an invalid
-- UTF-8 string as a pattern is undefined. It may cause your
-- program to crash. Note that this option can also be passed
-- to 'exec', to suppress the UTF-8 validity checking of subject strings.
--
no_utf8_check      :: PCREOption
no_utf8_check      = PCREOption no_utf8_check_cint

-- Internal name for hsc2hs to bind to.
type PCREOption_ = CInt

-- PCRE compile options, as CInts
#{enum PCREOption_,
  , anchored_cint        = PCRE_ANCHORED
  , auto_callout_cint    = PCRE_AUTO_CALLOUT
  , caseless_cint        = PCRE_CASELESS
  , dollar_endonly_cint  = PCRE_DOLLAR_ENDONLY
  , dotall_cint          = PCRE_DOTALL
  , dupnames_cint        = PCRE_DUPNAMES
  , extended_cint        = PCRE_EXTENDED
  , extra_cint           = PCRE_EXTRA
  , firstline_cint       = PCRE_FIRSTLINE
  , multiline_cint       = PCRE_MULTILINE
  , newline_cr_cint      = PCRE_NEWLINE_CR
  , newline_crlf_cint    = PCRE_NEWLINE_CRLF
  , newline_lf_cint      = PCRE_NEWLINE_LF
  , no_auto_capture_cint = PCRE_NO_AUTO_CAPTURE
  , ungreedy_cint        = PCRE_UNGREEDY
  , utf8_cint            = PCRE_UTF8
  , no_utf8_check_cint   = PCRE_NO_UTF8_CHECK
  }

--  , bsr_anycrlf_cint     = PCRE_BSR_ANYCRLF
--  , bsr_unicode_cint     = PCRE_BSR_UNICODE
--  , newline_any_cint     = PCRE_NEWLINE_ANY
--  , newline_anycrlf_cint = PCRE_NEWLINE_ANYCRLF

------------------------------------------------------------------------

-- | PCRE exec options, to be passed to exec
newtype PCREExecOption = PCREExecOption { unPCREExecOption :: PCREExecOption_ }
#if __GLASGOW_HASKELL__
    deriving (Eq,Ord,Show,Read)
#endif

-- | Combine a list of exec options into a single option, using bitwise (.|.)
combineExecOptions :: [PCREExecOption] -> PCREExecOption
combineExecOptions = PCREExecOption . foldr ((.|.) . unPCREExecOption) 0

-- | 'anchored'.
--
-- The 'anchored' option limits 'exec' to matching at
-- the first matching position. If a pattern was compiled
-- with 'anchored', or turned out to be anchored by virtue
-- of its contents, it cannot be made unachored at matching
-- time.
exec_anchored              :: PCREExecOption
exec_anchored              = PCREExecOption exec_anchored_cint

-- | 'newline_cr', 'newline_lf',
-- 'newline_crlf', 'newline_anycrlf', 'newline_any'
--
-- These options override the newline definition that was
-- chosen or defaulted when the pattern was compiled. For
-- details, see the description of 'compile' above. Dur-
-- ing matching, the newline choice affects the behaviour of
-- the dot, circumflex, and dollar metacharacters. It may
-- also alter the way the match position is advanced after a
-- match failure for an unanchored pattern.
--
-- When 'newline_crlf', 'newline_anycrlf', or 'newline_any'
-- is set, and a match attempt for an unanchored
-- pattern fails when the current position is at a CRLF
-- sequence, and the pattern contains no explicit matches for
-- CR or LF characters, the match position is advanced by two
-- characters instead of one, in other words, to after the
-- CRLF.
--
-- The above rule is a compromise that makes the most common
-- cases work as expected. For example, if the pattern is .+A
-- (and the 'dotall' option is not set), it does not match
-- the string /\\r\\nA/ because, after failing at the start, it
-- skips both the CR and the LF before retrying. However, the
-- pattern /[\\r\\n]A/ does match that string, because it contains
-- an explicit CR or LF reference, and so advances only
-- by one character after the first failure.
--
-- An explicit match for CR of LF is either a literal appear-
-- ance of one of those characters, or one of the \\r or \\n
-- escape sequences. Implicit matches such as [^X] do not
-- count, nor does \\s (which includes CR and LF in the char-
-- acters that it matches).
--
-- Notwithstanding the above, anomalous effects may still
-- occur when CRLF is a valid newline sequence and explicit
-- \\r or \\n escapes appear in the pattern.
--
-- exec_newline_any           :: PCREExecOption
-- exec_newline_any           = PCREExecOption exec_newline_any_cint

-- | 'exec_newline_anycrlf', see 'exec_newline_any'
-- exec_newline_anycrlf       :: PCREExecOption
-- exec_newline_anycrlf       = PCREExecOption exec_newline_anycrlf_cint

-- | 'exec_newline_cr', see 'exec_newline_any'
exec_newline_cr            :: PCREExecOption
exec_newline_cr            = PCREExecOption exec_newline_cr_cint

-- | 'exec_newline_crlf', see 'exec_newline_any'
exec_newline_crlf          :: PCREExecOption
exec_newline_crlf          = PCREExecOption exec_newline_crlf_cint

-- | 'exec_newline_lf', see 'exec_newline_any'
exec_newline_lf            :: PCREExecOption
exec_newline_lf            = PCREExecOption exec_newline_lf_cint

-- | 'PCRE_NOTBOL'
--
-- This option specifies that first character of the subject
-- string is not the beginning of a line, so the circumflex
-- metacharacter should not match before it. Setting this
-- without 'multiline' (at compile time) causes circumflex
-- never to match. This option affects only the behaviour of
-- the circumflex metacharacter. It does not affect \\A.
--
exec_notbol                :: PCREExecOption
exec_notbol                = PCREExecOption exec_notbol_cint

-- | 'noteol'
--
-- This option specifies that the end of the subject string
-- is not the end of a line, so the dollar metacharacter
-- should not match it nor (except in multiline mode) a newline
-- immediately before it. Setting this without 'multiline' 
-- (at compile time) causes dollar never to match.
-- This option affects only the behaviour of the dollar
-- metacharacter. It does not affect \\Z or \\z.
--
exec_noteol                :: PCREExecOption
exec_noteol                = PCREExecOption exec_noteol_cint

-- | PCRE_NOTEMPTY
--
-- An empty string is not considered to be a valid match if
-- this option is set. If there are alternatives in the pattern,
-- they are tried. If all the alternatives match the
-- empty string, the entire match fails. For example, if the
-- pattern
--
-- > a?b?
--
-- is applied to a string not beginning with /a/ or /b/, it
-- matches the empty string at the start of the subject. With
-- 'notempty' set, this match is not valid, so 'PCRE
-- searches further into the string for occurrences of /a/ or
-- /b/.
--
-- Perl has no direct equivalent of 'notempty', but it
-- does make a special case of a pattern match of the empty
-- string within its split() function, and when using the \/g
-- modifier. It is possible to emulate Perl's behaviour after
-- matching a null string by first trying the match again at
-- the same offset with PCRE_NOTEMPTY and PCRE_ANCHORED, and
-- then if that fails by advancing the starting offset (see
-- below) and trying an ordinary match again. There is some
-- code that demonstrates how to do this in the pcredemo.c
-- sample program.
--
exec_notempty              :: PCREExecOption
exec_notempty              = PCREExecOption exec_notempty_cint

-- | 'no_utf8_check'
--
-- When 'utf8' is set at compile time, the validity of the
-- subject as a UTF-8 string is automatically checked when
-- exec() is subsequently called. The value of
-- startoffset is also checked to ensure that it points to
-- the start of a UTF-8 character. There is a discussion
-- about the validity of UTF-8 strings in the section on
-- UTF-8 support in the main pcre page. If an invalid UTF-8
-- sequence of bytes is found, exec() returns the error
-- 'error_badutf8'. If startoffset contains an invalid
-- value, 'error_badutf8_offset' is returned.
--
-- If you already know that your subject is valid, and you
-- want to skip these checks for performance reasons, you can
-- set the 'no_utf8_check' option when calling
-- 'exec'. You might want to do this for the second and
-- subsequent calls to exec() if you are making repeated
-- calls to find all the matches in a single subject string.
-- However, you should be sure that the value of startoffset
-- points to the start of a UTF-8 character. When
-- 'no_utf8_check' is set, the effect of passing an
-- invalid UTF-8 string as a subject, or a value of startoff-
-- set that does not point to the start of a UTF-8 character,
-- is undefined. Your program may crash.
--
exec_no_utf8_check         :: PCREExecOption
exec_no_utf8_check         = PCREExecOption exec_no_utf8_check_cint

-- | 'partial'
--
-- This option turns on the partial matching feature. If the
-- subject string fails to match the pattern, but at some
-- point during the matching process the end of the subject
-- was reached (that is, the subject partially matches the
-- pattern and the failure to match occurred only because
-- there were not enough subject characters), 'exec'
-- returns 'error_partial' instead of 'error_nomatch'.
-- When 'partial' is used, there are restrictions on what
-- may appear in the pattern. These are discussed in the
-- pcrepartial documentation.
--
exec_partial               :: PCREExecOption
exec_partial               = PCREExecOption exec_partial_cint

-- Internal name for hsc2hs to bind to.
type PCREExecOption_ = CInt

-- PCRE exec options
#{enum PCREExecOption_,
  , exec_anchored_cint        = PCRE_ANCHORED
  , exec_newline_cr_cint      = PCRE_NEWLINE_CR
  , exec_newline_crlf_cint    = PCRE_NEWLINE_CRLF
  , exec_newline_lf_cint      = PCRE_NEWLINE_LF
  , exec_notbol_cint          = PCRE_NOTBOL
  , exec_noteol_cint          = PCRE_NOTEOL
  , exec_notempty_cint        = PCRE_NOTEMPTY
  , exec_no_utf8_check_cint   = PCRE_NO_UTF8_CHECK
  , exec_partial_cint         = PCRE_PARTIAL
  }

--  , exec_newline_any_cint     = PCRE_NEWLINE_ANY
--  , exec_newline_anycrlf_cint = PCRE_NEWLINE_ANYCRLF
--  , dfa_shortest   = PCRE_DFA_SHORTEST
--  , dfa_restart    = PCRE_DFA_RESTART

------------------------------------------------------------------------

-- | A type for PCRE Errors: exec-time error codes.
type PCREError = CInt

#{enum PCREError,
    , error_nomatch        = PCRE_ERROR_NOMATCH
    , error_null           = PCRE_ERROR_NULL
    , error_badoption      = PCRE_ERROR_BADOPTION
    , error_badmagic       = PCRE_ERROR_BADMAGIC
    , error_unknown_node   = PCRE_ERROR_UNKNOWN_NODE
    , error_nomemory       = PCRE_ERROR_NOMEMORY
    , error_nosubstring    = PCRE_ERROR_NOSUBSTRING
    , error_matchlimit     = PCRE_ERROR_MATCHLIMIT
    , error_callout        = PCRE_ERROR_CALLOUT
    , error_badutf8        = PCRE_ERROR_BADUTF8
    , error_badutf8_offset = PCRE_ERROR_BADUTF8_OFFSET
    , error_partial        = PCRE_ERROR_PARTIAL
    , error_badpartial     = PCRE_ERROR_BADPARTIAL
    , error_internal       = PCRE_ERROR_INTERNAL
    , error_badcount       = PCRE_ERROR_BADCOUNT
    , error_dfa_uitem      = PCRE_ERROR_DFA_UITEM
    , error_dfa_ucond      = PCRE_ERROR_DFA_UCOND
    , error_dfa_umlimit    = PCRE_ERROR_DFA_UMLIMIT
    , error_dfa_wssize     = PCRE_ERROR_DFA_WSSIZE
    , error_dfa_recurse    = PCRE_ERROR_DFA_RECURSE
    , error_recursionlimit = PCRE_ERROR_RECURSIONLIMIT
    }

--  , error_unknown_opcode = PCRE_ERROR_UNKNOWN_OPCODE
--  , error_nullwslimit    = PCRE_ERROR_NULLWSLIMIT
--  , error_badnewline     = PCRE_ERROR_BADNEWLINE

------------------------------------------------------------------------
-- Request types for fullinfo() */

-- | PCRE Info requests -- provides information about the compiled pattern.
type PCREInfo = CInt

#{enum PCREInfo,
    , info_options         = PCRE_INFO_OPTIONS
    , info_size            = PCRE_INFO_SIZE
    , info_capturecount    = PCRE_INFO_CAPTURECOUNT
    , info_backrefmax      = PCRE_INFO_BACKREFMAX
    , info_firstbyte       = PCRE_INFO_FIRSTBYTE
    , info_firstchar       = PCRE_INFO_FIRSTCHAR
    , info_firsttable      = PCRE_INFO_FIRSTTABLE
    , info_lastliteral     = PCRE_INFO_LASTLITERAL
    , info_nameentrysize   = PCRE_INFO_NAMEENTRYSIZE
    , info_namecount       = PCRE_INFO_NAMECOUNT
    , info_nametable       = PCRE_INFO_NAMETABLE
    , info_studysize       = PCRE_INFO_STUDYSIZE
    , info_default_tables  = PCRE_INFO_DEFAULT_TABLES
    }

--  , info_okpartial       = PCRE_INFO_OKPARTIAL
--  , info_jchanged        = PCRE_INFO_JCHANGED
--  , info_hascrorlf       = PCRE_INFO_HASCRORLF

------------------------------------------------------------------------

-- | Request types for config()
type PCREConfig = CInt

#{enum PCREConfig,
    , config_utf8                   = PCRE_CONFIG_UTF8
    , config_newline                = PCRE_CONFIG_NEWLINE
    , config_link_size              = PCRE_CONFIG_LINK_SIZE
    , config_posix_malloc_threshold = PCRE_CONFIG_POSIX_MALLOC_THRESHOLD
    , config_match_limit            = PCRE_CONFIG_MATCH_LIMIT
    , config_stackrecurse           = PCRE_CONFIG_STACKRECURSE
    , config_unicode_properties     = PCRE_CONFIG_UNICODE_PROPERTIES
    , config_match_limit_recursion  = PCRE_CONFIG_MATCH_LIMIT_RECURSION
    }

-- Not portable
--  , config_bsr                    = PCRE_CONFIG_BSR


------------------------------------------------------------------------

-- | PCREExtra.
-- A extra structure contains the following fields:
--
-- * flags        Bits indicating which fields are set
-- * study_data   Opaque data from study()
-- * match_limit  Limit on internal resource use
-- * match_limit_recursion  Limit on internal recursion depth
-- * callout_data Opaque data passed back to callouts
-- * tables       Points to character tables or is NULL
--
type PCREExtra = ()

-- | PCREExtraFlags. bit flags for extra structure.
type PCREExtraFlags = CInt

-- Bit flags for the extra structure. Do not re-arrange or redefine
-- these bits, just add new ones on the end, in order to remain compatible. */
#{enum PCREExtraFlags,
    , extra_study_data            = PCRE_EXTRA_STUDY_DATA
    , extra_match_limit           = PCRE_EXTRA_MATCH_LIMIT
    , extra_callout_data          = PCRE_EXTRA_CALLOUT_DATA
    , extra_tables                = PCRE_EXTRA_TABLES
    , extra_match_limit_recursion = PCRE_EXTRA_MATCH_LIMIT_RECURSION
    }

-- PCRE_EXP_DECL pcre *compile(const char *, int, const char **, int *, const unsigned char *);
-- PCRE_EXP_DECL int  config(int, void *);
-- PCRE_EXP_DECL int  exec(const pcre *, const extra *, PCRE_SPTR, int, int, int, int *, int);

------------------------------------------------------------------------
-- C api

{-
   pcre *pcre_compile(const char *pattern, int options,
               const char **errptr, int *erroffset,
               const unsigned char *tableptr);
-}

-- | Compile a pattern to an internal form. The pattern is a C string
-- terminated by a binary zero. A pointer to a single block of memory that is
-- obtained via pcre_malloc is returned. It is up to the caller to free
-- the memory (via pcre_free) when it is no longer required
--
-- The options argument contains various bit settings that affect the
-- compilation. It should be zero if no options are required.
--
-- If errptr is NULL, pcre_compile() returns NULL immediately.
-- Otherwise, if compilation of a pattern fails, pcre_compile() returns NULL,
-- and sets the variable pointed to by errptr to point to a textual error
-- message.
--
-- The offset from the start of the pattern to the character where the error
-- was discovered is placed in the variable pointed to by erroffset, which must
-- not be NULL.
--
foreign import ccall unsafe "pcre.h pcre_compile"
    c_pcre_compile  :: CString
                    -> PCREOption
                    -> Ptr CString
                    -> Ptr CInt
                    -> Ptr Word8
                    -> IO (Ptr PCRE)

-- Additional fields to c_pcre_compile:
--
--    errptr        Where to put an error message
--    erroffset     Offset in pattern where error was found
--    tableptr      Pointer to character tables, or NULL to to use built in

{-
       int pcre_exec(const pcre *code, const pcre_extra *extra,
            const char *subject, int length, int startoffset,
            int options, int *ovector, int ovecsize);
-}

-- | This function matches a compiled regular expression
-- against a given subject string, using a matching algorithm
-- that is similar to Perl's. It returns offsets to captured
-- substrings.
--
-- Its arguments are, in order:
--
-- * 'code'        Points to the compiled pattern (result of pcre_compile)
--
-- * 'extra'       Points to an associated pcre_extra structure (result of pcre_study), or is NULL
--
-- * 'subject'      Points to the subject string
--
-- * 'length'       Length of the subject string, in bytes
--
-- * 'startoffset'  Offset in bytes in the subject at which to start matching
--
-- * 'options'      Option bits
--
-- * 'ovector'      Points to a vector of ints for result substrings
--
-- * 'ovecsize'     Number of elements in the vector (a  multiple of 3)
--
-- Note, subject not required to be null terminated.
--
foreign import ccall unsafe "pcre.h pcre_exec"
    c_pcre_exec     :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> Ptr Word8
                    -> CInt
                    -> CInt
                    -> PCREExecOption
                    -> Ptr CInt
                    -> CInt
                    -> IO CInt

-- | Return information about a compiled pattern
foreign import ccall unsafe "pcre.h pcre_fullinfo"
    c_pcre_fullinfo :: Ptr PCRE
                    -> Ptr PCREExtra
                    -> PCREInfo
                    -> Ptr a
                    -> IO CInt

