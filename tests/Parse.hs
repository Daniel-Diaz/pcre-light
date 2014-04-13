--
-- A script to translate the pcre.c testsuite into Haskell
--

import System.Environment
import System.IO
import Data.Char
import Data.List
import Text.PrettyPrint.HughesPJ

data Test = Test String [String] [Maybe [String]]
       deriving (Eq,Show,Read)

main = do
    [f,g] <- getArgs
    inf   <- readFile f
    outf  <- readFile g

    let in_str  = lines inf
        out_str = lines outf

    let loop [] []     = []
        loop i_xs o_xs = Test r subj results   : loop (dropWhile (=="") i_ys)
                                                      (dropWhile (=="") o_ys)
           where
             ((r:subj),    i_ys) = break (== "") i_xs

             ((_:results'),o_ys) = break (== "") o_xs
             results= [ if s == "No match"
                           then Nothing
                           else Just [s]
                      | s <- filter (not . all isSpace . take 2) results'
                      ]

    print . vcat . intersperse (char ',') . map ppr . loop in_str $ out_str

breakReg ('/':rest) = 
  let s = reverse . dropWhile (/= '/') . reverse $ rest

      t = case head (reverse rest) of
               'i' -> ["caseless"]
               '/' -> []
               _   -> ["ERROR"]

  in if s == "" then ("ERROR", [])
                else (init s, t)


breakReg ('"':rest) =
  let s = reverse . dropWhile (/= '"') . reverse $ rest

      t = case head (reverse rest) of
               'i' -> ["caseless"]
               '/' -> []
               _   -> ["ERROR"]

  in if s == "" then ("ERROR", [])
                else (init s, t)

breakReg s          = ("ERROR", [])

ppr :: Test -> Doc
ppr (Test r subjs res) =
    hang (empty <+> text "testRegex" <+> text
       (show (fst $ breakReg r)) <+> 
               bracket (case snd (breakReg r) of
                                    [] -> empty
                                    [x] -> text x
                       ))
         4 $
         (bracket $ vcat $ punctuate (char ',') (map (text.show) subjs))
            $+$
         (bracket $ vcat $ punctuate (char ',') (map (text.show) res))

bracket x = char '[' <> x <> char ']'
