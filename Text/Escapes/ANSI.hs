{-# LANGUAGE BangPatterns #-}
module Text.Escapes.ANSI where

import Data.Char(isDigit)

import qualified System.IO             as SIO
import qualified System.Console.ANSI   as A

-- TODO:
--  * Add some simple smoke tests to parseANSI and stripANSI
--  * Take a look at http://www.isthe.com/chongo/tech/comp/ansi_escapes.html
--     - need 02 faint, 03 is standout, 01 is bold on, 00 is normal display
--     - Fiddle with the other SGR commands to see if it's better

printDiff :: (Eq a, Show a) => [a] -> [a] -> IO ()
printDiff = printDiff0 (0::Int)
  where printDiff0 _ []  []  = return ()
        printDiff0 i aas bbs = ppA >> printDiff0 (i + 1) as bs
          where (b,bs) = (head bbs, if null bbs then [] else tail bbs)
                (a,as) = (head aas, if null aas then [] else tail aas)
                ppA = do
                  if i > 0 then putStr "," else return ()
                  if null aas then putStrC A.Red "***"
                    else if null bbs || a /= b then putStrC A.Red (show a) -- ++ "(" ++ show b ++ ")")
                    else putStr (show a)

-- Wraps some IO op between a color set and reset command.
-- See 'hPutStrANSI' for a better API.
withColor :: A.Color -> IO b -> IO b
withColor c op = do { A.setSGR [A.SetColor A.Foreground A.Vivid c]; r <- op; A.setSGR [A.Reset]; return r }
-- Prints some text in color.
-- See 'hPutStrANSI' for a better API.
putStrC :: A.Color -> String -> IO ()
putStrC c = withColor c . putStr
-- Prints some text in color.
-- See 'hPutStrANSI' for a better API.
putStrLnC :: A.Color -> String -> IO ()
putStrLnC c = withColor c . putStrLn

-- ! Exactly like 'hPutStr', but permits ANSI escapes.  It interprets these
-- and sets the underlying console if necessary.  Hence, this works on
-- Windows and Unix.
hPutStrANSI :: SIO.Handle -> String -> IO ()
hPutStrANSI h = body
  where body s = do
          isa_tty <- SIO.hIsTerminalDevice h
          if isa_tty then colorString s
            else dontColorString s

        colorString = mapM_ handleANSI . parseANSI
        dontColorString = SIO.hPutStr h . stripANSI

        handleANSI (AString str) = SIO.hPutStr h str
        handleANSI (AEscape sgr) = A.hSetSGR h sgr
        handleANSI (AError _) = return ()
        -- handleANSI (AError err) = SIO.hPutStrLn SIO.stderr ("Tim.ANSI.hPutStrANSI: " ++ err)

-- ! See 'hPutStrANSI'.  Just adds a newline.
hPutStrLnANSI :: SIO.Handle -> String -> IO ()
hPutStrLnANSI h cs = hPutStrANSI h (cs ++ "\n")

-- ! Strips all the ANSI esacpes from a string.  Erroneous escape sequences
-- are dropped non-deterministically.
stripANSI :: String -> String
stripANSI str = concatMap handleANSI (parseANSI str)
  where handleANSI (AString s) = s
        handleANSI (AEscape _) = ""
        handleANSI (AError _) = ""

-- 0 is normal, 1 is bold, 2 is faint
aNSI_RESET = "\ESC[0m"
aNSI_RED = "\ESC[1;31m"
aNSI_DK_RED = "\ESC[0;31m"
aNSI_GRN = "\ESC[1;32m"
aNSI_DK_GRN = "\ESC[0;32m"
aNSI_YEL = "\ESC[1;33m"
aNSI_DK_YEL = "\ESC[0;33m"
aNSI_BLU = "\ESC[1;34m"
aNSI_DK_BLU = "\ESC[0;34m"
aNSI_MAG = "\ESC[1;35m"
aNSI_DK_MAG = "\ESC[0;35m"
aNSI_CYN = "\ESC[1;36m"
aNSI_DK_CYN = "\ESC[0;36m"
aNSI_WHI = "\ESC[1;37m"
aNSI_DK_WHI = "\ESC[0;37m"

printColorTable :: IO ()
printColorTable = do
  let cs = ["black", "red", "green", "yellow", "blue", "magenta", "cyan", "white"]
  flip mapM_ (zip [(0 ::Int)..] cs) $ \(ci, cstr) -> do
    -- hPutStrLnANSI SIO.stdout cstr
    let cstr_padded = replicate (16 - length cstr) ' ' ++ cstr
        str1 = ("\ESC[1;" ++ show (30+ci) ++ "m" ++ "******" ++ aNSI_RESET)
        str2 = ("\ESC[2;" ++ show (30+ci) ++ "m" ++ "******" ++ aNSI_RESET)
        str3 = ("\ESC[1;" ++ show (40+ci) ++ "m" ++ "******" ++ aNSI_RESET)
        str4 = ("\ESC[2;" ++ show (40+ci) ++ "m" ++ "******" ++ aNSI_RESET)

    putStr $ cstr_padded ++ "  "
    SIO.hFlush SIO.stdout
    hPutStrANSI SIO.stdout (str1++"  ")
    A.hSetSGR SIO.stdout [A.Reset]
    hPutStrANSI SIO.stdout (str2++"  ")
    A.hSetSGR SIO.stdout [A.Reset]
    hPutStrANSI SIO.stdout (str3++"  ")
    A.hSetSGR SIO.stdout [A.Reset]
    hPutStrANSI SIO.stdout (str4++"  ")
    A.hSetSGR SIO.stdout [A.Reset]
    putStrLn ""


-- 30 black, 31 red, 32 green, 33 yellow, 34 blue, 35 magenta, 36 cyan, 37 white

-- A chunk in an ANSI string.
-- Either an escape or a bit of string
data ANSI = AString String  -- | A chunk of normal printable chars.
          | AEscape [A.SGR] -- | An ANSI escape decoded
          | AError String   -- | A malformed, illegal, or unsupported escape
          deriving Show

parseANSI :: String -> [ANSI]
parseANSI str = reverse (goStr 0 [] "" str)
  where -- This state indicates that we are scanning a string (which may be empty
        goStr :: Int -> [ANSI] -> String -> String -> [ANSI]
        -- EOS
        goStr _  as str [] = mkStr as str
        -- Handle RESETs separate from normal escapes
        goStr !o as str ('\ESC':'[':'0':'m':cs) = goStr (o + 4) (AEscape [A.Reset] : mkStr as str) "" cs
        -- Start of a new escape sequence (non reset)
        goStr !o as str s@('\ESC':'[':cs) = goEsc (o+2) (mkStr as str) s A.Dull [] 2 cs
        -- Normal case: append a non-escape character
        goStr !o as str (c:cs) = goStr (o + 1) as (c:str) cs

        mkStr as str = if null str then as else AString (reverse str) : as

        -- goEsc indicates we've just seen a "\ESC[" and now expect to see some escapes
        -- (<int>;)*<int>m
        -- ARguments:
        --   string offset
        --   ansi chunks so far
        --   string from the start of the encoding (for error messages)
        --   list of escapes from this sequence so far
        --   the num chars read in this escape (for errors)
        --   the string we're parsing the escape from
        -- \ESC[<int>;<int>m
        goEsc :: Int -> [ANSI] -> String -> A.ColorIntensity -> [A.SGR] -> Int -> String -> [ANSI]
        goEsc !o as estr ci es !k cs
          | null cs = escErr "unterminated escape" 0 ""
       -- | head cs == 'm' = goStr (o + length ds) (AEscape (reverse es) : as)
          | null ds = escErr "expected escape code" 0 cs
          | null cs' = escErr "unterminated escape" (length ds) ""
          | head cs' == 'm' =  addEsc $ \_  es -> goStr (o + length ds + 1) (mkEsc es : as) "" (tail cs')
          | head cs' == ';' =  addEsc $ \ci es -> goEsc (o + length ds + 1) as estr ci es (k + length ds + 1) (tail cs')
          | otherwise = escErr "expected ';' or 'm'" (length ds) cs'
          where escErr m i cs
                  | null es   = goStr (o + i) (AError emsg : as) "" cs
                  | otherwise = goStr (o + i) (AError emsg : AEscape (reverse es) : as) "" cs
                  where emsg = show (o + i) ++ ". \"" ++ take (k + i) estr ++ "\"" ++ m

                mkEsc [] = AEscape [A.Reset]
                mkEsc es = AEscape (reverse es)
                (ds,cs') = span isDigit cs
                addEsc :: (A.ColorIntensity -> [A.SGR] -> [ANSI]) -> [ANSI]
                addEsc c = case reads ds :: [(Int,String)] of
                            -- A.SetConsoleIntensity A.NormalIntensity -- 0
                            [(0,"")] -> c ci (A.Reset : es)
                            -- A.SetConsoleIntensity A.BoldIntensity   -- 1
                            [(1,"")] -> c A.Vivid es
                            -- A.SetConsoleIntensity A.FaintIntensity  -- 2
                            [(2,"")] -> c A.Dull es

                            [(30,"")] -> c ci (A.SetColor A.Foreground ci A.Black : es)
                            [(31,"")] -> c ci (A.SetColor A.Foreground ci A.Red : es)
                            [(32,"")] -> c ci (A.SetColor A.Foreground ci A.Green : es)
                            [(33,"")] -> c ci (A.SetColor A.Foreground ci A.Yellow : es)
                            [(34,"")] -> c ci (A.SetColor A.Foreground ci A.Blue : es)
                            [(35,"")] -> c ci (A.SetColor A.Foreground ci A.Magenta : es)
                            [(36,"")] -> c ci (A.SetColor A.Foreground ci A.Cyan : es)
                            [(37,"")] -> c ci (A.SetColor A.Foreground ci A.White : es)

                            [(40,"")] -> c ci (A.SetColor A.Background ci A.Black : es)
                            [(41,"")] -> c ci (A.SetColor A.Background ci A.Red : es)
                            [(42,"")] -> c ci (A.SetColor A.Background ci A.Green : es)
                            [(43,"")] -> c ci (A.SetColor A.Background ci A.Yellow : es)
                            [(44,"")] -> c ci (A.SetColor A.Background ci A.Blue : es)
                            [(45,"")] -> c ci (A.SetColor A.Background ci A.Magenta : es)
                            [(46,"")] -> c ci (A.SetColor A.Background ci A.Cyan : es)
                            [(47,"")] -> c ci (A.SetColor A.Background ci A.White : es)

                            _ -> escErr ("unsupported escape " ++ show ds) 0 (tail cs')

