-------------------------------------------------------------------------------
-- |
-- Module      :  SendFile
-- Copyright   :  (c) Patrick Brisbin 2010 
-- License     :  as-is
--
-- Maintainer  :  pbrisbin@gmail.com
-- Stability   :  unstable
-- Portability :  unportable
--
-- Using a typical XMonad Prompt, enter an address, subject, and file
-- path; an email will be sent with the file as an attachment.
--
-- This module is designed to be used specifically with mutt. This means
-- that mutt must be installed to actually execute the send /and/ a mutt
-- alias file can be read to create the completion list for addresses.
--
-------------------------------------------------------------------------------

module SendFile (
    -- * Usage
    -- $usage
    sendFilePrompt
    ) where

import XMonad.Core         (X(..), io)
import XMonad.Util.Run     (runProcessWithInput, runProcessWithInputAndWait)
import XMonad.Prompt       (XPConfig(..))
import XMonad.Prompt.Input (inputPrompt, inputPromptWithCompl, (?+))
import System.Directory    (getHomeDirectory)
import System.FilePath     ((</>))

-- $usage
--
-- First, copy this source code into @~\/.xmonad\/lib\/SendFile.hs@,
-- then import it into @~\/.xmonad\/xmonad.hs@ like so:
--
-- > import SendFile
-- > import qualified XMonad.Prompt as P
--
-- Finally, add an appropriate keybinding, for example:
--
-- >  , ((modm .|. controlMask, xK_f), sendFilePrompt P.defaultXPConfig "~/.mutt/alias")
--

-- | The main function, reads addresses out of a mutt alias file. If you
--   don't use a mutt alias file, you can actually provide any file that
--   lists bracketted email addresses one per line.
sendFilePrompt :: XPConfig -> FilePath -> X ()
sendFilePrompt conf aliasFile =
    inputPromptWithCompl conf "To"      (aliasCompletion aliasFile) ?+ \to   ->
    inputPrompt          conf "Subject"                             ?+ \subj ->
    inputPromptWithCompl conf "File"    fileListCompletion          ?+ \file ->
    io $ runProcessWithInputAndWait "mutt" ["-s", subj, "-a", file, "--", to] "Please see attached.\n" 2000
        >> return ()

-- | Provides a completion function from your alias file.
aliasCompletion :: FilePath -> String -> IO [String]
aliasCompletion f [] = return []
aliasCompletion f s  = do
    contents <- readFile' f
    let l = map parseAddress $ lines contents

    return $ filter (\x -> take (length s) x == s) l

    where
        -- translate ~ if needed
        readFile' ('~':'/':rest) = getHomeDirectory >>= \d -> readFile $ d </> rest
        readFile' f              = readFile f

        parseAddress = takeWhile (/= '>') . drop 1 . dropWhile (/= '<')

-- | Uses bash completion to provide general file completion.
fileListCompletion :: String -> IO [String]
fileListCompletion [] = return []
fileListCompletion s = (filter notboring . lines) `fmap`
    runProcessWithInput "/bin/bash" [] ("compgen -A file " ++ s ++ "\n")

    where
        -- skip dumb directories
        notboring = not . flip elem [".", ".."]

