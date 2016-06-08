{-# LANGUAGE NoMonomorphismRestriction, RecordWildCards, NamedWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
-- | (see source)
module NSSpeechRecognizer.Workflow.Example where
-- import NSSpeechRecognizer.Workflow
import NSSpeechRecognizer
import Workflow.OSX (runWorkflowT)
import Workflow.Core (press,insert,delay,getClipboard,setClipboard,openApplication,openURL,click,MouseButton(LeftButton))

import qualified Control.Monad.Catch ()
  -- instances only (in particular, @instance MonadThrow IO@)
import Control.Arrow ((>>>))
import Data.List (intercalate)
import Data.Char (toUpper)
import Control.Monad.IO.Class (liftIO)

--------------------------------------------------------------------------------

{-|
@
stack build && stack exec -- example-NSSpeechRecognizer-workflow
@
-}
main :: IO ()
main = do
  putStrLn "NSSpeechRecognizer.Workflow.Example..."
  let Recognizer{..} = (aVoiceMapRecognizer commands)
  let recognizer = Recognizer{rHandler, rState = rState{rExclusivity = Exclusive}}
  print "(Listening...)"
  foreverRecognizer recognizer

--------------------------------------------------------------------------------

copy = do
  press "H-c"
  pause
  getClipboard
 --TODO command key is stuck, why? oh, after the pressing, the key doesn't go back up
 -- Disabling sticky keys doesn't help

cut = do
  press "H-x"
  pause
  getClipboard

doubleClick = do
  click [] 2 LeftButton

-- | for one "frame"
pause = delay 30

insertByClipboard s = do
  setClipboard s
  pause
  press "H-v"

camelCase = words >>> go >>> intercalate ""
 where
 go = \case
  [] -> []
  (w:ws) -> w : fmap capitalize ws
 capitalize = \case
  [] -> []
  (c:cs) -> toUpper c : cs

-- | @nothing = return ()
nothing :: Monad m => m ()
nothing = return ()

{-TODO strict map?

Once the @Map@ is constructed, the @Workflow@s have already been "compiled"
into @IO@, and are thereafter cached implicitly.

Either way, the indirection-overhead (of using workflow over IO directly),
for such simple monadic computations, is milliseconds at most (No benchmarks yet,
just my own "Profiling by printing the current system time before-and-after").

-}

{-old

lol haskell records
foreverRecognizer (aVoiceMapRecognizer mapping){rState = rState{rExclusivity = Exclusive}}

-}

--------------------------------------------------------------------------------

{-| (See source).

These keyboard shortcuts are faster than those defined within the
@Accessibility > Dictation > Dictation Commands... > Press Keyboard Shortcut...@
menu.

With Lazy Maps, the first time a @key@ is recognized, its @value@ @Workflow@ is
"parsed" (via 'press') and "compiled" (via 'runWorkflowT')
into @IO@. With GHC's laziness, the actions are thereafter cached implicitly.
A(n 'evaluate'd) Strict Map would take a little longer to construct,
but its values will have already all been evaluated before the
speech recognition engine has started listening.

-}
commands = fmap (fmap runWorkflowT)
  -- runWorkflowT :: WorkflowT IO :~> IO

  -- ("H" is Control elsewhere)

  -- if you see:
  -- example-NSSpeechRecognizer-workflow: syntax error: {{Workflow.Keys.press "..."}}
  -- your call to 'press' has a syntax error

  -- blanks don't have to be filtered. convenient to be able to quickly add a new command

  -- "quit listening":
  -- The double UserInterrupt seems to be necessary when NSRunLoop is running within the GHC runtime.

  [ "scroll     "   -: press "<spc>"                   -- Extra whitespace okay
  , "scroll up"     -: press "S-<spc>"
  , "close tab"     -: press "H-w"                     -- "H" i.e. Hyper is Command on OSX
  , "my email"      -: insert "samboosalis@gmail.com"  -- an Abbreviation

  , "quit listening"-: do
    openApplication "Terminal"
    press "C-c C-c"                                    -- double UserInterrupt

  , "camel that"-: do                                  -- camel case the currently selected text
    x <- cut
    let y = camelCase x
    liftIO $ print y
    insert y

  , "google that"-: do                                 -- Google the currently selected text
    s <- copy
    pause
    openURL $ "http://www.google.com/" ++ s  --TODO url-encode

  , "google word"-: do                                 -- Google the word under the cursor
    doubleClick
    pause
    s <- copy
    pause
    openURL $ "http://www.google.com/" ++ s   --TODO url-encode

  -- , "Invalid"       -: press "S-abc"                -- Syntax errors are thrown under 'runWorkflowT'

  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  , "" -: nothing
  ]
