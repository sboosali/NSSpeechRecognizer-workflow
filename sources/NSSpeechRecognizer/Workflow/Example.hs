{-# LANGUAGE RecordWildCards, NamedWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-} -- to test inference
module NSSpeechRecognizer.Workflow.Example where
-- import NSSpeechRecognizer.Workflow
import NSSpeechRecognizer
import Workflow.OSX (runWorkflowT)
import Workflow.Core (press,insert)

import qualified Control.Monad.Catch ()
  -- instances only (in particular, @instance MonadThrow IO@)

{-|
@
stack build && stack exec -- example-NSSpeechRecognizer-workflow
@
-}
main :: IO ()
main = do
  let Recognizer{..} = (aVoiceMapRecognizer mapping)
  let recognizer = Recognizer{rHandler, rState = rState{rExclusivity = Exclusive}}
  print "(Listening...)"
  foreverRecognizer recognizer

{-old

lol haskell records
foreverRecognizer (aVoiceMapRecognizer mapping){rState = rState{rExclusivity = Exclusive}}

-}

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
mapping = fmap (fmap runWorkflowT) -- runWorkflowT :: WorkflowT IO :~> IO
  [ "scroll     "   -: press "<spc>"      -- Extra white spaces okay
  , "scroll up"     -: press "S-<spc>"
  , "close tab"     -: press "H-w"        -- "H" i.e. Hyper is Command on OSX
  , "my email"      -: insert "samboosalis@gmail.com"  -- alias,
  -- , "Invalid"       -: press "S-abc"      -- Syntax errors are thrown when 'runWorkflowT'
  , "quit listening"-: press "C-c C-c"    -- double UserInterrupt
    -- (only works when terminal is for foreground).
    -- The double UserInterrupt seems to be necessary when NSRunLoop is running within the GHC runtime.

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
  -- ("H" is Control elsewhere)
  -- if you see:
  -- example-NSSpeechRecognizer-workflow: syntax error: {{Workflow.Keys.press "..."}}
  -- your call to 'press' has a syntax error
  -- blanks don't have to be filtered. convenient to be able to quickly add a new command

nothing :: Monad m => m ()
nothing = return ()

{-TODO strict map?

Once the @Map@ is constructed, the @Workflow@s have already been "compiled"
into @IO@, and are thereafter cached implicitly.

Either way, the indirection-overhead (of using workflow over IO directly),
for such simple monadic computations, is milliseconds at most (No benchmarks yet,
just my own "Profiling by printing the current system time before-and-after").

-}
