{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Emanote.Model.BibTeX where

import Control.Monad.Logger (MonadLogger)
import Data.Aeson qualified as Aeson
import Data.IxSet.Typed (Indexable (..), IxSet, ixFun, ixList)
import Data.IxSet.Typed qualified as Ix
import Emanote.Model.Note (Note)
import Emanote.Model.Note qualified as N
import Emanote.Model.SData qualified as SD
import Emanote.Prelude (logD)
import Emanote.Route qualified as R
import Emanote.Source.Loc
import Optics.Operators ((.~), (^.))
import Optics.TH (makeLenses)
import Relude
import Text.Pandoc (readBibLaTeX)
import Text.Pandoc.Builder qualified as B
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Definition
import Text.Pandoc.Options (def)

data BibTeX = BibTeX
  { _bibtexRoute :: R.R R.BibTeX,
    _bibtexSource :: (Loc, FilePath),
    _bibtexDoc :: Pandoc
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (Aeson.ToJSON)

parseBibTeX ::
  forall m.
  (MonadIO m, MonadLogger m) =>
  R.R R.BibTeX ->
  (Loc, FilePath) ->
  Text ->
  m (Maybe BibTeX)
parseBibTeX r s txt =
  case runPure $ readBibLaTeX def txt of
    Left e -> logD (show e) >> return Nothing
    Right doc -> return . Just $ BibTeX r s doc

updateBib :: Maybe FilePath -> Aeson.Value -> Aeson.Value
updateBib path = SD.modifyAeson ("bibliography" :| []) (const (fmap (Aeson.String . toText) path))

-- instance Ord Task where
--   (<=) = (<=) `on` (_taskRoute &&& _taskNum)

-- type BibTeXIxs =
--   '[ -- Route to the note containing this task
--      R.LMLRoute
--    ]

-- type IxBibTeX = IxSet BibTeXIxs BibTeX

-- instance Indexable BibTeXIxs BibTeX where
--   indices =
--     ixList
--       (ixFun $ one . _bibtexRoute)

-- noteTasks :: Note -> IxTask
-- noteTasks note =
--   let taskListItems = TaskList.queryTasks $ note ^. N.noteDoc
--    in Ix.fromList
--         $ zip [1 ..] taskListItems
--         <&> \(idx, (checked, doc)) ->
--           Task (note ^. N.noteRoute) idx doc checked

makeLenses ''BibTeX
