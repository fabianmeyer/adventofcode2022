module PrettyPrint where

import qualified Data.Text as T
import Prettyprinter hiding (group, space)

printKeyValuePairs kvs = show . vcat $ (\(key, value) -> fillBreak ((maximum $ T.length . fst <$> kvs) + 3) (pretty key <> ":") <+> pretty value) <$> kvs