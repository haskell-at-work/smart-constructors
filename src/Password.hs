module Password
  ( Password
  , ValidationError(..)
  , validatePassword
  ) where

import           Data.Text (Text)
import qualified Data.Text as Text

newtype Password =
  Password Text

data ValidationError
  = TooShort
  | TooLong
  deriving (Eq, Show)

validatePassword :: Text -> Either ValidationError Password
validatePassword t
  | l < 10 = Left TooShort
  | l > 50 = Left TooLong
  | otherwise = Right (Password t)
  where
    l = Text.length t
