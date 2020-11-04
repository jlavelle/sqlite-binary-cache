{-# Language AllowAmbiguousTypes #-}
{-# Language CPP                 #-}

module SQLite.BinaryCache.FieldType where

import           Control.DeepSeq                  (NFData)
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Lazy             as LBS
import           Data.Data                        (Data)
import           Data.Int                         (Int16, Int32, Int64, Int8)
import           Data.Text                        (Text)
import qualified Data.Text.Lazy                   as LT
import           Data.Time                        (Day, UTCTime)
import           Data.Word                        (Word16, Word32, Word64, Word8)
import           Database.SQLite.Simple.FromField (FromField)
import           Database.SQLite.Simple.ToField   (ToField)
import           GHC.Generics                     (Generic)

data FieldType
  = FieldInteger
  | FieldBlob
  | FieldReal
  | FieldText
  deriving (Eq, Ord, Enum, Show, Generic, Data, NFData)

render :: FieldType -> Text
render = \case
  FieldInteger -> "integer"
  FieldReal    -> "real"
  FieldText    -> "text"
  FieldBlob    -> "blob"

-- | Gives the SQLite column type for a type @a@
class (FromField a, ToField a) => ToFieldType a where
  toFieldType :: FieldType

instance ToFieldType Bool where
  toFieldType = FieldInteger

instance ToFieldType Double where
  toFieldType = FieldReal

instance ToFieldType Float where
  toFieldType = FieldReal

instance ToFieldType Int where
  toFieldType = FieldInteger

instance ToFieldType Int8 where
  toFieldType = FieldInteger

instance ToFieldType Int16 where
  toFieldType = FieldInteger

instance ToFieldType Int32 where
  toFieldType = FieldInteger

instance ToFieldType Int64 where
  toFieldType = FieldInteger

instance ToFieldType Integer where
  toFieldType = FieldInteger

instance ToFieldType Word where
  toFieldType = FieldInteger

instance ToFieldType Word8 where
  toFieldType = FieldInteger

instance ToFieldType Word16 where
  toFieldType = FieldInteger

instance ToFieldType Word32 where
  toFieldType = FieldInteger

instance ToFieldType Word64 where
  toFieldType = FieldInteger

instance ToFieldType ByteString where
  toFieldType = FieldBlob

instance ToFieldType LBS.ByteString where
  toFieldType = FieldBlob

instance ToFieldType Text where
  toFieldType = FieldText

instance ToFieldType LT.Text where
  toFieldType = FieldText

instance ToFieldType UTCTime where
  toFieldType = FieldText

instance ToFieldType Day where
  toFieldType = FieldText

instance ToFieldType String where
  toFieldType = FieldText
