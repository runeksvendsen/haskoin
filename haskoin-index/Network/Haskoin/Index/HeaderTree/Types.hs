module Network.Haskoin.Index.HeaderTree.Types where

<<<<<<< 311bf49f4a22595ab7de7426c5dfd40a275e131e
import           Data.Serialize        (decode, encode)
import           Data.String           (fromString)
=======
import           Data.Serialize        (encode)
>>>>>>> Move from binary to cereal
import           Data.Word             (Word32, Word64)
import           Database.Persist      (PersistField (..), PersistValue (..),
                                        SqlType (..))
import           Database.Persist.Sql  (PersistFieldSql (..))
import           Network.Haskoin.Block

type BlockHeight = Word32
type ShortHash = Word64
type Timestamp = Word32
type Work = Double

newtype NodeHeader = NodeHeader { getNodeHeader :: BlockHeader }
    deriving (Show, Eq)

{- SQL database backend for HeaderTree -}

instance PersistField NodeHeader where
    toPersistValue = PersistByteString . encode . getNodeHeader
    fromPersistValue (PersistByteString bs) =
        case decode bs of
            Right x -> Right (NodeHeader x)
            Left  e -> Left  (fromString e)
    fromPersistValue _ = Left "Invalid persistent block header"

instance PersistFieldSql NodeHeader where
    sqlType _ = SqlBlob

