module Cardano.UTxOCSMT.HTTP.Base16
    ( encodeBase16Text
    , decodeBase16Text
    , unsafeDecodeBase16Text
    ) where

import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

-- | Encode a 'ByteString' as Base16 'T.Text'.
encodeBase16Text :: ByteString -> T.Text
encodeBase16Text = TE.decodeUtf8 . Base16.encode

-- | Decode a Base16 'T.Text' to a 'ByteString'.
decodeBase16Text :: T.Text -> Either String ByteString
decodeBase16Text txt =
    case Base16.decode (TE.encodeUtf8 txt) of
        Right bs -> Right bs
        Left _ -> Left "invalid base16"

{- | Partial variant of 'decodeBase16Text'. Only safe when the
caller has already validated the input (e.g. a constant or
previously-encoded bytes).
-}
unsafeDecodeBase16Text :: T.Text -> ByteString
unsafeDecodeBase16Text txt =
    case decodeBase16Text txt of
        Left err ->
            error $ "unsafeDecodeBase16Text: " ++ err
        Right bs -> bs
