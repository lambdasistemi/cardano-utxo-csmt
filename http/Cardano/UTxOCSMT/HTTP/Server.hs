module Cardano.UTxOCSMT.HTTP.Server
    ( runAPIServer
    , runDocsServer
    , apiApp
    , docsApp
    )
where

import Cardano.UTxOCSMT.Application.Metrics
    ( Metrics
    , renderPrometheus
    )
import Cardano.UTxOCSMT.HTTP.API
    ( API
    , InclusionProofResponse
    , MerkleRootEntry
    , ReadyResponse (..)
    , UTxOByAddressEntry
    , api
    , docs
    )
import Cardano.UTxOCSMT.HTTP.Swagger (swaggerServer)
import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO (..))
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word16)
import Network.Socket (PortNumber)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
    ( Handler
    , Server
    , err400
    , err404
    , err503
    , errBody
    , serve
    , throwError
    , (:<|>) (..)
    )

-- | Server implementation for the API
apiServer
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> (Text -> IO (Either String [UTxOByAddressEntry]))
    -> IO ReadyResponse
    -> Server API
apiServer getMetrics getMerkleRoots getProof getByAddress getReady =
    prometheusHandler
        :<|> metricsHandler
        :<|> merkleRootsHandler
        :<|> proofHandler
        :<|> byAddressHandler
        :<|> readyHandler
  where
    metricsHandler = do
        r <- liftIO getMetrics
        maybe (throwError err404) return r

    prometheusHandler = do
        r <- liftIO getMetrics
        maybe (throwError err404) (return . renderPrometheus) r

    -- \| Check sync status and return 503 if not ready
    requireSynced :: Handler a -> Handler a
    requireSynced action = do
        ReadyResponse{ready} <- liftIO getReady
        unless ready $ throwError err503
        action

    merkleRootsHandler = requireSynced $ liftIO getMerkleRoots

    proofHandler txId txIx = requireSynced $ do
        r <- liftIO $ getProof txId txIx
        maybe
            (throwError err404)
            (pure :: InclusionProofResponse -> Handler InclusionProofResponse)
            r

    byAddressHandler address = requireSynced $ do
        r <- liftIO $ getByAddress address
        case r of
            Left err ->
                throwError
                    err400{errBody = BL.fromStrict $ TE.encodeUtf8 $ T.pack err}
            Right entries -> pure entries

    readyHandler = liftIO getReady

-- | WAI Application for the API
apiApp
    :: IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> (Text -> IO (Either String [UTxOByAddressEntry]))
    -> IO ReadyResponse
    -> Application
apiApp getMetrics getMerkleRoots getProof getByAddress getReady =
    simpleCors
        $ serve api
        $ apiServer getMetrics getMerkleRoots getProof getByAddress getReady

{- | Run the API server on the specified port
Takes a port number, an IO action that provides the current Metrics,
an IO action that retrieves all merkle roots, a proof query function,
a by-address query function,
and an IO action that returns the sync readiness status
-}
runAPIServer
    :: PortNumber
    -> IO (Maybe Metrics)
    -> IO [MerkleRootEntry]
    -> (Text -> Word16 -> IO (Maybe InclusionProofResponse))
    -> (Text -> IO (Either String [UTxOByAddressEntry]))
    -> IO ReadyResponse
    -> IO ()
runAPIServer port getMetrics getMerkleRoots getProof getByAddress getReady =
    run (fromIntegral port)
        $ apiApp getMetrics getMerkleRoots getProof getByAddress getReady

-- | WAI Application for the documentation
docsApp :: Maybe PortNumber -> Application
docsApp mApiPort = serve docs (swaggerServer mApiPort)

{- | Run the documentation server on the specified port
Takes the docs port and optionally the API port (for Swagger to point to)
-}
runDocsServer :: PortNumber -> Maybe PortNumber -> IO ()
runDocsServer port mApiPort = run (fromIntegral port) (docsApp mApiPort)
