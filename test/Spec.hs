{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- (MimeUnrender(mimeUnrender))

import Control.Concurrent.STM (TVar, modifyTVar, newTVarIO, readTVarIO)
import Control.Lens hiding (Index)
import qualified Data.Map as M
import Generics.SOP
import Generics.SOP.TH
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Protolude
import Servant.API
import Servant.API.ContentTypes (AllCTRender)
import Servant.CSV.Records
import Servant.Client
import Servant.Server
import Test.Hspec

--------------------- OneRecord  ---------------------------

data OneRecord

instance GetLRT OneRecord where
  type Index OneRecord = Int
  type Products OneRecord = '[(Text, Int)]
  getLRT _ = T (defRT "index") $ L (RC $ defRT "name" :* defRT "age" :* Nil) E

oneRecord :: Proxy (CSV OneRecord x)
oneRecord = Proxy

ciao43 :: EncodingWith OneRecord [LP I Int (Products OneRecord)]
ciao43 = EncodingWith [L (I ("ciao", 43)) E]

ciaoT12_43 :: EncodingWith OneRecord [LU I Int (Products OneRecord)]
ciaoT12_43 = fmap (T (I 12)) <$> ciao43

deleteIndex12 :: EncodingWith x [LD I Int]
deleteIndex12 = EncodingWith [T (I 12) E]

--------------------- TwoRecord ---------------------------

data TwoRecord

data Foo = Foo Int Text deriving (Show, Eq)

deriveGeneric ''Foo

instance GetLRT TwoRecord where
  type Index TwoRecord = Int
  type Products TwoRecord = '[(Text, Int), Foo]
  getLRT _ =
    T (defRT "index2") $
      L (RC $ defRT "name" :* defRT "age" :* Nil) $
        L (RC $ defRT "feet number" :* defRT "secret PIN" :* Nil) E

twoRecord :: Proxy (CSV TwoRecord x)
twoRecord = Proxy

ciao_43_72_1234 :: EncodingWith TwoRecord [LP I Int (Products TwoRecord)]
ciao_43_72_1234 = EncodingWith [L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E]

ciao_T42_43_72_1234 :: EncodingWith TwoRecord [QueryL TwoRecord]
ciao_T42_43_72_1234 = EncodingWith [T (I 42) $ L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E]

type TestApi l =
  Get '[CSV l 'Querying] (EncodingWith l [QueryL l])
    :<|> ReqBody '[CSV l 'Putting] (EncodingWith l [PutL l]) :> Put '[PlainText] NoContent
    :<|> ReqBody '[CSV l 'Updating] (EncodingWith l [UpdateL l]) :> Patch '[PlainText] NoContent
    :<|> ReqBody '[CSV l 'Deleting] (EncodingWith l [DeleteL l]) :> Delete '[PlainText] NoContent

testApi :: Proxy (TestApi l)
testApi = Proxy

data RamState l = RamState
  { _ramState_state :: Map (Index l) (PutL l)
  , _ramState_nextID :: Index l
  }

deriving instance (Show (Index l)) => Show (RamState l)


insertRecords :: (Ord (Index l), Enum (Index l)) => [PutL l] -> RamState l -> RamState l
insertRecords xs s = foldl' insertRecord s xs

insertRecord :: (Ord (Index l), Enum (Index l)) => RamState l -> PutL l -> RamState l
insertRecord (RamState state' index') x = RamState (M.insert index' x state') (succ index')

updateRecords :: (Ord (Index l), Enum (Index l)) => [UpdateL l] -> RamState l -> RamState l
updateRecords xs s = foldl' updateRecord s xs 

updateRecord :: Ord (Index l) => RamState l -> UpdateL l -> RamState l
updateRecord (RamState state' index') (T (I i) x) = RamState (M.insert i x state') index'

listRecords :: Eq (Index l) => RamState l -> [QueryL l]
listRecords (RamState state' _) = uncurry (T . I) <$> M.assocs state'

bootState :: (Ord (Index l), Enum (Index l)) => EncodingWith l [QueryL l] -> RamState l
bootState (EncodingWith xs) = RamState
  do M.fromList $ (\(T (I i) l) -> (i, l)) <$> xs
  do succ $ maximum $ unI . getT <$> xs

server :: (Enum (Index l), Ord (Index l), Show (Index l)) => TVar (RamState l) -> Server (TestApi l)
server stateT = getA :<|> putS :<|> updateS :<|> deleteS
  where
    getA = fmap (EncodingWith . listRecords) $ liftIO $ readTVarIO stateT
    putS (EncodingWith xs) = do
      liftIO $ atomically $ modifyTVar stateT (insertRecords xs)
      pure NoContent
    updateS (EncodingWith xs) = do 
      liftIO $ atomically $ modifyTVar stateT (updateRecords xs)
      pure NoContent
    deleteS (EncodingWith xs) = do 
      liftIO $ atomically $ modifyTVar stateT (deleteRecords xs)
      pure NoContent

deleteRecords :: Ord (Index l) => [LD I (Index l)] -> RamState l -> RamState l
deleteRecords xs s = foldl' deleteRecord s xs 

deleteRecord :: Ord (Index l) => RamState l -> LD I (Index l) -> RamState l
deleteRecord (RamState state' index') (T (I i) E)= RamState (M.delete i state') index'


queryL :: GetLRT l => ClientM (EncodingWith l [QueryL l])

putL :: GetLRT l => EncodingWith l [PutL l] -> ClientM NoContent

updateL :: GetLRT l => EncodingWith l [UpdateL l] -> ClientM NoContent

deleteL :: GetLRT l => EncodingWith l [LD I (Index l)] -> ClientM NoContent

queryL :<|> putL :<|> updateL :<|> deleteL= client testApi

application
  :: (Ord (Index l), Enum (Index l), GetLRT l, Show (Index l))
  => TVar (RamState l)
  -> Application
application stateT req resp = do
  serve testApi (server stateT) req resp

testServer
  :: (Ord (Index l), Enum (Index l), GetLRT l, Show (Index l))
  => EncodingWith l [QueryL l]
  -> ClientM (EncodingWith l [QueryL l])
  -> IO (EncodingWith l [QueryL l])
testServer input testA = do
  resultV <- newEmptyMVar
  stateT <- liftIO $ newTVarIO $ bootState input
  let test = do
        manager' <- newManager defaultManagerSettings
        void $
          forkIO $ do
            res <- runClientM testA $ mkClientEnv manager' $ BaseUrl Http "localhost" 3000 ""
            case res of
              Left err -> panic $ show err
              Right result -> putMVar resultV result
      settings = setBeforeMainLoop test (setPort 3000 defaultSettings)
  stop <- forkIO $ runSettings settings $ application stateT
  output <- takeMVar resultV
  killThread stop
  threadDelay 100_000
  pure output

main :: IO ()
main = hspec $ do
  describe "one records" $ do
    it "renders putting one value" $ shouldBe
      do mimeRender (oneRecord @'Putting) ciao43
      do "name,age\nciao,43\n"
    it "parses querying rows" $ shouldBe
      do mimeUnrender (oneRecord @'Querying) "index,name,age\n12,ciao,43\n"
      do Right ciaoT12_43
    it "renders updating one row" $ shouldBe
      do mimeRender (oneRecord @'Updating) ciaoT12_43
      do "index,name,age\n12,ciao,43\n"
    it "renders deleting one row" $ shouldBe
      do mimeRender (oneRecord @'Deleting) (deleteIndex12 @OneRecord)
      do "index\n12\n"
  describe "two records" $ do
    it "renders putting one value" $ shouldBe
      do mimeRender (twoRecord @'Putting) ciao_43_72_1234
      do "name,age,feet number,secret PIN\nciao,43,72,1234\n"
    it "parses querying rows" $
      shouldBe
        do mimeUnrender (twoRecord @'Querying) "index2,name,age,feet number,secret PIN\n12,ciao,43,72,1234\n"
        do Right (fmap (fmap $ T (I 12)) ciao_43_72_1234)
    it "renders updating one row" $
      shouldBe
        do mimeRender (twoRecord @'Updating) ciao_T42_43_72_1234
        do "index2,name,age,feet number,secret PIN\n42,ciao,43,72,1234\n"
    it "renders deleting one row" $
      mimeRender (twoRecord @'Deleting) (deleteIndex12 @TwoRecord) == "index2\n12\n"
  describe "Querying API" $ do
    it "serve a Oneecord" $ do
      r <- testServer ciaoT12_43 queryL
      shouldBe r ciaoT12_43
    it "serve a TwoRecord" $ do
      r <- testServer ciao_T42_43_72_1234 queryL
      shouldBe r ciao_T42_43_72_1234
  describe "Querying API" $ do
    it "serve a OneRecord" $ do
      r <- testServer ciaoT12_43 queryL
      shouldBe r ciaoT12_43
    it "serve a TwoRecord" $ do
      r <- testServer ciao_T42_43_72_1234 queryL
      shouldBe r ciao_T42_43_72_1234
  describe "Putting API" $ do
    it "put a OneRecord and get it back" $ do
      r <- testServer ciaoT12_43 $ putL ciao43 >> queryL
      shouldBe r $ EncodingWith
        do
          [ T (I 12) $ L (I ("ciao", 43)) E
            , T (I 13) $ L (I ("ciao", 43)) E
            ]
    it "put a TwoRecord and get it back" $ do
      r <- testServer ciao_T42_43_72_1234 $ putL ciao_43_72_1234 >> queryL
      shouldBe r $ EncodingWith
        do
          [ T (I 42) $ L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E
            , T (I 43) $ L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E
            ]
  describe "Updating API" $ do
    it "update a OneRecord and get it back" $ do
      r <- testServer ciaoT12_43 $ 
        updateL (EncodingWith @ OneRecord [T (I 12) $ L (I ("ciao", 47)) E]) >> queryL
      shouldBe r $ EncodingWith
        do [ T (I 12) $ L (I ("ciao", 47)) E ]
  describe "Delete API" $ do
    it "delete a OneRecord and get back null" $ do
      r <- testServer ciaoT12_43 $ 
        deleteL (EncodingWith @ OneRecord [T (I 12) E]) >> queryL
      shouldBe r $ EncodingWith
        do []
