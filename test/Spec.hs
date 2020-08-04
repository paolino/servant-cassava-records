{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

import Generics.SOP
import Generics.SOP.TH
-- (MimeUnrender(mimeUnrender))

import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Network.Wai
import Network.Wai.Handler.Warp
import Protolude
import Servant.API
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

ciao_T12_43_72_1234 :: EncodingWith TwoRecord [QueryL TwoRecord]
ciao_T12_43_72_1234 = EncodingWith [T (I 42) $ L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E]


type TestApi l =
  Get '[CSV l 'Querying] (EncodingWith l [QueryL l])

testApi :: Proxy (TestApi l)
testApi = Proxy

server :: EncodingWith l [QueryL l] -> Server (TestApi l)
server = return

clientL :: GetLRT l => ClientM (EncodingWith l [QueryL l])
clientL = client testApi

application :: GetLRT l => EncodingWith l [QueryL l] -> Application
application = serve testApi . server

testServer :: GetLRT l => EncodingWith l [QueryL l] -> IO (EncodingWith l [QueryL l])
testServer input = do
  resultV <- newEmptyMVar
  let test = do
        manager' <- newManager defaultManagerSettings
        void $
          forkIO $ do
            res <- runClientM clientL $ mkClientEnv manager' $ BaseUrl Http "localhost" 3000 ""
            case res of
              Left err -> panic $ show err
              Right result -> putMVar resultV result
      settings = setBeforeMainLoop test (setPort 3000 defaultSettings)
  stop <- forkIO $ runSettings settings $ application input
  output <- takeMVar resultV
  killThread stop
  threadDelay 100_000
  pure output

main :: IO ()
main = hspec $ do
  describe "one records" $ do
    it "renders putting one value" $
      mimeRender (oneRecord @'Putting) ciao43 == "name,age\nciao,43\n"
    it "parses querying rows" $
      mimeUnrender (oneRecord @'Querying) "index,name,age\n12,ciao,43\n" == Right ciaoT12_43
    it "renders updating one row" $
      mimeRender (oneRecord @'Updating) ciaoT12_43 == "index,name,age\n12,ciao,43\n"
    it "renders deleting one row" $
      mimeRender (oneRecord @'Deleting) (deleteIndex12 @OneRecord) == "index\n12\n"
  describe "two records" $ do
    it "renders putting one value" $
      mimeRender (twoRecord @'Putting) ciao_43_72_1234 == "name,age,feet number,secret PIN\nciao,43,72,1234\n"
    it "parses querying rows" $
      mimeUnrender (twoRecord @'Querying) "index2,name,age,feet number,secret PIN\n12,ciao,43,72,1234\n" == Right (fmap (fmap $ T (I 12)) ciao_43_72_1234)
    it "renders updating one row" $
      mimeRender (twoRecord @'Updating) (fmap (T (I 12)) <$> ciao_43_72_1234) == "index2,name,age,feet number,secret PIN\n12,ciao,43,72,1234\n"
    it "renders deleting one row" $
      mimeRender (twoRecord @'Deleting) (deleteIndex12 @TwoRecord) == "index2\n12\n"
  describe "Querying API" $ do
    it "serve a Oneecord" $ do
      r <- testServer ciaoT12_43
      shouldBe r ciaoT12_43
    it "serve a TwoRecord" $ do
      r <- testServer ciao_T12_43_72_1234
      shouldBe r ciao_T12_43_72_1234
