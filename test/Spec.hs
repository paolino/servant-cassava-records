{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

import Generics.SOP
import Generics.SOP.TH
import Protolude
import Servant.API -- (MimeUnrender(mimeUnrender))
import Servant.CSV.Records
import Test.Hspec

data OneRecord

instance GetLRT OneRecord where
  type Index OneRecord = Int
  type Products OneRecord = '[(Text, Int)]
  getLRT _ = T (defRT "index") $ L (RC $ defRT "name" :* defRT "age" :* Nil) E

oneRecord :: Proxy (CSV OneRecord x)
oneRecord = Proxy

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


ciao43 :: EncodingWith OneRecord [LP I Int (Products OneRecord)]
ciao43 = EncodingWith [L (I ("ciao", 43)) E]

deleteIndex12 :: EncodingWith x [LD I Int]
deleteIndex12 = EncodingWith [T (I 12) E]


ciao_43_72_1234 :: EncodingWith TwoRecord [LP I Int (Products TwoRecord)]
ciao_43_72_1234 = EncodingWith [L (I ("ciao", 43)) $ L (I $ Foo 72 "1234") E]

main :: IO ()
main = hspec $ do
  describe "one records" $ do
    it "renders putting one value" $
      mimeRender (oneRecord @'Putting) ciao43 == "name,age\nciao,43\n"
    it "parses querying rows" $
      mimeUnrender (oneRecord @'Querying) "index,name,age\n12,ciao,43\n" == Right (fmap (fmap $ T (I 12)) ciao43)
    it "renders updating one row" $
      mimeRender (oneRecord @'Updating) (fmap (T (I 12)) <$> ciao43) == "index,name,age\n12,ciao,43\n"
    it "renders deleting one row" $
      mimeRender (oneRecord @'Deleting) (deleteIndex12 @ OneRecord) == "index\n12\n"
  describe "two records" $ do
    it "renders putting one value" $
       mimeRender (twoRecord @'Putting) ciao_43_72_1234 == "name,age,feet number,secret PIN\nciao,43,72,1234\n"
    it "parses querying rows" $
      mimeUnrender (twoRecord @'Querying) "index2,name,age,feet number,secret PIN\n12,ciao,43,72,1234\n" == Right (fmap (fmap $ T (I 12)) ciao_43_72_1234)
    it "renders updating one row" $
      mimeRender (twoRecord @'Updating) (fmap (T (I 12)) <$> ciao_43_72_1234) == "index2,name,age,feet number,secret PIN\n12,ciao,43,72,1234\n"
    it "renders deleting one row" $
      mimeRender (twoRecord @'Deleting) (deleteIndex12 @ TwoRecord) == "index2\n12\n" 
