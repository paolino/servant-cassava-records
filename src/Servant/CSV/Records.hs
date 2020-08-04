{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Servant.CSV.Records where

import Data.ByteString.Lazy (ByteString)
import Data.Csv
import GHC.Exts
import Generics.SOP hiding (Z, fromList, to)
import qualified Network.HTTP.Media as M
import Protolude hiding (All, ByteString, toList)
import Servant.API
  ( Accept (..)
  , MimeRender (..)
  , MimeUnrender (..)
  )
import Data.CSV.Product


-- | supported operations
data Operation = Querying | Updating | Deleting | Putting

-- | mime at type level
-- * 'l' is an encoding index type
-- * 'a' is the operaation we are aiming at
data CSV l (a :: Operation)

-- | mime values tagged with the 'Operation' to perform
newtype EncodingWith l a = EncodingWith {getEncoding :: a}
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (EncodingWith l) where
  pure = EncodingWith
  EncodingWith f <*> EncodingWith x = EncodingWith (f x)

instance Accept (CSV l x) where
  contentType _ = "text" M.// "csv"


class (All Eq (Products l), All IsRecord (Products l)) => GetLRT l where
  type Index l
  type Products l :: [*]
  getLRT :: Proxy l -> L (UpdateOpOf (Products l)) (RC RT) (Index l) (Products l)

proxyL :: Proxy (CSV l a) -> Proxy l
proxyL _ = Proxy

getIndexOp :: L ('UpdateOp op) f o a -> L ('UpdateOp 'NoOp) f o '[]
getIndexOp (T x _) = T x E

type MimeC l u o op = (Index l ~ o, Products l ~ u, GetLRT l, PutOpOf u ~ op)

--------------------- mime unrender instances ---------------------------

mimeUnrender' :: L op (RC RT) o u -> ByteString -> Either [Char] [L op I o u]
mimeUnrender' lrt =
  fmap (toList . snd) . decodeByNameWithP
    do parseProducts lrt
    do defaultDecodeOptions

instance MimeC l u o op => MimeUnrender (CSV l 'Querying) (EncodingWith l [L ('UpdateOp op) I o u]) where
  mimeUnrender (getLRT . proxyL -> lrt) x = EncodingWith @l <$> mimeUnrender' lrt x

instance MimeC l u o op => MimeUnrender (CSV l 'Putting) (EncodingWith l [L op I o u]) where
  mimeUnrender (unT . getLRT . proxyL -> lrt) x = EncodingWith @l <$> mimeUnrender' lrt x

instance MimeC l u o op => MimeUnrender (CSV l 'Updating) (EncodingWith l [L ('UpdateOp op) I o u]) where
  mimeUnrender (getLRT . proxyL -> lrt) x = EncodingWith @l <$> mimeUnrender' lrt x

instance MimeC l u o op => MimeUnrender (CSV l 'Deleting) (EncodingWith l [L DeleteOp I o '[]]) where
  mimeUnrender (getIndexOp . getLRT . proxyL -> lrt) x = EncodingWith @l <$> mimeUnrender' lrt x

--------------------- mime render instances ---------------------------

mimeRender' :: L op (RC RT) o a -> [L op I o a] -> ByteString
mimeRender' lrt xs =
  encodeWith @Record (defaultEncodeOptions {encUseCrLf = False}) $
    fmap fromList $
      (toS <$> recordColumns lrt) : (renderProducts lrt <$> toList xs)

instance MimeC l u o op => MimeRender (CSV l 'Putting) (EncodingWith l [L op I o u]) where
  mimeRender (unT . getLRT . proxyL -> lrt) = mimeRender' lrt . getEncoding

instance MimeC l u o op => MimeRender (CSV l 'Updating) (EncodingWith l [L ('UpdateOp op) I o u]) where
  mimeRender (getLRT . proxyL -> lrt) = mimeRender' lrt . getEncoding

instance MimeC l u o op => MimeRender (CSV l 'Deleting) (EncodingWith l [L DeleteOp I o '[]]) where
  mimeRender (getIndexOp . getLRT . proxyL -> lrt) = mimeRender' lrt . getEncoding

instance MimeC l u o op => MimeRender (CSV l 'Querying) (EncodingWith l [L ('UpdateOp op) I o u]) where
  mimeRender (getLRT . proxyL -> lrt) = mimeRender' lrt . getEncoding

--------------------- supported mime types data ---------------------------

-- | type that can be queried 
type QueryL l = LU I (Index l) (Products l)

-- | type  that can be inserted
type PutL l = LP I (Index l) (Products l)

-- | type that can be updated (same as queried)
type UpdateL l = QueryL l

-- |  type for record deletion
type DeleteL l = LD I (Index l)
