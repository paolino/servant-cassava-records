{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Data.CSV.Product where

import Data.Csv
import Generics.SOP hiding (Z, fromList, to)
import Protolude hiding (All, ByteString, toList)

--------------------- column level CSV operations ---------------------------

-- | render to 'Field'
newtype R a = R {unR :: a -> Field}

-- | parser from 'Field'
newtype P a = P {unP :: Field -> Parser a}

-- | a column level parse and render with column name
data RT a = RT
  { -- | column name
    rtColumn :: Text
  , -- | how to render 'a'
    rtRender :: R a
  , -- | how to parse 'a'
    rtParse :: P a
  }

-- | default RT
defRT :: (ToField a, FromField a) => Text -> RT a
defRT c = RT c (R toField) (P parseField)

-- | capture the SOP product type of 'a'
newtype RC f a = RC {unRC :: NP f (ProductCode a)}

type family Output a where
  Output (RC RT) = RT
  Output I = I

-- | type level counting Op
data Op where
  NoOp :: Op
  PutOp :: Op -> Op
  UpdateOp :: Op -> Op

type DeleteOp = 'UpdateOp 'NoOp

-- | capture the product type encoding
class (IsProductType x (ProductCode x)) => IsRecord x

instance (IsProductType x (ProductCode x)) => IsRecord x

-- | list of heterogeneous product types
-- * 'f' is a context for their fields
-- * 'o' is the type to index L elements
-- * 'a' is a list of product types
data L (op :: Op) f o (a :: [*]) where
  T
    :: Eq o =>
    { getT :: Output f o
    , unT :: L op f o b
    } ->
    L ('UpdateOp op) f o b
  L :: (Eq a, IsRecord a, Show a) => f a -> L op f o b -> L ('PutOp op) f o (a : b)
  E :: L 'NoOp f o '[]

deriving instance Eq (L op I o a)

deriving instance Show o => Show (L op I o a)

-- | compute the 'Op' from the list of product types
type family PutOpOf a where
  PutOpOf '[] = 'NoOp
  PutOpOf (x : xs) = 'PutOp (PutOpOf xs)

type UpdateOpOf a = 'UpdateOp (PutOpOf a)

--------------------- rendering ---------------------------

-- | render all product types in L
renderProducts :: L op (RC RT) o a -> L op I o a -> [Field]
renderProducts E E = []
renderProducts (L (RC rts) rtss) (L (I x) xs) =
  renderProduct (hmap rtRender rts) (productTypeFrom x) <> renderProducts rtss xs
renderProducts (T RT {..} rtss) (T (I x) xs) = unR rtRender x : renderProducts rtss xs

-- | render one product type
renderProduct :: NP R as -> NP I as -> [Field]
renderProduct Nil Nil = []
renderProduct (R f :* fs) (I x :* xs) =
  f x : renderProduct fs xs

--------------------- parsing ---------------------------

-- | parse all product types in L
parseProducts :: L op (RC RT) o a -> NamedRecord -> Parser (L op I o a)
parseProducts E _ = pure E
parseProducts (L (RC rts) rest) r = do
  s' <- parseProduct rts r
  rest' <- parseProducts rest r
  pure $ L (I $ productTypeTo s') rest'
parseProducts (T RT {..} rest) r = do
  s' <- r .: toS rtColumn >>= unP rtParse
  rest' <- parseProducts rest r
  pure $ T (I s') rest'

parseProduct :: NP RT a -> NamedRecord -> Parser (NP I a)
parseProduct Nil _ = pure Nil
parseProduct (RT c _ (P p) :* ps) r = do
  s <- parseProduct ps r
  x <- r .: toS c >>= p
  pure $ I x :* s

--------------------- select columns ---------------------------

-- | columns of a product type
productColumns :: All Top a => NP RT a -> [Text]
productColumns = hcollapse . hmap (K . rtColumn)

-- | collect columns from an element
recordColumns :: L op (RC RT) o a -> [Text]
recordColumns E = []
recordColumns (L (RC f) rest) = productColumns f <> recordColumns rest
recordColumns (T f rest) = rtColumn f : recordColumns rest

--------------   L handy composition operators -----------------------
li :: (Show x, IsRecord x, Eq x) => x -> L op I o b -> L ('PutOp op) I o (x : b)
li = L . I

lie :: (Show x, IsRecord x, Eq x) => x -> L ('PutOp 'NoOp) I o '[x]
lie x = li x E

ti :: (Eq x, Show x) => x -> L op I x b -> L ('UpdateOp op) I x b
ti = T . I

tie :: (Eq x, Show x) => x -> L ('UpdateOp 'NoOp) I x '[]
tie x = ti x E

-- | L like operator grouping right
(++:) :: (Eq a, IsRecord a, Show a) => f a -> L op f o b -> L ('PutOp op) f o (a : b)
(++:) = L

-- | shortcut to compose last 2 elements
(++^)
  :: (IsRecord a1, Eq a1, IsRecord a2, Eq a2, Show a1, Show a2)
  => f a2
  -> f a1
  -> L ('PutOp ('PutOp 'NoOp)) f o '[a2, a1]
(++^) x y = L x $ L y E

infixr 0 ++:

infixr 0 ++^

-- | a put 'L' element type
type LP f o x = L (PutOpOf x) f o x

-- | an update 'L' element type
type LU f o x = L ('UpdateOp (PutOpOf x)) f o x

-- | a delete 'L' element type
type LD f o = L DeleteOp f o '[]