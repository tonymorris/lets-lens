module Lets.Data (
  Locality(..)
, Address(..)
, Person(..)
, IntAnd(..)
, fredLocality
, fredAddress
, fred
, maryLocality
, maryAddress
, mary
, tenAndABC
) where

data Locality =
  Locality
    String -- city
    String -- state
    String -- country
  deriving (Eq, Show)  

data Address =
  Address
    String -- street
    String -- suburb
    Locality
  deriving (Eq, Show)  

data Person =
  Person
    Int -- age
    String -- name
    Address -- address
  deriving (Eq, Show)

data IntAnd a =
  IntAnd
    Int
    a
  deriving (Eq, Show)

fredLocality ::
  Locality
fredLocality =
  Locality
    "Fredmania"
    "New South Fred"
    "Fredalia"

fredAddress ::
  Address
fredAddress =
  Address
    "15 Fred St"
    "Fredville"
    fredLocality

fred ::
  Person
fred =
  Person
    24
    "Fred"
    fredAddress

maryLocality ::
  Locality
maryLocality =
  Locality
    "Mary Mary"
    "Western Mary"
    "Maristan"

maryAddress ::
  Address
maryAddress =
  Address
    "83 Mary Ln"
    "Maryland"
    maryLocality

mary ::
  Person
mary =
  Person
    28
    "Mary"
    maryAddress

tenAndABC ::
  IntAnd String
tenAndABC =
  IntAnd 10 "ABC"
