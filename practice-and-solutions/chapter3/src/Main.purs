module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry

showEntry :: Entry -> String
showEntry entry =
  entry.lastName <> ", " <>
  entry.firstName <> ", " <>
  showAddress entry.address

showAddress :: Address -> String
showAddress address =
  address.street <> ", " <>
  address.city   <> ", " <>
  address.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName book = head $ filter p book
  where
    p :: Entry -> Boolean
    p entry = entry.firstName == firstName && entry.lastName == lastName

findEntry' :: String -> String -> AddressBook -> Maybe Entry
findEntry' firstName lastName = head <<< filter p
  where
    p :: Entry -> Boolean
    p entry = entry.firstName == firstName && entry.lastName == lastName

findEntry'' :: String -> String -> AddressBook -> Maybe Entry
findEntry'' firstName lastName = filter p >>> head
  where
    p :: Entry -> Boolean
    p entry = entry.firstName == firstName && entry.lastName == lastName

-- a = { street: "utca", city: "varos", state: "state"}
-- e1 = { firstName: "A", lastName: "B", address: a }
-- e2 = { firstName: "C", lastName: "D", address: a }
--
-- b2 = insertEntry e1 emptyBook
-- b3 = insertEntry e2 b2
--
-- findEntry "A" "B" b3
-- findEntry "A" "C" b3

-- EXERCISES
--
-- 2.
findEntryByAddress :: String -> String -> String -> AddressBook -> Maybe Entry
findEntryByAddress street city state = head <<< filter predicate
  where
    predicate :: Entry -> Boolean
    predicate e = e.address.street == street && e.address.city == city && e.address.state == state

-- 3.
-- Pretty ugly, and I'm sure that there is something more efficient. QUESTION: keep looking.
isNameInAddressBook :: String -> String -> AddressBook -> Boolean
isNameInAddressBook firstName lastName book =
  case filter (\e -> e.firstName == firstName && e.lastName == lastName) book of
    Nil -> false
    _ -> true

-- 4.
removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy p
  where
    p e1 e2 = e1.firstName == e2.firstName && e1.lastName == e2.lastName
