module Bookstore
  ( BookInfo
  , MagazineInfo
  , BookReview
  , BetterReview
  , BookRecord
  , myInfo
  , myBookReview
  , myBetterReview
  , customer1
  , customer2
  ) where

-- Defining Types
data BookInfo =
  Book Int
       String
       [String]
  deriving (Show)

data MagazineInfo =
  Magazine Int
           String
           [String]
  deriving (Show)

-- main = putStrLn "Hello World"
data BookReview =
  BookReview BookInfo
             CustomerId
             String

-- Type synonyms
type CustomerId = Int

type ReviewBody = String

data BetterReview =
  BetterReview BookInfo
               CustomerId
               ReviewBody

type BookRecord = (BookInfo, BookReview)

-- algebraic data type
type CardHolder = String

type CardNumber = String

type Address = [String]

data BillingInfo
  = CreditCard CardNumber
               CardHolder
               Address
  | CashOnDelivery
  | Invoice CustomerId
  deriving (Show)

data Customer = Customer
  { customerID      :: CustomerId
  , customerName    :: String
  , customerAddress :: Address
  } deriving (Show)

myInfo :: BookInfo
myInfo =
  Book 9780135072455 "Algebra of Programming" ["Richard Bird", "Oege de Moor"]

myBookReview :: BookReview
myBookReview = BookReview myInfo 165688 "Very nice book"

myBetterReview :: BetterReview
myBetterReview = BetterReview myInfo 165688 "Very nice book"

customer1 :: Customer
customer1 =
  Customer 271828 "J.R. Hacker" ["255 Syntax Ct", "Milpitas, CA 95134", "USA"]

customer2 :: Customer
customer2 =
  Customer
  { customerID = 271828
  , customerAddress = ["1048576 Disk Drive", "Milpitas, CA 95134", "USA"]
  , customerName = "Jane Q. Citizen"
  }
