main = do
  print "stuff"

-- Product data types is a combination of types (AND)
-- An address is a combination of String and Number (Zip code)
-- A name could be a combination of Strings - First Name and Last name

-- Sum data types is a set of data that is either (OR)
-- meaning you can only choose one particular data
-- A boolean type which has TRUE or FALSE
-- A traffic light type which has the color Red, Yellor or Green

-- An example of Product data types in popular programming languages such as C
-- are structs
-- author_name is a combination of two char arrays - first_name and last_name
-- book is a combination of author_name type, two char arrays isbn and title
-- an int year and a double prie

-- struct author_name {
--     char *first_name
--     char *last_name
-- };

-- struct book {
--     author_name author;
--     char *isbn
--     char *title
--     int year_published;
--     double price
-- };


-- This is how the struct above would look like in Haskell using the
-- record syntax

data AuthorName = AuthorName
  { first_name :: String,
    second_name :: String
  }

data Book = Book
  { author :: AuthorName,
    isbn :: String,
    title :: String,
    year :: Int,
    price :: Double
  }

-- Converting the struct to a non-record syntax
-- data AuthorName = AuthorName String String
-- data Book = AuthorName String String Int

-- The existing problem of using product data types commonly found
-- in popular programming languages such as Java

-- 

-- public class Book {
--     Author author;
--     String isbn;
--     String title;
--     int yearPublished;
--     double price;
-- }

-- public class VinylRecord {
--     String artist;
--     String title;
--     int yearPublished;
--     double price;
-- }