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

-- Using a super class to get the similarities of Book and Vinyl Record
-- public class StoreItem {
--   String title;
--   int yearPublished;
--   double price;
-- }

-- public class Book extends StoreItem {
--   Author author;
--   String isbn;
-- }

-- public class VinylRecord extends StoreItem {
--   String artist;
-- }


-- public class CollectibleToy {
--   String name;
--   String description;
--   double price;
-- }

-- To include the CollectibleToy as a StoreItem (see above StoreItem class)
-- You need to refactor StoreItem to only contain price property
-- Because it's the only property that is common between CollectibleToy,
-- Book and VinylRecord

-- The ugly result ...
-- public class StoreItem {
--   double price;
-- }

-- public class Book extends StoreItem {
--   Author author;
--   String isbn;
--   String title;
--   int yearPublished;
-- }

-- public class VinylRecord extends StoreItem {
--   String artist;
--   String title;
--   int yearPublished;
-- }

-- public class CollectibleToy {
--   String name;
--   String description;
--   double price;
-- }

-- A Car type, SportsCar type and a Spoiler type
-- data SportsCar = SportsCar Car Spoiler


-- The sum type model enables you to define different sets
-- of data

-- A simple sum type model for Boolean, it can be either False or True
data Bool = False | True

-- A more complex sum type model for Name (FirstName & LastName)
-- and NameWithMiddle (FirstName, MiddleName & LastName)
type FirstName = String
type LastName = String
type MiddleName = String

data Name = Name FirstName LastName 
  | NameWithMiddle FirstName MiddleName LastName
  -- Adding this data constructor for tricky edge-cases for Authors who uses initials
  -- ex. H.P. Lovecraft or J.K. Rowling
  | TwoInitialsWithLast Char Char LastName
  -- Another example on how sum-type model can accomodate weird cases for an author who uses MiddleName and LastName initials
  -- ex. Andrew W.K.
  | FirstNameWithTwoInits FirstName Char Char

-- Making a simpler way of implementing Artist and Author using sum-types
data Creator = AuthorCreator Author | ArtistCreator Artist

data Author = Author Name
-- Using another sum type because an Artist can be either a Person or a Band
data Artist = Person Name | Band String

-- Here's how to create an instance of the Creator type

-- Though this is a bit verbose, but its definietly better than
-- hierarchical way (Superclass, Subclasses) of creating creators
hpLoveCraft :: Creator
hpLoveCraft = AuthorCreator(
               (Author
                (TwoInitialsWithLast 'H' 'P' "Lovecraft"))
              )

-- Accomodating weird cases like H.P. Lovecraft, Andrew W.K. in an OOP language
-- would look like this

-- public class Name {
--   String firstName;
--   String lastName;
--   String middleName;
--   char firstInitial;
--   char middleInitial;
--   char lastInitial;
-- }

