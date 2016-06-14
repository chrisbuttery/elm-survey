module Model exposing (..)


type alias Model =
  { options : List Option
  , newOptionTitle : String
  , nextID : Int
  }


type alias Option =
  { title: String
  , count: Int
  , id: Int
  }


{--
  newOption:
  Take a string, int and int.
  Return a structure based on the 'Option' type
--}

newOption : String -> Int -> Int -> Option
newOption title count id =
  { title = title
  , count = count
  , id = id
  }
