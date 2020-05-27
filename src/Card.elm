module Card exposing (Card, CardPhrase)


type alias Card =
    { id : String
    , title : String
    , phrases : List CardPhrase
    }


type alias CardPhrase =
    { phrase : String
    , translation : String
    }
