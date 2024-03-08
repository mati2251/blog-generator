module Html.Internal where

newtype Html = Html String

newtype Structure = Structure String

type Title = String

append_ :: Structure -> Structure -> Structure
append_ a b = Structure (getStructureString a <> getStructureString b)

render :: Html -> String
render (Html a) = a

getStructureString :: Structure -> String
getStructureString (Structure a) = a

el_ :: String -> String -> String
el_ tag content = "<" <> tag <> ">" <> content <> "</" <> tag <> ">"

html_ :: Title -> Structure -> Html
html_ title content =
  Html
    ( el_
        "html"
        ( el_ "head" (el_ "title" (escape title))
            <> el_ "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el_ "p" . escape

h1_ :: String -> Structure
h1_ = Structure . el_ "h1" . escape

list_ :: String -> [Structure] -> Structure
list_ tag structs = Structure (el_ tag (concatMap getStructureString structs))

ul_ :: [Structure] -> Structure
ul_ = list_ "ul"

ol_ :: [Structure] -> Structure
ol_ = list_ "ol"

code_ :: String -> Structure
code_ = Structure . el_ "pre" . escape

escape :: String -> String
escape =
  let escapeChar c = case c of
        '<' -> "&lt;"
        '>' -> "&gt;"
        '&' -> "&amp;"
        '"' -> "&quot;"
        '\'' -> "&apos;"
        _ -> [c]
   in concatMap escapeChar
