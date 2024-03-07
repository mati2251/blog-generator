module Html
  ( Html,
    Title,
    Structure,
    html_,
    p_,
    h1_,
    append_,
    render,
  )
where

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
        ( el_ "head" (el_ "title" title)
            <> el_ "body" (getStructureString content)
        )
    )

p_ :: String -> Structure
p_ = Structure . el_ "p"

h1_ :: String -> Structure
h1_ = Structure . el_ "h1"
