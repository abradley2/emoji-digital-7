module Main where

import Prelude
import Data.Array (fold, foldl)
import Data.Array as Array
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Newtype (wrap, unwrap, class Newtype)
import Data.String as String
import Data.Tuple (Tuple)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Flame (Html)
import Flame as Flame
import Flame.Application.EffectList (Application)
import Flame.Html.Attribute as A
import Flame.Html.Element as H
import Flame.Html.Event as E

type Model
  = { emojiName :: String
    , placeholderName :: String
    , content :: String
    }

render :: Model -> DisplayCharacter
render { content, emojiName, placeholderName } =
  foldl
    ( \acc cur ->
        if cur == characterMatrix emojiName placeholderName " " then
          acc <> cur
        else
          acc <> cur <> characterMatrix emojiName placeholderName " "
    )
    mempty
    $ characterMatrix emojiName placeholderName
    <$> String.split (wrap "") content

renderView :: Model -> DisplayCharacter -> Html Msg
renderView { emojiName } (DisplayCharacter rows) =
  H.div
    [ A.class' "flex flex-column items-center" ]
    $ renderRow
    <$> Array.fromFoldable rows
  where
  renderRow :: List String -> Html Msg
  renderRow columns =
    H.div [ A.class' "flex" ]
      $ ( \col ->
            if col == emojiName then
              H.img
                [ A.class' "flex-shrink-0 ma0 pa0 bn w1.5 h1.5 bg-transparent"
                , A.src "im_square.gif"
                ]
            else
              H.img [ A.class' "flex-shrink-0 ma0 pa0 bn w1.5 h1.5 bg-transparent" ]
        )
      <$> Array.fromFoldable columns

newline :: String
newline =
  """
"""

renderText :: DisplayCharacter -> String
renderText (DisplayCharacter rows) =
  fold <<< Array.intersperse newline
    $ (renderRow <$> Array.fromFoldable rows)
  where
  renderRow :: List String -> String
  renderRow (Cons column columns) = ":" <> column <> ":" <> renderRow columns

  renderRow Nil = mempty

data Msg
  = EmojiNameChanged String
  | PlaceholderNameChanged String
  | ContentChanged String
  | NoOp

application :: Application Model Msg
application =
  { view: view
  , subscribe: mempty
  , init: init
  , update: update
  }

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { emojiName: "awesome-face"
  , placeholderName: "blank"
  , content: "COOL"
  }
    /\ mempty

update :: Model -> Msg -> Tuple Model (Array (Aff (Maybe Msg)))
update model (EmojiNameChanged emojiName) =
  model { emojiName = emojiName }
    /\ mempty

update model (PlaceholderNameChanged placeholderName) =
  model { placeholderName = placeholderName }
    /\ mempty

update model (ContentChanged content) =
  model { content = String.toUpper content }
    /\ mempty

update model NoOp = model /\ mempty

inputClass :: String
inputClass = "pa2 black-80 br2 b--blue outline-light-blue b--solid avenir f5"

inputLabelClass :: String
inputLabelClass = "black-70 f5 dib mb1"

view :: Model -> Html Msg
view model =
  H.div
    [ A.class' "pa3 avenir"
    ]
    [ H.div
        [ A.class' "flex flex-column items-center" ]
        [ H.div_
            [ H.label
                [ A.class' inputLabelClass
                , A.for "emoji-name"
                ]
                [ H.text "Emoji Name" ]
            , H.br
            , H.input
                [ A.type' "text"
                , A.class' inputClass
                , A.id "emoji-name"
                , A.value model.emojiName
                , E.onInput EmojiNameChanged
                ]
            ]
        , H.div
            [ A.class' "mt3" ]
            [ H.label
                [ A.class' inputLabelClass
                , A.for "placeholder-name"
                ]
                [ H.text "Placeholder Name" ]
            , H.br
            , H.input
                [ A.type' "text"
                , A.class' inputClass
                , A.id "placeholder-name"
                , A.value model.placeholderName
                , E.onInput PlaceholderNameChanged
                ]
            ]
        , H.div
            [ A.class' "mt3" ]
            [ H.label
                [ A.class' inputLabelClass
                , A.for "content"
                ]
                [ H.text "Content" ]
            , H.br
            , H.input
                [ A.type' "text"
                , A.class' inputClass
                , A.id "content"
                , A.value model.content
                , E.onInput ContentChanged
                ]
            ]
        ]
    , let
        displayObject = render model
      in
        H.div_
          [ H.div
              [ A.class' "mt3" ]
              [ renderView model displayObject
              ]
          , H.textarea
              [ A.class' "mt3 db courier center measure-wide"
              , A.value (renderText displayObject)
              , E.onInput $ const NoOp
              , E.onChange NoOp
              , A.rows 5
              , A.cols $ 300
              ]
              [ H.text $ renderText displayObject
              ]
          ]
    ]

main :: Effect Unit
main = Flame.mount_ (wrap "#app") application

newtype DisplayCharacter
  = DisplayCharacter (List (List String))

instance showDisplayCharacter :: Show DisplayCharacter where
  show = unwrap >>> map Array.fromFoldable >>> Array.fromFoldable >>> show

derive instance newtypeDisplayCharacter :: Newtype DisplayCharacter _

derive instance eqDisplayCharacter :: Eq DisplayCharacter

instance monoidDisplayCharacter :: Monoid DisplayCharacter where
  mempty =
    [ []
    , []
    , []
    , []
    , []
    ]
      # asDisplayCharacter

instance semigroupDisplayCharacter :: Semigroup DisplayCharacter where
  append (DisplayCharacter (Cons rowA rowsA)) (DisplayCharacter (Cons rowB rowsB)) = DisplayCharacter $ Cons (rowA <> rowB) (unwrap $ (DisplayCharacter rowsA <> DisplayCharacter rowsB))
  append _ _ = mempty

asDisplayCharacter :: Array (Array String) -> DisplayCharacter
asDisplayCharacter = map List.fromFoldable >>> List.fromFoldable >>> DisplayCharacter

characterMatrix :: String -> String -> String -> DisplayCharacter
characterMatrix x o "A" =
  asDisplayCharacter
    [ [ o, x, o ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    ]

characterMatrix x o "B" =
  asDisplayCharacter
    [ [ x, x, o ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    , [ x, x, o ]
    ]

characterMatrix x o "C" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, o ]
    , [ x, o, o ]
    , [ x, o, o ]
    , [ x, x, x ]
    ]

characterMatrix x o "D" =
  asDisplayCharacter
    [ [ x, x, o ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, o ]
    ]

characterMatrix x o "E" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, o ]
    , [ x, x, x ]
    , [ x, o, o ]
    , [ x, x, x ]
    ]

characterMatrix x o "F" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, o ]
    , [ x, x, x ]
    , [ x, o, o ]
    , [ x, o, o ]
    ]

characterMatrix x o "G" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, o ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    ]

characterMatrix x o "H" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    ]

characterMatrix x _ "I" =
  asDisplayCharacter
    [ [ x ]
    , [ x ]
    , [ x ]
    , [ x ]
    , [ x ]
    ]

characterMatrix x o "J" =
  asDisplayCharacter
    [ [ o, o, x ]
    , [ o, o, x ]
    , [ o, o, x ]
    , [ x, o, x ]
    , [ o, x, o ]
    ]

characterMatrix x o "K" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, o ]
    , [ x, o, x ]
    , [ x, o, x ]
    ]

characterMatrix x o "L" =
  asDisplayCharacter
    [ [ x, o, o ]
    , [ x, o, o ]
    , [ x, o, o ]
    , [ x, o, o ]
    , [ x, x, x ]
    ]

characterMatrix x o "M" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    ]

characterMatrix x o "N" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    ]

characterMatrix x o "O" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    ]

characterMatrix x o "P" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, o ]
    , [ x, o, o ]
    ]

characterMatrix x o "Q" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ o, o, x ]
    ]

characterMatrix x o "R" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, x, o ]
    , [ x, x, o ]
    , [ x, o, x ]
    ]

characterMatrix x o "S" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ x, o, o ]
    , [ x, x, x ]
    , [ o, o, x ]
    , [ x, x, x ]
    ]

characterMatrix x o "T" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ o, x, o ]
    ]

characterMatrix x o "U" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    ]

characterMatrix x o "V" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ o, x, o ]
    , [ o, x, o ]
    ]

characterMatrix x o "W" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ x, o, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    ]

characterMatrix x o "X" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ x, o, x ]
    ]

characterMatrix x o "Y" =
  asDisplayCharacter
    [ [ x, o, x ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ o, x, o ]
    , [ o, x, o ]
    ]

characterMatrix x o "Z" =
  asDisplayCharacter
    [ [ x, x, x ]
    , [ o, o, x ]
    , [ o, x, o ]
    , [ x, o, o ]
    , [ x, x, x ]
    ]

characterMatrix _ o " " =
  asDisplayCharacter
    [ [ o ]
    , [ o ]
    , [ o ]
    , [ o ]
    , [ o ]
    ]

characterMatrix _ _ _ = mempty
