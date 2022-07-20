module Main where

import Prelude
import Effect.Class (liftEffect)
import Data.Array (fold)
import Data.List (List(..))
import Data.List as List
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Newtype (wrap, unwrap, class Newtype)
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
import Data.Array as Array
import Effect.Console (log)

type Model
  = { emojiName :: String
    , placeholderName :: String
    , content :: String
    }

render :: Model -> DisplayCharacter
render { emojiName, placeholderName, content } = fold $ characterMatrix emojiName placeholderName <$> String.split (wrap "") content

renderView :: Model -> DisplayCharacter -> Html Msg
renderView { emojiName } (DisplayCharacter rows) = H.div_ $ renderRow <$> Array.fromFoldable rows
  where
  renderRow :: List String -> Html Msg
  renderRow columns =
    H.div_
      $ ( \col ->
            if col == emojiName then
              H.img [ A.class' "w2 h2 bg-black" ]
            else
              H.img [ A.class' "w2 h2 bg-white" ]
        )
      <$> Array.fromFoldable columns

data Msg
  = EmojiNameChanged String
  | PlaceholderNameChanged String
  | ContentChanged String

application :: Application Model Msg
application =
  { view: view
  , subscribe: mempty
  , init: init
  , update: update
  }

init :: Tuple Model (Array (Aff (Maybe Msg)))
init =
  { emojiName: ""
  , placeholderName: ""
  , content: ""
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
  let
    nextModel = model { content = content }

    logEffect = map (const Nothing) <<< log <<< show $ render nextModel
  in
    nextModel /\ [ liftEffect logEffect ]

inputClass :: String
inputClass = "pa2 black-80 br2 b--light-blue b--solid avenir f5"

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
        renderView model displayObject
    ]

main :: Effect Unit
main = Flame.mount_ (wrap "#app") application

newtype DisplayCharacter
  = DisplayCharacter (List (List String))

instance showDisplayCharacter :: Show DisplayCharacter where
  show = unwrap >>> map Array.fromFoldable >>> Array.fromFoldable >>> show

derive instance newtypeDisplayCharacter :: Newtype DisplayCharacter _

derive newtype instance monoidDisplayCharacter :: Monoid DisplayCharacter

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
    [ [ x, x, x ]
    , [ x, o, x ]
    , [ x, x, x ]
    , [ x, o, x ]
    , [ x, x, x ]
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

characterMatrix _ o " " =
  asDisplayCharacter
    [ [ o ]
    , [ o ]
    , [ o ]
    , [ o ]
    , [ o ]
    ]

characterMatrix _ _ _ =
  asDisplayCharacter
    [ []
    , []
    , []
    , []
    , []
    , []
    ]
