module Internal.Placement exposing (Placement(..), toAttribute)

import Ui
import Ui.Anim
import Ui.Layout
import Ui.Prose


type Placement
    = Above
    | Below


toAttribute : Placement -> (Ui.Element msg -> Ui.Attribute msg)
toAttribute placement =
    case placement of
        Above ->
            Ui.above

        Below ->
            Ui.below
