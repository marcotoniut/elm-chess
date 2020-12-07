module Component exposing (..)

import Html exposing (Html, Attribute, text)
import Html.Attributes exposing (attribute)

emptyAttribute : Attribute a
emptyAttribute = attribute "__noop" ""

blank : Html a
blank = text ""
