module Theme exposing (..)

import Color
import Color.Convert
import Color.Manipulate
import Element
import Element.Font as Font
import Interface as UI


offWhiteBackgroundColor : Element.Color
offWhiteBackgroundColor =
    UI.hex_to_color "F8FAFB"


whiteFontColor : Element.Color
whiteFontColor =
    UI.hex_to_color "F0F1FF"


darkHeaderColor : Element.Color
darkHeaderColor =
    UI.hex_to_color "191449"


textColor : Element.Color
textColor =
    UI.hex_to_color "837F98"


borderColor : Element.Color
borderColor =
    UI.hex_to_color "DFDFE1"


buttonPrimaryColor : Element.Color
buttonPrimaryColor =
    UI.hex_to_color "6F6AF8"


buttonPrimaryHoveredColor : Element.Color
buttonPrimaryHoveredColor =
    Color.Convert.hexToColor "6F6AF8"
        |> Result.withDefault Color.red
        |> Color.Manipulate.lighten 0.05
        |> Color.toRgba
        |> UI.rgbaToColor

fontFamilyPoppins : Element.Attribute msg
fontFamilyPoppins =
    fontFamily
        "Poppins"
        "https://fonts.googleapis.com/css2?family=Poppins:wght@600&family=Roboto+Slab:wght@900&display=swap"

fontFamily : String -> String -> Element.Attribute msg
fontFamily fontName fontUrl =
    Font.family
        [ Font.external
            { name = fontName
            , url = fontUrl
            }
        , Font.sansSerif
        ]

