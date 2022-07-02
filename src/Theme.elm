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


lightenDarkHeaderColor : Float -> Element.Color
lightenDarkHeaderColor lighten =
    darkHeaderRawColor
        |> Color.Manipulate.lighten lighten
        |> Color.toRgba
        |> UI.rgbaToColor


darkHeaderRawColor : Color.Color
darkHeaderRawColor =
    UI.getRawColorFromHex "191449"


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


rawPurpleColor : Color.Color
rawPurpleColor =
    UI.getRawColorFromHex "6363FC"


purpleColor : Element.Color
purpleColor =
    rawPurpleColor
        |> UI.convertColor


lightPurpleColor : Element.Color
lightPurpleColor =
    UI.getRawColorFromHex "6363FC"
        |> Color.Manipulate.lighten 0.15
        |> UI.convertColor


lightenPurpleColor : Float -> Element.Color
lightenPurpleColor pct =
    UI.getRawColorFromHex "6363FC"
        |> Color.Manipulate.lighten pct
        |> UI.convertColor


offWhiteColor : Element.Color
offWhiteColor =
    UI.hex_to_color "F4F6FD"
