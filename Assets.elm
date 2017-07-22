module Assets exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (attribute)
import Svg exposing (..)
import Svg.Attributes exposing (..)


globe : String -> Html msg
globe sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Language" ]
        , Svg.path [ d "M32.0123,56.0206l-.0019,0-.0019,0a24,24,0,1,1,.0038,0Zm-10.467-8.0949a27.6451,27.6451,0,0,1-1.5314-3.57.4964.4964,0,0,0-.4661-.3348h-2.479a.4994.4994,0,0,0-.3921.8119,20.0893,20.0893,0,0,0,4.1545,3.7417A.4978.4978,0,0,0,21.5453,47.9257Zm-7.5119-7.905h4.1888a.4917.4917,0,0,0,.4854-.587,38.981,38.981,0,0,1-.613-4.9434.4957.4957,0,0,0-.4927-.47H12.68a.5069.5069,0,0,0-.5031.5654,19.8751,19.8751,0,0,0,1.3885,5.1263A.5071.5071,0,0,0,14.0334,40.0207Zm-1.3531-10h4.9216a.4957.4957,0,0,0,.4927-.4695,38.98,38.98,0,0,1,.613-4.9435.4917.4917,0,0,0-.4854-.5871H14.0334a.5071.5071,0,0,0-.4677.3084,19.8773,19.8773,0,0,0-1.3885,5.1263A.5069.5069,0,0,0,12.68,30.0207Zm4.3885-10h2.479a.4963.4963,0,0,0,.4661-.3347,27.6084,27.6084,0,0,1,1.5314-3.57.4978.4978,0,0,0-.7141-.6486,20.0906,20.0906,0,0,0-4.1545,3.7415A.4994.4994,0,0,0,17.0688,20.0207ZM29.25,12.7948c-1.97,1.1322-3.6857,3.4513-4.9542,6.5359a.5039.5039,0,0,0,.4669.69H29.51a.5.5,0,0,0,.5-.5V13.2293A.5057.5057,0,0,0,29.25,12.7948ZM29.51,24.0206H23.2576a.5017.5017,0,0,0-.4939.3977,36.6608,36.6608,0,0,0-.6726,5.0664.507.507,0,0,0,.5039.536H29.51a.5.5,0,0,0,.5-.5v-5A.5.5,0,0,0,29.51,24.0206Zm0,10H22.595a.5071.5071,0,0,0-.5039.5361,36.6593,36.6593,0,0,0,.6726,5.0663.5017.5017,0,0,0,.4939.3976H29.51a.5.5,0,0,0,.5-.5v-5A.5.5,0,0,0,29.51,34.0207Zm0,10H24.7624a.504.504,0,0,0-.4669.69c1.2685,3.0846,2.9843,5.4037,4.9542,6.5359a.5057.5057,0,0,0,.7607-.4345V44.5207A.5.5,0,0,0,29.51,44.0207Zm21.83-10H46.4189a.496.496,0,0,0-.4929.4695,38.9441,38.9441,0,0,1-.613,4.9435.4919.4919,0,0,0,.4856.587h4.1885a.5069.5069,0,0,0,.4678-.3083,19.862,19.862,0,0,0,1.3885-5.1263A.5068.5068,0,0,0,51.34,34.0207Zm-4.3883,10H44.473a.4965.4965,0,0,0-.4661.3348,27.6451,27.6451,0,0,1-1.5314,3.57.4977.4977,0,0,0,.7141.6485,20.088,20.088,0,0,0,4.1544-3.7416A.4994.4994,0,0,0,46.952,44.0207ZM34.771,51.2466c1.97-1.1322,3.6858-3.4512,4.9544-6.5358a.504.504,0,0,0-.4669-.69H34.51a.5.5,0,0,0-.5.5v6.2914A.5056.5056,0,0,0,34.771,51.2466ZM34.51,40.0207h6.2529a.5021.5021,0,0,0,.4941-.3976,36.6751,36.6751,0,0,0,.6724-5.0663.5068.5068,0,0,0-.5037-.5361H34.51a.5.5,0,0,0-.5.5v5A.5.5,0,0,0,34.51,40.0207Zm-.5-26.7914v6.2915a.5.5,0,0,0,.5.5h4.7482a.5039.5039,0,0,0,.4669-.69c-1.2684-3.0846-2.9843-5.4036-4.9544-6.5359A.5057.5057,0,0,0,34.01,13.2293Zm0,11.2913v5a.5.5,0,0,0,.5.5H41.426a.5068.5068,0,0,0,.5037-.536,36.7071,36.7071,0,0,0-.6724-5.0664.5021.5021,0,0,0-.4941-.3977H34.51A.5.5,0,0,0,34.01,24.5206Zm8.4651-8.4047a27.6377,27.6377,0,0,1,1.5314,3.57.4964.4964,0,0,0,.4661.3347h2.479a.4994.4994,0,0,0,.392-.8118A20.0909,20.0909,0,0,0,43.19,15.4673.4978.4978,0,0,0,42.4755,16.1159Zm2.8375,8.4918a38.9441,38.9441,0,0,1,.613,4.9435.496.496,0,0,0,.4929.4695H51.34a.5068.5068,0,0,0,.5031-.5654,19.8689,19.8689,0,0,0-1.3885-5.1265.5071.5071,0,0,0-.4678-.3082H45.7986A.4918.4918,0,0,0,45.313,24.6077Z", attribute "fill-rule" "evenodd" ] []
        ]


previous : String -> Html msg
previous sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Way back" ]
        , Svg.path [ d "M51.022,44.7359,34.0235,34.419v8.5233a1.9967,1.9967,0,0,1-3.0014,1.7936L14.01,34.4112v8.61a2,2,0,0,1-4,0v-22a2,2,0,0,1,4,0v8.6115L31.0221,19.3075a1.9968,1.9968,0,0,1,3.0014,1.7938v8.5231L51.022,19.3075a1.9969,1.9969,0,0,1,3.0015,1.7938v21.841A1.9967,1.9967,0,0,1,51.022,44.7359Z", attribute "fill-rule" "evenodd" ] []
        ]


rewind : String -> Html msg
rewind sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Back" ]
        , Svg.path [ d "M29.0222,44.7359l-17.9934-10.92a2.12,2.12,0,0,1,0-3.5874l17.9934-10.92a1.9968,1.9968,0,0,1,3.0014,1.7938v8.5231L49.0222,19.3075a1.9968,1.9968,0,0,1,3.0014,1.7938v21.841a1.9967,1.9967,0,0,1-3.0014,1.7936L32.0236,34.419v8.5233A1.9967,1.9967,0,0,1,29.0222,44.7359Z", attribute "fill-rule" "evenodd" ] []
        ]


fastForward : String -> Html msg
fastForward sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Forward" ]
        , Svg.path [ d "M34.9987,19.3057,52.9919,30.2262a2.12,2.12,0,0,1,0,3.5874l-17.9932,10.92A1.9968,1.9968,0,0,1,31.9973,42.94V34.4174L14.9987,44.7341A1.9968,1.9968,0,0,1,11.9973,42.94V21.0994a1.9967,1.9967,0,0,1,3.0014-1.7937L31.9973,29.6226V21.0994A1.9967,1.9967,0,0,1,34.9987,19.3057Z", attribute "fill-rule" "evenodd" ] []
        ]


next : String -> Html msg
next sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Way forward" ]
        , Svg.path [ d "M12.9987,19.3057,29.9973,29.6226V21.0994a1.9967,1.9967,0,0,1,3.0014-1.7937L50.01,29.63v-8.61a2,2,0,0,1,4,0v22a2,2,0,0,1-4,0V34.4094L32.9987,44.7341A1.9968,1.9968,0,0,1,29.9973,42.94V34.4174L12.9987,44.7341A1.9967,1.9967,0,0,1,9.9973,42.94V21.0994A1.9967,1.9967,0,0,1,12.9987,19.3057Z", attribute "fill-rule" "evenodd" ] []
        ]


pause : String -> Html msg
pause sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Pause" ]
        , Svg.path [ d "M25.01,50.0208h-6a3,3,0,0,1-3-3v-30a3,3,0,0,1,3-3h6a3,3,0,0,1,3,3v30A3,3,0,0,1,25.01,50.0208Zm14,0a3,3,0,0,1-3-3v-30a3,3,0,0,1,3-3h6a3,3,0,0,1,3,3v30a3,3,0,0,1-3,3Z", attribute "fill-rule" "evenodd" ] []
        ]


play : String -> Html msg
play sz =
    svg [ width sz, height sz, viewBox "0 0 64 64" ]
        [ Svg.title [] [ text "Play" ]
        , Svg.path [ d "M18.8913,12.2186,54.8874,30.215a2,2,0,0,1,0,3.5771L18.8913,51.7889A2,2,0,0,1,15.9972,50V14.0071A2,2,0,0,1,18.8913,12.2186Z", attribute "fill-rule" "evenodd" ] []
        ]
