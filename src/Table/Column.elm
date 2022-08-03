module Table.Column exposing
    ( Column, date, dateTime, float, int, string, ellipsis, custom
    , date_, int_, string_, custom_
    , Sorter(..)
    , keep, tooltip, withSort
    , dateTime_
    )

{-| Flexible helpers for constructing `Table.Column`s


# Primary Constructors

@docs Column, date, dateTime, float, int, string, ellipsis, custom


# Extended Constructors

@docs date_, int_, string_, custom_


# Exposing Type Constructors

@docs Sorter


# Column Modifiers

@docs keep, tooltip, withSort

-}

import Element.WithContext as E exposing (Element, Length, text)
import Html as H
import Html.Attributes as Attr
import Json.Encode as Encode
import Strftime
import Time


formatDate : Time.Posix -> String
formatDate =
    Strftime.format "%Y-%m-%d" Time.utc


format : Time.Posix -> Element { ctx | tz : Time.Zone } msg
format time =
    let
        format_ : Time.Zone -> String
        format_ tz =
            let
                fromZoneAndTime : Time.Zone -> Time.Posix -> String
                fromZoneAndTime =
                    Strftime.format "%-I:%M %p %b %d %Y"
            in
            if Time.utc == tz then
                fromZoneAndTime Time.utc time ++ " UTC"

            else
                fromZoneAndTime tz time
    in
    E.with .tz (format_ >> E.text)


type alias Column ctx record msg =
    { title : String
    , startingWidth : Maybe Length
    , sort : Sorter record
    , body : record -> Element ctx msg
    , toStr : record -> String
    , filter : Maybe (String -> String -> Bool)
    , encode : record -> Encode.Value
    , tooltip : Maybe String
    }


keep :
    (String -> String -> Bool)
    -> Column ctx record msg
    -> Column ctx record msg
keep fn c =
    { c | filter = Just fn }


tooltip :
    String
    -> Column ctx record msg
    -> Column ctx record msg
tooltip str c =
    { c | tooltip = Just str }


{-| SIMPLE Column.date
-}
date :
    String
    -> Maybe Length
    -> (record -> Time.Posix)
    -> Column { ctx | tz : Time.Zone } record msg
date title_ startingWidth toPosix =
    date_
        title_
        startingWidth
        toPosix
        (toPosix >> formatDate >> E.text)


dateTime_ : String -> Maybe Length -> (record -> Time.Posix) -> Column { ctx | tz : Time.Zone } record msg
dateTime_ title_ startingWidth toPosix =
    date_
        title_
        startingWidth
        toPosix
        (toPosix >> format)


dateTime :
    String
    -> Maybe Length
    -> (record -> Time.Posix)
    -> Column { ctx | tz : Time.Zone } record msg
dateTime title_ startingWidth toPosix =
    let
        dateTime__ : Time.Posix -> Element { ctx | tz : Time.Zone } msg
        dateTime__ time =
            let
                dateTimeFormat : Time.Zone -> String
                dateTimeFormat timezone =
                    -- e.g. 2020-05-13 04:17 PM
                    Strftime.format "%Y-%m-%d %H:%M %p" timezone time
            in
            E.with .tz (dateTimeFormat >> E.text)
    in
    date_
        title_
        startingWidth
        toPosix
        (toPosix >> dateTime__)


date_ :
    String
    -> Maybe Length
    -> (record -> Time.Posix)
    -> (record -> Element { ctx | tz : Time.Zone } msg)
    -> Column { ctx | tz : Time.Zone } record msg
date_ title_ startingWidth toPosix render =
    { title = title_
    , startingWidth = startingWidth
    , sort = sorter title_ (ap (toPosix >> Time.posixToMillis))
    , body = render
    , toStr = toPosix >> formatDate
    , filter = Nothing
    , encode = toPosix >> formatDate >> Encode.string
    , tooltip = Nothing
    }


int :
    String
    -> Maybe Length
    -> (record -> Int)
    -> Column ctx record msg
int title_ startingWidth toI =
    int_
        title_
        startingWidth
        toI
        (toI >> String.fromInt >> text)


int_ :
    String
    -> Maybe Length
    -> (record -> Int)
    -> (record -> Element ctx msg)
    -> Column ctx record msg
int_ title_ startingWidth toI render =
    { title = title_
    , startingWidth = startingWidth
    , sort = sorter title_ (ap toI)
    , body = render
    , toStr = toI >> String.fromInt
    , filter = Nothing
    , encode = toI >> Encode.int
    , tooltip = Nothing
    }


float :
    String
    -> Maybe Length
    -> (record -> Float)
    -> Column ctx record msg
float title_ startingWidth toF =
    { title = title_
    , startingWidth = startingWidth
    , sort = sorter title_ (ap toF)
    , body = toF >> String.fromFloat >> text
    , toStr = toF >> String.fromFloat
    , filter = Nothing
    , encode = toF >> Encode.float
    , tooltip = Nothing
    }


{-| SIPMLE Column.string
-}
string :
    String
    -> Maybe Length
    -> (record -> String)
    -> Column ctx record msg
string title_ startingWidth toComparable =
    string_
        title_
        startingWidth
        toComparable
        (toComparable >> text)
        (toComparable >> Encode.string)


ellipsis : String -> Maybe Length -> (record -> String) -> Column ctx record msg
ellipsis title_ startingWidth toComparable =
    let
        ellipsis_ : String -> Element ctx msg
        ellipsis_ str =
            let
                elmUiElement_text_classes : String
                elmUiElement_text_classes =
                    "s t wf hf"
            in
            H.span
                [ Attr.style "text-overflow" "ellipsis"
                , Attr.style "overflow" "hidden"
                , Attr.style "width" "100%"
                , Attr.class elmUiElement_text_classes
                , Attr.style "line-height" "1.1"
                ]
                [ H.text str ]
                |> E.html
    in
    string_
        title_
        startingWidth
        toComparable
        (toComparable >> ellipsis_)
        (toComparable >> Encode.string)


string_ :
    String
    -> Maybe Length
    -> (record -> String)
    -> (record -> Element ctx msg)
    -> (record -> Encode.Value)
    -> Column ctx record msg
string_ title_ startingWidth toComparable render encoder =
    { title = title_
    , startingWidth = startingWidth
    , sort = sorter title_ (ap (toComparable >> String.toLower))
    , body = render
    , toStr = toComparable
    , filter = Nothing
    , encode = encoder
    , tooltip = Nothing
    }


custom :
    String
    -> Maybe Length
    -> (record -> Element ctx msg)
    -> Column ctx record msg
custom title_ startingWidth render =
    custom_ title_ startingWidth render (\_ -> Encode.null)



-- withSort


custom_ :
    String
    -> Maybe Length
    -> (record -> Element ctx msg)
    -> (record -> Encode.Value)
    -> Column ctx record msg
custom_ title_ startingWidth render encoder =
    { title = title_
    , startingWidth = startingWidth
    , sort = Unsorted
    , body = render
    , toStr = always ""
    , filter = Nothing
    , encode = encoder
    , tooltip = Nothing
    }


withSort : (record -> comparable) -> Column ctx record msg -> Column ctx record msg
withSort toComparable column =
    { column
        | sort = sorter column.title (ap toComparable)
    }



-- Sort helpers


type Sorter record
    = Sorted String (List record -> List record)
    | Unsorted


sorter : String -> (record -> record -> Order) -> Sorter record
sorter k fn =
    Sorted k (List.sortWith fn)


ap : (a -> comparable) -> a -> a -> Order
ap toComp a b =
    compare (toComp a) (toComp b)
