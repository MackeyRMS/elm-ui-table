module Table exposing
    ( init, sort, preFilter, filtered, Table
    , View, lined, build
    , view
    , WithAbilities, attributes, headRowAttributes, bodyRowAttributes
    , withRowBorders, withStickyHeader
    , InfiniteListConf, RowHeight, WindowHeight, infinite
    , keep
    , records
    )

{-| Flexible helpers for constructing `Table`s


# Construct a Table from a List of records with an id key

@docs init, sort, preFilter, filtered, Table


# Prepare the View for a Table giving it a Msg constructor that takes a Table

@docs View, lined, build


# Takes a Table returns Html

@docs view


# Phantom helpers

@docs WithAbilities, attributes, headRowAttributes, bodyRowAttributes
@docs withRowBorders, withStickyHeader
@docs InfiniteListConf, RowHeight, WindowHeight, infinite


# Filtering

@docs keep


# Accessors

@docs records

-}

import Dict exposing (Dict)
import Element.WithContext as E exposing (Attribute, Element)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Events as Event
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import Element.WithContext.Region as Region
import Html
import Html.Attributes as Attr
import Icon
import InfiniteList
import Length exposing (Length)
import StringUtil
import Table.Column exposing (Column, Sorter(..))
import Time


type alias TLDR =
    { top : Int
    , left : Int
    , bottom : Int
    , right : Int
    }


edges : TLDR
edges =
    { top = 0
    , left = 0
    , bottom = 0
    , right = 0
    }



-- Raw API
-- NOTE: If we do end up needing something like this, consider using extensible records?
-- type alias IxColumn record msg =
--     { header : Element ctx msg
--     , width : Length
--     , sorter : Sorter record
--     , view : Int -> record -> Element ctx msg
--     }


{-| -}
init : List r -> Table r
init ls =
    Table
        { sortedColumn = Unsorted
        , records = ls
        , ord = Asc
        , filters = Dict.empty
        , infiniteList = InfiniteList.init
        }


{-| -}
sort : Column ctx record msg -> Table record -> Table record
sort c (Table i) =
    Table { i | sortedColumn = c.sort }


{-| -}
records : Table record -> List record
records (Table t) =
    t.records


{-| -}
filtered : Maybe (record -> Bool) -> List (Column ctx record msg) -> Table record -> List record
filtered filterFn columns (Table table) =
    let
        columnFilterFns_ : List (record -> Bool)
        columnFilterFns_ =
            columns
                |> List.filterMap
                    (\c ->
                        Maybe.andThen
                            (\fn ->
                                Maybe.map
                                    (\text ->
                                        \rec -> fn text (c.toStr rec)
                                    )
                                    (Dict.get c.title table.filters)
                            )
                            c.filter
                    )

        columnFilterFns : List (record -> Bool)
        columnFilterFns =
            Maybe.withDefault (always True) filterFn
                :: columnFilterFns_

        columnFilters : record -> Bool
        columnFilters record =
            List.all ((|>) record) columnFilterFns

        filter : List record -> List record
        filter =
            if List.length columnFilterFns > 0 then
                List.filter columnFilters

            else
                identity
    in
    case ( table.sortedColumn, table.ord ) of
        ( Sorted _ sort_, Asc ) ->
            table.records
                |> filter
                |> sort_

        ( Sorted _ sort_, Desc ) ->
            table.records
                |> filter
                |> sort_
                |> List.reverse

        ( Unsorted, _ ) ->
            table.records
                |> filter


{-| -}
type Table record
    = Table (Internal record)


type alias Internal record =
    { sortedColumn : Sorter record
    , records : List record
    , ord : Ord
    , filters : Dict String String

    -- InfiniteList Props
    , infiniteList : InfiniteList.Model
    }


{-| -}
preFilter : Column ctx r msg -> String -> Table r -> Table r
preFilter c string (Table i) =
    Table { i | filters = Dict.insert c.title string i.filters }



-- TODO: Incorporate loading support for Async data.
-- type Data r
--     = Synchronous (List r)
--     | Asynchronous (Async r)


{-| -}
type alias WithAbilities =
    { can_add_table_attributes : ()
    , can_add_header_row_attributes : ()
    , can_add_body_row_attributes : ()
    , can_add_row_borders : ()
    }


{-| -}
type View c ctx record msg
    = View (InternalView ctx record msg)


type alias InternalView ctx record msg =
    { idPrefix : String
    , attributes_ : List (Attribute ctx msg)
    , headRowAttributes_ : List (Attribute ctx msg)
    , bodyRowAttributes_ : { isFirstRow : Bool } -> record -> List (Attribute ctx msg)
    , table : Internal record
    , toMsg : Table record -> msg
    , columns : List (Column ctx record msg)
    , sticky : Bool
    , infinite_ : Maybe InfiniteListConf
    , filter : Maybe (record -> Bool)
    }


{-| -}
type alias InfiniteListConf =
    { rowHeight : RowHeight
    , containerHeight : Maybe WindowHeight
    , howManyLengthsBelowTheFold : Int
    }


{-| -}
type alias RowHeight =
    Length


{-| -}
type alias WindowHeight =
    Length



{--Optional functions for overriding each part of a tables attributes
can only be called once to avoid allowing callers to accidentally stack overrides.

If you need to be able to stack you're overrides use the fact that Attributes are a list
and elm-ui uses a last wins strategy.

[Background.color blue, Background.color red] => Means the element will ultimately render with a red background
So
[Background.color blue] ++ [Background.color red] will do the same.
--}


{-| -}
attributes :
    List (Attribute ctx msg)
    -> View { c | can_add_table_attributes : () } ctx record msg
    -> View c ctx record msg
attributes ls (View v) =
    View { v | attributes_ = v.attributes_ ++ ls }


{-| -}
headRowAttributes :
    List (Attribute ctx msg)
    -> View { c | can_add_header_row_attributes : () } ctx record msg
    -> View c ctx record msg
headRowAttributes ls (View v) =
    View { v | headRowAttributes_ = v.headRowAttributes_ ++ ls }


{-| -}
bodyRowAttributes :
    (record -> List (Attribute ctx msg))
    -> View { c | can_add_body_row_attributes : () } ctx record msg
    -> View c ctx record msg
bodyRowAttributes ls (View v) =
    View { v | bodyRowAttributes_ = \first r -> v.bodyRowAttributes_ first r ++ ls r }


{-| -}
withStickyHeader : View c ctx record msg -> View c ctx record msg
withStickyHeader (View v) =
    View { v | sticky = True }


{-| -}
infinite : InfiniteListConf -> View c ctx record msg -> View c ctx record msg
infinite conf (View v) =
    View { v | infinite_ = Just conf }


{-| -}
keep : (r -> Bool) -> View c ctx r msg -> View c ctx r msg
keep pred (View v) =
    View { v | filter = Just pred }


{-| -}
withRowBorders :
    View { c | can_add_row_borders : () } ctx record msg
    -> View c ctx record msg
withRowBorders (View v) =
    View
        { v
            | headRowAttributes_ =
                Border.widthEach { edges | bottom = xsmall }
                    :: Border.solid
                    :: Border.color grayValue0
                    :: v.headRowAttributes_
            , bodyRowAttributes_ = rowBorderBodyAttrs v
        }


xxsmall : Int
xxsmall =
    remToPx 0.0675


xsmall : Int
xsmall =
    remToPx 0.125


small : Int
small =
    remToPx 0.25


medium : Int
medium =
    remToPx 0.375


large : Int
large =
    remToPx 0.75


transparent : E.Color
transparent =
    E.rgba255 0 0 0 0


blackValue : E.Color
blackValue =
    E.rgba255 17 17 17 1


grayValue0 : E.Color
grayValue0 =
    E.rgba255 103 103 103 1


grayValue1 : E.Color
grayValue1 =
    E.rgba255 144 144 144 1


grayValue2 : E.Color
grayValue2 =
    E.rgba255 196 196 196 1


grayValue3 : E.Color
grayValue3 =
    E.rgba255 217 217 217 1


grayValue4 : E.Color
grayValue4 =
    E.rgba255 238 238 238 1


whiteValue : E.Color
whiteValue =
    E.rgba255 255 255 255 1


rowBorderBodyAttrs : InternalView ctx record msg -> { isFirstRow : Bool } -> record -> List (Attribute ctx msg)
rowBorderBodyAttrs v ({ isFirstRow } as fst) r =
    if isFirstRow then
        v.bodyRowAttributes_ fst r

    else
        Border.widthEach { edges | top = 1 }
            :: Border.solid
            :: Border.color grayValue2
            :: v.bodyRowAttributes_ fst r


tooltip : (Element ctx msg -> Attribute ctx msg) -> String -> Attribute ctx msg
tooltip position s =
    E.inFront <|
        E.el
            [ E.width E.fill
            , E.height E.fill
            , E.transparent True
            , E.mouseOver [ E.transparent False ]
            , E.htmlAttribute (Attr.style "transition" "opacity 100ms 500ms linear")
            , E.htmlAttribute (Attr.style "z-index" "999")
            , (position << E.map never) <|
                E.el [ E.htmlAttribute (Attr.style "pointer-events" "none") ]
                    (E.el
                        [ Background.color grayValue1
                        , Font.color whiteValue
                        , E.padding medium
                        , Border.rounded small
                        , Font.size 12
                        ]
                        (E.text s)
                    )
            ]
            E.none


{-| -}
view : View c ctx record msg -> Element ctx msg
view (View ({ table, columns } as v)) =
    let
        return : Element ctx msg
        return =
            E.column
                (if v.sticky then
                    E.height E.fill
                        :: v.attributes_

                 else
                    v.attributes_
                )
                (header
                    :: (if v.sticky then
                            [ E.column
                                [ E.scrollbarY
                                , E.height E.fill
                                , E.width E.fill
                                ]
                                body
                            ]

                        else
                            body
                       )
                )

        header : Element ctx msg
        header =
            E.row v.headRowAttributes_
                (List.map headerCell columns)

        headerCell : Column ctx record msg -> Element ctx msg
        headerCell c =
            (if c.sort == Unsorted then
                E.text c.title

             else
                let
                    click : Sorter record -> Ord -> Attribute ctx msg
                    click m o =
                        Table { table | sortedColumn = m, ord = o }
                            |> v.toMsg
                            |> Event.onClick
                in
                if sameKey c.sort table.sortedColumn && table.ord == Asc then
                    E.row
                        [ E.width E.fill
                        ]
                        [ renderFilter v c table
                        , Icon.caret__Up
                            |> Icon.new
                            |> Icon.toHtml
                            |> E.html
                            |> E.el
                                [ E.centerX
                                , E.centerY
                                , E.moveUp 5
                                , E.height (E.px large)
                                ]
                            |> E.el
                                [ E.height (E.px (remToPx 1.7))
                                , E.pointer
                                , Border.color transparent
                                , Border.width xxsmall
                                , Border.rounded medium
                                , Font.color blackValue
                                , E.mouseOver
                                    [ Background.color grayValue3
                                    , Border.color grayValue3
                                    ]
                                , click c.sort Desc
                                ]
                        ]

                else if sameKey c.sort table.sortedColumn && table.ord == Desc then
                    E.row
                        [ E.width E.fill
                        ]
                        [ renderFilter v c table
                        , Icon.caret__Down
                            |> Icon.new
                            |> Icon.toHtml
                            |> E.html
                            |> E.el
                                [ E.centerX
                                , E.centerY
                                , E.moveDown 5
                                , E.height (E.px large)
                                ]
                            |> E.el
                                [ E.height (E.px (remToPx 1.7))
                                , E.pointer
                                , Border.color transparent
                                , Border.width xxsmall
                                , Border.rounded medium
                                , Font.color blackValue
                                , E.mouseOver
                                    [ Background.color grayValue3
                                    , Border.color grayValue3
                                    ]
                                , click Unsorted Desc
                                ]
                        ]

                else
                    E.row
                        [ E.width E.fill
                        ]
                        [ renderFilter v c table
                        , Icon.caret__UpAndDown
                            |> Icon.new
                            |> Icon.toHtml
                            |> E.html
                            |> E.el
                                [ E.centerX
                                , E.centerY
                                ]
                            |> E.el
                                [ E.height (E.px (remToPx 1.7))
                                , E.pointer
                                , Border.color transparent
                                , Border.width xxsmall
                                , Border.rounded medium
                                , Font.color grayValue1
                                , E.mouseOver
                                    [ Background.color grayValue3
                                    , Border.color grayValue3
                                    , Font.color blackValue
                                    ]
                                , click c.sort Asc
                                ]
                        ]
            )
                |> E.el
                    ([ Just (E.htmlAttribute (Attr.class "min-width-0"))
                     , Maybe.map E.width c.startingWidth
                     , Maybe.map (tooltip E.below) c.tooltip
                     ]
                        |> List.filterMap identity
                    )

        filtered_ : List record
        filtered_ =
            filtered v.filter columns (Table table)

        body : List (Element ctx msg)
        body =
            if filtered_ |> List.isEmpty then
                [ E.el
                    [ E.width E.fill
                    , E.padding large
                    ]
                  <|
                    E.el [ E.centerX ] <|
                        E.text "No Results"
                ]

            else
                case v.infinite_ of
                    Just conf ->
                        let
                            infListOverrides : Element ctx msg
                            infListOverrides =
                                -- wrap the style node in a div to prevent `Dark Reader` from blowin up the dom.
                                Html.span
                                    [ Attr.attribute "style" "display: none;"
                                    , Attr.attribute "class" "elm-css-style-wrapper"
                                    ]
                                    [ Html.node "style"
                                        []
                                        [ Html.text
                                            ("#"
                                                ++ infListContainerId
                                                ++ " { flex-shrink: 0 }"
                                            )
                                        ]
                                    ]
                                    |> E.html
                        in
                        [ E.column
                            [ E.height (E.fill |> E.minimum 0)
                            , E.width (E.fill |> E.minimum 0)
                            , E.clipX
                            , E.scrollbarY
                            , E.htmlAttribute
                                (InfiniteList.onScroll
                                    (\newInfList ->
                                        { table | infiniteList = newInfList }
                                            |> Table
                                            |> v.toMsg
                                    )
                                )
                            ]
                            [ infListOverrides
                            , E.with identity
                                (\ctx ->
                                    InfiniteList.view (bodyConf conf ctx) table.infiniteList filtered_
                                        |> E.html
                                )
                            ]
                        ]

                    Nothing ->
                        filtered_
                            |> List.map (bodyRow Nothing)

        infListContainerId : String
        infListContainerId =
            v.idPrefix ++ "-inf-container"

        bodyConf : InfiniteListConf -> ctx -> InfiniteList.Config record msg
        bodyConf { rowHeight, containerHeight, howManyLengthsBelowTheFold } ctx =
            let
                result : InfiniteList.Config record msg
                result =
                    InfiniteList.config
                        { itemView =
                            -- TODO: Can/Should this be Html.Styled.Keyed on (htmlIdx, listIdx)?
                            -- \ _ _ -> : htmlIdx -> listIdx -> record -> Element Msg
                            \_ _ ->
                                bodyRow (Just rowHeight) >> toUnstyled ctx
                        , itemHeight = InfiniteList.withConstantHeight (round <| Length.inCssPixels rowHeight)
                        , containerHeight = windowHeight
                        }
                        -- Render double the window so fast scrolling isn't glitchy
                        |> InfiniteList.withOffset (windowHeight * howManyLengthsBelowTheFold)
                        |> InfiniteList.withId infListContainerId

                windowHeight : Int
                windowHeight =
                    case containerHeight of
                        Just x ->
                            Length.inCssPixels x |> round

                        Nothing ->
                            1080
            in
            result

        bodyRow : Maybe RowHeight -> record -> Element ctx msg
        bodyRow rowHeight record =
            let
                isFirstRow : Bool
                isFirstRow =
                    case List.head filtered_ of
                        Just x ->
                            x == record

                        Nothing ->
                            False
            in
            E.row
                (v.bodyRowAttributes_ { isFirstRow = isFirstRow } record
                    |> (case rowHeight of
                            Just x ->
                                Length.inCssPixels x |> round |> E.px |> E.height |> (::)

                            Nothing ->
                                identity
                       )
                )
                (List.map (bodyCell record) columns)

        bodyCell : rec -> Column ctx rec msg -> Element ctx msg
        bodyCell record c =
            E.el
                -- bodyCellAttrs
                (E.htmlAttribute (Attr.class "min-width-0")
                    :: (case c.startingWidth of
                            Just x ->
                                [ E.width x ]

                            Nothing ->
                                []
                       )
                )
                (c.body record)
    in
    return


toUnstyled : ctx -> Element ctx msg -> Html.Html msg
toUnstyled ctx =
    let
        fontFamily : String
        fontFamily =
            """-apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, Oxygen-Sans, Ubuntu, Cantarell, "Helvetica Neue", Arial, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol"
                """
    in
    E.layoutWith ctx
        { options = [ E.noStaticStyleSheet ] }
        [ Font.family [ Font.typeface fontFamily ]
        , Font.regular

        -- , E.withAttribute .defaultFontSize fontSize
        , E.height (E.fill |> E.minimum 0)
        , E.width E.fill
        ]


renderFilter : InternalView ctx rec msg -> Column ctx rec msg -> Internal rec -> Element ctx msg
renderFilter v c table =
    case c.filter of
        Just _ ->
            renderFilter_ v c table

        Nothing ->
            E.el [ E.width E.fill ] (E.text c.title)


renderFilter_ : InternalView ctx rec msg -> Column ctx rec msg -> Internal rec -> Element ctx msg
renderFilter_ ({ toMsg } as v) c table =
    let
        padding : Int
        padding =
            medium

        iconWidth : Int
        iconWidth =
            remToPx 1

        searchIconOffset : Int
        searchIconOffset =
            padding + iconWidth

        filterText : Maybe String
        filterText =
            Dict.get c.title table.filters

        readonly : Bool
        readonly =
            filterText == Nothing

        idPrefix : String
        idPrefix =
            v.idPrefix ++ StringUtil.dasherize c.title

        searchIconId : String -> String
        searchIconId postFix =
            idPrefix ++ "-filter" ++ postFix
    in
    Input.search
        (E.htmlAttribute (Attr.style "margin" "0")
            :: E.htmlAttribute (Attr.id (searchIconId "-input"))
            :: E.width E.fill
            :: Border.widthEach { edges | bottom = xxsmall }
            :: Border.color
                (if Maybe.map (not << String.isEmpty) filterText == Just True then
                    blackValue

                 else
                    transparent
                )
            :: Border.rounded 0
            :: E.htmlAttribute (Attr.readonly readonly)
            :: E.centerY
            :: Background.color transparent
            :: E.paddingEach
                { top = padding
                , right = searchIconOffset
                , bottom = padding
                , left = 0
                }
            :: E.spacing 0
            :: E.inFront
                (E.el
                    [ E.height E.fill
                    , E.width (E.px searchIconOffset)
                    , E.alignRight
                    ]
                    (if readonly then
                        Input.button
                            [ Region.description ("Filter by " ++ c.title)
                            , E.height E.fill
                            , E.width E.fill
                            , Border.color transparent
                            , Border.width xxsmall
                            , Border.rounded medium
                            , E.mouseOver
                                [ Background.color grayValue3
                                , Border.color grayValue3
                                ]
                            , E.focused
                                [ Background.color grayValue3
                                , Border.color grayValue3
                                ]
                            , E.htmlAttribute (Attr.id (searchIconId "-search"))
                            ]
                            { onPress = Just ({ table | filters = Dict.insert c.title "" table.filters } |> Table |> toMsg)
                            , label =
                                Icon.magnifyingGlass
                                    |> Icon.new
                                    --"black"
                                    |> Icon.inColor "#111"
                                    |> Icon.toHtml
                                    |> E.html
                                    |> E.el
                                        [ E.centerY
                                        , E.centerX
                                        , E.height (E.px large)
                                        ]
                            }

                     else
                        Input.button
                            [ E.width E.fill
                            , E.height E.fill
                            , Region.description ("Clear " ++ c.title ++ " filter")
                            , Border.color transparent
                            , Border.width xxsmall
                            , Border.rounded medium
                            , E.focused
                                [ Background.color grayValue3
                                , Border.color grayValue3
                                ]
                            , E.mouseOver
                                [ Background.color grayValue3
                                , Border.color grayValue3
                                ]
                            , E.htmlAttribute (Attr.id (searchIconId "-clear"))
                            ]
                            { onPress = Just ({ table | filters = Dict.remove c.title table.filters } |> Table |> toMsg)
                            , label =
                                E.el [ E.centerY, E.centerX ]
                                    (E.text "×")
                            }
                    )
                )
            :: (if readonly then
                    [ E.focused [] ]

                else
                    (if filterText == Just "" then
                        \ls ->
                            Event.onLoseFocus
                                ({ table | filters = Dict.remove c.title table.filters }
                                    |> Table
                                    |> toMsg
                                )
                                :: ls

                     else
                        identity
                    )
                        [ Font.regular
                        , E.focused
                            [ Border.color blackValue ]
                        ]
               )
        )
        { onChange =
            \txt ->
                { table | filters = Dict.insert c.title txt table.filters }
                    |> Table
                    |> toMsg
        , text = Dict.get c.title table.filters |> Maybe.withDefault c.title
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.size 12
                , E.transparent readonly

                -- , Font.regular
                , E.height (E.px 0)
                , E.moveUp (toFloat (padding + 2))
                ]
                (E.text c.title)
        }


sameKey : Sorter r -> Sorter r -> Bool
sameKey s1 s2 =
    case ( s1, s2 ) of
        ( Sorted k1 _, Sorted k2 _ ) ->
            k1 == k2

        _ ->
            False


{-| Make an elm-ui based table with sane defaults for a common use case
-}
lined :
    { idPrefix : String }
    -> (Table record -> msg)
    -> Table record
    -> List (Column ctx record msg)
    ->
        View
            { can_add_body_row_attributes : ()
            , can_add_header_row_attributes : ()
            , can_add_table_attributes : ()
            }
            ctx
            record
            msg
lined id toMsg table columns =
    linedHelp id toMsg table columns
        |> withRowBorders


linedHelp :
    { idPrefix : String }
    -> (Table record -> msg)
    -> Table record
    -> List (Column ctx record msg)
    -> View c ctx record msg
linedHelp { idPrefix } toMsg (Table table) columns =
    View
        { idPrefix = idPrefix
        , attributes_ = []
        , headRowAttributes_ =
            E.centerY
                :: Background.color grayValue4
                :: Font.bold
                :: gaps
        , bodyRowAttributes_ =
            \_ _ -> gaps
        , table = table
        , toMsg = toMsg
        , columns = columns
        , sticky = False
        , infinite_ = Nothing
        , filter = Nothing
        }


{-| Make a raw elm-ui native table
-}
build :
    { idPrefix : String }
    -> (Table record -> msg)
    -> Table record
    -> List (Column ctx record msg)
    -> View WithAbilities ctx record msg
build { idPrefix } toMsg (Table table) columns =
    let
        baseAttrs : List (Attribute ctx msg)
        baseAttrs =
            []
    in
    View
        { idPrefix = idPrefix
        , attributes_ = baseAttrs
        , headRowAttributes_ = baseAttrs
        , bodyRowAttributes_ = \_ _ -> baseAttrs
        , table = table
        , toMsg = toMsg
        , columns = columns
        , sticky = False
        , infinite_ = Nothing
        , filter = Nothing
        }


type Ord
    = Asc
    | Desc



{--Internal Helper Functions
--}


remToPx : Float -> Int
remToPx rem =
    round (rem * 16)


gaps : List (Attribute ctx msg)
gaps =
    let
        cssLineHeight : Float
        cssLineHeight =
            1.5

        padPlus : Int
        padPlus =
            cssLineHeight
                * (medium |> toFloat)
                |> round
    in
    [ E.padding padPlus
    , E.spacing padPlus
    ]
