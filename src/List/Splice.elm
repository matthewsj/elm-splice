module List.Splice exposing (splice, Item(..))

{-| This library makes it easy to build a list from a mixture of items,
sublists, and `Maybe`s.

Literal list syntax can be frustrating when you're trying to construct lists
where you know much of the shape of the list you want to construct, but you
need to insert a variable number of items at certain points. This comes up a lot
when building lists of `Html` elements, for instance: You might know that you
want the body of a `div` to start with a standard header and end with a standard
footer but have a variable number of items in between, or you might want to
include a link inline if one is available but include nothing otherwise.

Using `splice` you can express these concepts directly. For instance, this code
produces a `div` that includes a link if one exists:

    describe : String -> Maybe String -> Html msg
    describe item maybeUrl =
        div []
            (splice
                [ I (text "The item in question is a ")
                , I (text item)
                , M (Maybe.map (\url -> a [ href url ] [ text "(link)" ]) maybeUrl)
                , I (text ".")
                ]
            )

@docs splice, Item

-}


{-| Specifications of shapes that can be spliced together in a splice list. See
documentation for `splice` for details.
-}
type Item t
    = I t
    | L (List t)
    | M (Maybe t)


{-| Splices together a list of `SpliceItem`s into a plain list.

`splice` allows you to stitch together a list where you want to insert variable
numbers of elements in arbitrary positions in the list. For instance:

    render items extraSpicy =
        div []
            (splice
                [ I (text "Here are the items:")
                , L (List.map renderItem items)
                , I (text "... and that's all!")
                , M
                    (if extraSpicy then
                        Just (text "That was extra spicy!")

                     else
                        Nothing
                    )
                ]
            )

Instead of specifying elements directly, you wrap each one according to its type:
`I` for literal items to include in the list, `L` for lists of items, and `M` for
maybe items.

-}
splice : List (Item t) -> List t
splice items =
    List.reverse (spliceInternal [] items)


{-| Accumulator-based helper for `splice`. Performs the splice, building up a
reversed version of the answer into `acc` and returning it when done.
-}
spliceInternal : List a -> List (Item a) -> List a
spliceInternal acc items =
    case items of
        [] ->
            acc

        (I item) :: rest ->
            spliceInternal (item :: acc) rest

        (L subitems) :: rest ->
            spliceInternal (reverseAppend subitems acc) rest

        (M maybeItem) :: rest ->
            case maybeItem of
                Just item ->
                    spliceInternal (item :: acc) rest

                Nothing ->
                    spliceInternal acc rest


{-| Appends a reversed copy of the first argument onto the second. For instance,

    reverseAppend [ 1, 2, 3 ] [ 4, 5 ]

evaluates to

    [ 3, 2, 1, 4, 5 ]

-}
reverseAppend : List a -> List a -> List a
reverseAppend listToReverse base =
    case listToReverse of
        [] ->
            base

        x :: rest ->
            reverseAppend rest (x :: base)
