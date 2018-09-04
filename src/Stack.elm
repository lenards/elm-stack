module Stack exposing (Stack, empty, push, pop, top, depth, isEmpty)

{-| An implementation of a "last in, first out" (LIFO) data structure for elements of `t`.

Create a Stack by calling `Stack.empty`.

To add elements, `Stack.push val stack` or `Stack.push val Stack.empty`.

To remove elements, `Stack.pop stack`.

...


# Stack

@docs Stack


# Build

@docs empty, pop, push


# Query

@docs isEmpty, depth, top

-}


type alias StackImpl t =
    { data : List t }


type Stack t
    = Stack (StackImpl t)


unwrap : Stack t -> StackImpl t
unwrap stack =
    case stack of
        Stack impl ->
            impl


wrap : StackImpl t -> Stack t
wrap impl =
    Stack impl


empty : Stack t
empty =
    Stack { data = [] }


isEmpty : Stack t -> Bool
isEmpty stack =
    (depth stack) == 0


push : t -> Stack t -> Stack t
push val stack =
    let
        impl =
            unwrap stack
    in
        { impl | data = (val :: impl.data) } |> wrap


pop : Stack t -> ( Maybe t, Stack t )
pop stack =
    let
        impl =
            unwrap stack
    in
        case impl.data of
            val :: rest ->
                ( Just val, wrap <| { impl | data = rest } )

            [] ->
                ( Nothing, stack )


top : Stack t -> Maybe t
top stack =
    case (unwrap stack) |> .data of
        val :: rest ->
            Just val

        _ ->
            Nothing


depth : Stack t -> Int
depth stack =
    List.length ((unwrap stack) |> .data)
