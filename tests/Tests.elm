module Tests exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)
import Tuple


-- Module Under Test

import Stack exposing (Stack)


testSuite : Test
testSuite =
    describe "the tests for verifying Stack behavior"
        [ describe "explore the exposed functions of Stack"
            [ test "an empty stack can be created" <|
                \() ->
                    (Stack.empty == Stack.empty)
                        |> Expect.true ("two empty Stacks should be equal")
            , test "is empty indicated for empty Stack" <|
                \() ->
                    (Stack.isEmpty Stack.empty)
                        |> Expect.true ("expect `True` for isEmpty of an empty Stack")
            , test "popping an empty stack returns Nothing" <|
                \() ->
                    (Stack.pop Stack.empty)
                        |> Tuple.first
                        |> Expect.equal Nothing
            , test "what we `push` is on `top" <|
                \() ->
                    (Stack.push "Lebowksi" Stack.empty)
                        |> Stack.top
                        |> Expect.equal (Just "Lebowksi")
            , test "depth increases by 1 when a value is pushed" <|
                \() ->
                    (Stack.push "Lebowksi" Stack.empty)
                        |> Stack.depth
                        |> Expect.equal 1
            , test "is empty false for a non-empty Stack" <|
                \() ->
                    (Stack.push "Lebowksi" Stack.empty)
                        |> Stack.isEmpty
                        |> Expect.false ("expect `False` for isEmpty of a non-empty Stack")
            , test "top returns the top element of a single element Stack" <|
                \() ->
                    (Stack.push "Lebowksi" Stack.empty)
                        |> Stack.top
                        |> Expect.equal (Just "Lebowksi")
            , test "push all the elements from a list onto a Stack" <|
                \() ->
                    [ "The Dude", "Walter", "Donny", "Maude", "Nihilist 1" ]
                        |> List.foldr (\c -> Stack.push c) Stack.empty
                        |> Stack.depth
                        |> Expect.equal (List.length [ "The Dude", "Walter", "Donny", "Maude", "Nihilist 1" ])
            , test "last item in the list is returned on `pop`" <|
                \() ->
                    [ "The Dude", "Walter", "Donny", "Maude", "Nihilist 1" ]
                        |> List.foldr (\c -> Stack.push c) Stack.empty
                        |> Stack.pop
                        |> Tuple.first
                        |> Expect.equal (Just "The Dude")
            ]
        ]
