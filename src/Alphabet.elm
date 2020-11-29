module Alphabet exposing (..)

import Array

alphabet : List Char
alphabet = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z']

alphabetArray : Array.Array Char
alphabetArray = Array.fromList alphabet

alphabetLength : Int
alphabetLength = List.length alphabet

intToAlphabet : Int -> String
intToAlphabet n = 
  let m = modBy alphabetLength n
      d = n // alphabetLength
      l = Maybe.withDefault "" <| Maybe.map String.fromChar <| Array.get m alphabetArray
  in l ++ if d == 0 then "" else intToAlphabet d
