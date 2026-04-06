open! Float_u

external eq_int8_u : int8# -> int8# -> bool = "%int8#_equal"
external eq_int16_u : int16# -> int16# -> bool = "%int16#_equal"
external of_int_u : int -> int# = "%int#_of_int"
external eq_int_u : int# -> int# -> bool = "%int#_equal"
external char_to_int8_u : char# -> int8# = "%identity"

let eq_char_u x y = eq_int8_u (char_to_int8_u x) (char_to_int8_u y)
