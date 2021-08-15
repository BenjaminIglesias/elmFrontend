module CountingMaybe exposing (..)

type CountingMaybe a
    = CountingJust Int a
    | CountingNothing

map : (a -> b) -> CountingMaybe a -> CountingMaybe b
map action counted =
    case counted of
        CountingJust count value -> CountingJust (count + 1) (action value)
        CountingNothing -> CountingNothing

