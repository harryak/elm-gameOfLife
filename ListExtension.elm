module ListExtension (cartesianProduct) where

{-| Helpers to extend the functionality of the List module.

# Advanced map
@docs cartesianProduct
-}

import List

{-| Function to combine each element of first list with each element of the second one using the given function.

    cartesianProduct (,) [1,2] [3,4] == [(1,3), (1,4), (2,3), (2,4)]
-}
cartesianProduct : (a -> b -> result) -> List a -> List b -> List result
cartesianProduct f xs ys = List.foldr (\z zs -> List.append (List.map (f z) ys) zs) [] xs