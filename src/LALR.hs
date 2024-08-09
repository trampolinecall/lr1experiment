{-# LANGUAGE PatternSynonyms #-}

module LALR (LALRItem (..), new_item, new_item_with_index_0, generate) where

import Data.Function ((&))
import qualified Data.List as List (findIndex)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (Set)
import qualified Data.Set as Set

import FirstAndFollowSets (find_firsts, find_follows)
import Grammar (Grammar, Rule, pattern Rule)
import qualified Grammar
import Item (Item)
import qualified Item
import ItemSet (ItemSet (ItemSet))
import qualified LR0
import StateTable (Action (..), StateTable)
import qualified StateTable.Generation
import Symbols (Terminal (..))

data LALRItem = LALRItem Rule Int Terminal deriving (Show, Eq, Ord)
new_item :: Rule -> Int -> Terminal -> Maybe LALRItem
new_item r@(Rule _ _ production) i l
    | i > length production = Nothing
    | otherwise = Just $ LALRItem r i l
new_item_with_index_0 :: Rule -> Terminal -> LALRItem
new_item_with_index_0 r l = LALRItem r 0 l

instance Item LALRItem where
    rule (LALRItem r _ _) = r
    index (LALRItem _ i _) = i

    sym_after_dot (LALRItem (Rule _ _ production) index _)
        | index < length production = Just (production !! index)
        | otherwise = Nothing

    can_move_forward (LALRItem (Rule _ _ r_production) i _) = i <= length (r_production)
    move_forward (LALRItem r i l) = new_item r (i + 1) l

    find_closure grammar kernel =
        lr0_closure
            & Set.map (\(LR0.LR0Item rule@(Rule _ nt _) index) -> follow_sets Map.! nt & Set.map (\lookahead -> LALRItem rule index lookahead))
            & Set.unions
        where
            lr0_closure =
                kernel
                    & Set.map (\(LALRItem ru i _) -> fromJust $ LR0.new_item ru i) -- fromJust is safe because the item already exists as an LALR item so the index must be valid
                    & Item.find_closure grammar
            first_sets = find_firsts grammar
            follow_sets =
                find_follows
                    ( kernel
                        & Set.map (\(LALRItem (Rule _ nt _) _ lookahead) -> Map.singleton nt (Set.singleton lookahead))
                        & Map.unionsWith (<>)
                    )
                    ((kernel & Set.map Item.rule & Set.toList) <> (lr0_closure & Set.map Item.rule & Set.toList))
                    first_sets

generate :: Grammar -> StateTable LALRItem ()
generate grammar =
    StateTable.Generation.generate
        (\rule -> new_item_with_index_0 rule EOF)
        lalr_intern
        ( \(LALRItem rule@(Rule _ r_nt _) _ lookahead) ->
            if r_nt == Grammar.augment_nt grammar
                then Map.singleton lookahead Accept
                else Map.singleton lookahead (Reduce rule)
        )
        grammar

lalr_intern :: Set LALRItem -> Set LALRItem -> [ItemSet LALRItem] -> ((Int, Bool), [ItemSet LALRItem])
lalr_intern kernel closure sets =
    case List.findIndex
        (\(ItemSet _ f_kernel f_closure) -> Set.map remove_lookaheads (f_kernel <> f_closure) == Set.map remove_lookaheads (kernel <> closure))
        sets of
        Just found_set_index ->
            ((found_set_index, False), modify_at found_set_index (merge kernel closure) sets)
        Nothing ->
            let new_item_set = ItemSet (length sets) kernel closure
            in (((length sets), True), (sets ++ [new_item_set]))
    where
        remove_lookaheads (LALRItem rule ind _) = (rule, ind)

        modify_at :: Int -> (a -> a) -> [a] -> [a]
        modify_at n _ [] = error $ "cannot modify at index " ++ show n ++ " in list of length 0"
        modify_at 0 change (first : more) = change first : more
        modify_at n change l@(first : more)
            | n >= length l = error $ "cannot modify at index " ++ show n ++ " in list of length " ++ show (length l)
            | otherwise = first : (modify_at (n - 1) change more)

        merge kernel closure (ItemSet number already_kernel already_closure) = ItemSet number (Set.union already_kernel kernel) (Set.union already_closure closure)
