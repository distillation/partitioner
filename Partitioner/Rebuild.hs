module Partitioner.Rebuild(generateRebuildingFunctions) where
    
import Language.Core
import Data.List(partition, sortBy)
import Data.Function(on)

generateRebuildingFunctions :: [DataType] -> [Function]
generateRebuildingFunctions ds = unpartitionFunction : map generateRebuildingFunction ds ++ map generateUnflatteningFunction ds

unpartitionFunction :: Function
unpartitionFunction = ("unpartition", Lambda "xs" (Case (Bound 0) [Branch "Singleton" ["x"] (Con "ConsTransformer" [Bound 0, Con "NilTransformer" []]),
																   Branch "Join" ["l","r"] (Apply (Apply (Fun "(++)") (Apply (Fun "unpartition") (Bound 1))) (Apply (Fun "unpartition") (Bound 0)))]))

generateRebuildingFunction :: DataType -> Function
generateRebuildingFunction (DataType name _ _ _ _) = ("rebuild" ++ name, Lambda "xs" (TupleLet ["y","ys"] (Apply (Fun ("unflatten" ++ name)) (Apply (Fun "unpartition") (Bound 0))) (Bound 1)))

generateUnflatteningFunction :: DataType -> Function
generateUnflatteningFunction (DataType name _ cons _ _) = ("unflatten" ++ name, Lambda "xs" (Case (Bound 0) ([Branch "ConsTransformer" ["y", "ys"] (Case (Bound 1) (generateOriginalBranches name cons))])))

generateOriginalBranches :: String -> [DataCon] -> [Branch]
generateOriginalBranches name cons = map (generateOriginalBranch name) cons

generateOriginalBranch :: String -> DataCon -> Branch
generateOriginalBranch tname (cname, ctypes) = Branch (cname ++ "Par") args generateUnflatteningBranchExp
 where
     typeNameBinders = zip (map (\(DataType n _ _ _ _) -> n) ctypes) (reverse [0..(length ctypes) - 1])
     (inductiveBinders, normalBinders) = partition (\(n, _) -> tname == n) typeNameBinders
     args = reverse (map (\i -> "x" ++ take (snd i) ['\'','\''..]) normalBinders)
     
     -- Generate triple containing (type_name, original binder point, binding point relative to unflattening calls)
     updatedInductiveBinders = zipWith (\(t,b) i -> (t,b, 2 * i + 1)) inductiveBinders (reverse [0..(length inductiveBinders) - 1])
     updatedNormalBinders = zipWith (\(t,b) i -> (t,b, 2 * (length inductiveBinders) + i)) normalBinders (reverse [0..(length normalBinders) - 1])
     -- Generate sorted list of triples, sorted according to orignal binding point
     updatedBinders = map (\x -> Bound (thd3 x)) $ reverse $ (sortBy (compare `on` snd3) (updatedInductiveBinders ++ updatedNormalBinders))
     
     generateUnflatteningBranchExp
      | ctypes == [] = (Tuple [(Con cname []), Bound 0]) -- No arguments
      | inductiveBinders == [] = (Tuple [(Con cname updatedBinders), Bound (length updatedBinders)]) -- Only normal binders
      | otherwise = makeCons (length normalBinders) (reverse inductiveBinders) -- Inductive & normal binders
     
     makeCons _ [] = Tuple [Con cname updatedBinders, Bound 0]
     makeCons z [(_,i)] = TupleLet ["y" ++ pstring i, "ys" ++ pstring i] (Apply (Fun ("unflatten" ++ tname ++ "'")) (Bound z)) (Tuple [Con cname updatedBinders, Bound 0])
     makeCons z ((_,i):xs) = TupleLet ["y" ++ pstring i, "ys" ++ pstring i] (Apply (Fun ("unflatten" ++ tname ++ "'")) (Bound z)) (makeCons 0 xs)
 
     pstring n = take n ['\'','\''..]
     
     snd3 (_,b,_) = b
     thd3 (_,_,c) = c