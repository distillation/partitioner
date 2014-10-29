module Partitioner.Partition(generatePartitioningFunctions) where
    
import Language.Core
import Data.List(partition)

generatePartitioningFunctions :: [DataType] -> [Function]
generatePartitioningFunctions ds = partitionFunction : map generatePartitioningFunction ds ++ map generateConversionToFlatList ds

partitionFunction :: Function
partitionFunction = ("partition", Lambda "xs" (Case (Bound 0) [Branch "ConsTransformer" ["y","ys"] (Case (Bound 0) [Branch "NilTransformer" [] (Con "Singleton" [Bound 1]), Branch "ConsTransformer" ["z","zs"] (TupleLet ["l","r"] (Apply (Apply (Fun "splitAt") (Apply (Apply (Fun "div") (Apply (Fun "length") (Bound 4))) (Con "S" [Con "S" [Con "Z" []]]))) (Bound 4)) (Con "Join" [Apply (Fun "partition") (Bound 1), Apply (Fun "partition") (Bound 0)]))])]))

generatePartitioningFunction :: DataType -> Function
generatePartitioningFunction (DataType name _ _ _ _) = ("partition" ++ name, Lambda "xs" (Apply (Fun "partition") (Apply (Fun ("flatten" ++ name)) (Bound 0))))

generateConversionToFlatList :: DataType -> Function
generateConversionToFlatList (DataType name tvars cons _ _) = ("flatten" ++ name, Lambda "xs" (Case (Bound 0) (generateFlatteningBranches name cons)))

generateFlatteningBranches :: String -> [DataCon] -> [Branch]
generateFlatteningBranches tname cons = map (generateFlatteningBranch tname) cons

generateFlatteningBranch :: String -> DataCon -> Branch
generateFlatteningBranch tname (cname, ctypes) = Branch cname args (Con "ConsTransformer" flatConsList )
 where           
     numArgs = [0..(length ctypes) - 1]
     args = foldr (\i xs -> ("x" ++ take i ['\'','\''..]):xs) [] (numArgs)
     typeNameBinders = zip (map (\(DataType n _ _ _ _) -> n) ctypes) (reverse numArgs)
     (inductiveBinders, normalBinders) = partition (\(n,b) -> tname == n) typeNameBinders
     
     flatConsList
      | ctypes == [] = [Con (cname ++ "Par") []]
      | inductiveBinders == [] = [Con (cname ++ "Par") (map (Bound . snd) normalBinders)]
      | otherwise = [Con (cname ++ "Par") (map (Bound . snd) normalBinders), makeCons inductiveBinders]
                     
     makeCons [(n,i)] = Apply (Fun ("flatten" ++ n)) (Bound i)
     makeCons ((n,i):xs) = Apply (Apply (Fun "(++)") (Apply (Fun ("flatten" ++ n)) (Bound i))) (makeCons xs)