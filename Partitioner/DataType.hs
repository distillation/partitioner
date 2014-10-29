module Partitioner.DataType(generateFlattenedDataTypes) where

import Language.Core
import Data.List(nub)

generateFlattenedDataTypes :: [DataType] -> [DataType]
generateFlattenedDataTypes = map generateParallelDataTypes'

generateParallelDataTypes' :: DataType -> DataType
generateParallelDataTypes' (DataType name tvars cons context derives) = DataType (name ++ "Par") [] (generateParallelConstructors name cons) context derives

generateParallelConstructors :: String -> [DataCon] -> [DataCon]
generateParallelConstructors tname = map (generateParallelConstructor tname)

generateParallelConstructor :: String -> DataCon -> DataCon
generateParallelConstructor tname (cname, ctypes) = (cname ++ "Par", filter (\(DataType n _ _ _ _) -> n /= tname) ctypes)