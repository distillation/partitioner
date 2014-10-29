module Partitioner(
    addPartitioningToProgram,
    generateFlattenedDataTypes,
    generatePartitioningFunctions,
    generateRebuildingFunctions
) where

import Partitioner.DataType    
import Partitioner.Partition
import Partitioner.Rebuild
import Language.Core

addPartitioningToProgram :: Program -> Program
addPartitioningToProgram (Program (Where e fs) dataTypes mn pr wt es dls) = Program (Where e (fs ++ generatePartitioningFunctions dataTypes ++ generateRebuildingFunctions dataTypes)) (dataTypes ++ generateFlattenedDataTypes dataTypes) mn pr wt es dls
addPartitioningToProgram (Program e dataTypes mn pr wt es dls) = Program (Where e (generatePartitioningFunctions dataTypes ++ generateRebuildingFunctions dataTypes)) (dataTypes ++ generateFlattenedDataTypes dataTypes) mn pr wt es dls