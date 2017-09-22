{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module DWave.SAPI.Types where
import qualified Foreign.C.Types as C2HSImp
import qualified Foreign.Ptr as C2HSImp
import qualified Foreign.Storable as C2HSImp
import Data.Monoid
import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as T
import qualified Data.Map as M

#include "dwave_sapi.h"

{#context prefix = "sapi"#}
{#enum Code {underscoreToCase} deriving (Eq, Show)#}
{#enum SolverParameterAnswerMode {underscoreToCase} deriving (Eq, Show)#}
{#enum Postprocess {underscoreToCase} deriving (Eq, Show)#}

instance Storable SolverParameterAnswerMode where
  sizeOf = sizeOf . fromEnum
  alignment = alignment . fromEnum
  peek p = (return . toEnum) =<< peek (castPtr p)
  poke p x = poke (castPtr p) (fromEnum x)
  
instance Storable Postprocess where
  sizeOf = sizeOf . fromEnum
  alignment = alignment . fromEnum
  peek p = (return . toEnum) =<< peek (castPtr p)
  poke p x = poke (castPtr p) (fromEnum x)

C.include "<dwave_sapi.h>"

data AnnealOffsets = AnnealOffsets [Double]
data Connection -- we don't need to know about the token

data IsingResult = IsingResult {
  isolutions :: [([(Int, Int)], Double, Int)]
} deriving (Show)

data QuboResult = QuboResult {
  qsolutions :: [([(Int, Int)], Double, Int)]
} deriving (Show)

instance Storable IsingResult where
  sizeOf _ = {#sizeof IsingResult#}
  alignment _ = sizeOf nullPtr
  peek p = do
     solutionLen <- fromIntegral <$> {#get IsingResult->solution_len#} p
     numSolutions <- fromIntegral <$> {#get IsingResult->num_solutions#} p
     solutions :: [[CInt]] <- do
       sols <- {#get IsingResult->solutions#} p
       mapM (\i->peekArray solutionLen (plusPtr sols (i * solutionLen))) [0..numSolutions-1]
     energies <- map realToFrac <$> (peekArray numSolutions =<< {#get IsingResult->energies#} p)
     numOccurrences <- map fromIntegral <$> (peekArray numSolutions =<< {#get IsingResult->num_occurrences#} p)
     return $ IsingResult $ zip3 (filterIsing solutions) energies numOccurrences
  poke = undefined -- Don't expect to need to write results to memory

instance Storable QuboResult where
  sizeOf _ = {#sizeof IsingResult#}
  alignment _ = sizeOf nullPtr
  peek p = do
     solutionLen <- fromIntegral <$> {#get IsingResult->solution_len#} p
     numSolutions <- fromIntegral <$> {#get IsingResult->num_solutions#} p
     solutions :: [[CInt]] <- do
       sols <- {#get IsingResult->solutions#} p
       mapM (\i->peekArray solutionLen (plusPtr sols (i * solutionLen))) [0..numSolutions-1]
     energies <- map realToFrac <$> (peekArray numSolutions =<< {#get IsingResult->energies#} p)
     numOccurrences <- map fromIntegral <$> (peekArray numSolutions =<< {#get IsingResult->num_occurrences#} p)
     return $ QuboResult $ zip3 (filterQubo solutions) energies numOccurrences
  poke = undefined -- Don't expect to need to write results to memory
     
filterQubo :: [[CInt]] -> [[(Int,Int)]]
filterQubo = filterResult [0,1]

filterIsing :: [[CInt]] -> [[(Int, Int)]]
filterIsing = filterResult [-1,1]

filterResult :: [Int] -> [[CInt]] -> [[(Int,Int)]]
filterResult validspins vs =  map (f.map fromIntegral) vs where
  f = filter (\(i, s)-> s `elem` validspins) . zip [0..]

data ProblemEntry = ProblemEntry {
  i :: Int,
  j :: Int,
  value :: Double
} deriving (Show)

data Embeddings = Embeddings {
  verts :: [Int]
}

data FindEmbeddingParameters

instance Storable Embeddings where
  sizeOf p = 16 
  alignment p = sizeOf nullPtr 
  peek p = do
    i <- fromIntegral <$> {#get Embeddings->len#} p
    es <- castPtr <$> {#get Embeddings->elements#} p
    Embeddings <$> peekArray i es
  poke p (Embeddings x) = do
    {#set Embeddings.len#} p (fromIntegral $ length x)
    es <- castPtr <$> {#get Embeddings->elements#} p
    pokeArray es $ x

instance Storable ProblemEntry where
  sizeOf _ = {#sizeof ProblemEntry#}
  alignment _ = sizeOf nullPtr
  peek p = ProblemEntry <$> liftM fromIntegral ({#get ProblemEntry->i#} p)
                        <*> liftM fromIntegral ({#get ProblemEntry->j#} p)
                        <*> liftM (fromRational . toRational) ({#get ProblemEntry->value#} p) 
  poke p x = do 
    {#set ProblemEntry.i#} p (fromIntegral $ i x)
    {#set ProblemEntry.j#} p (fromIntegral $ j x)
    {#set ProblemEntry.value#} p (fromRational . toRational $ value x)

data Problem = Problem {
  elements :: [ProblemEntry]
} 

instance Storable AnnealOffsets where
  sizeOf (AnnealOffsets p) = {#sizeof AnnealOffsets#}
  alignment p = sizeOf nullPtr 
  peek p = do
    i <- fromIntegral <$> {#get AnnealOffsets->len#} p
    es <- castPtr <$> {#get AnnealOffsets->elements#} p
    AnnealOffsets <$> peekArray i es
  poke p (AnnealOffsets x) = do
    {#set AnnealOffsets.len#} p (fromIntegral $ length x) 
    es <- castPtr <$> {#get AnnealOffsets->elements#} p
    pokeArray es $ x

instance Storable Problem where
  sizeOf p = 16 
  alignment p = sizeOf nullPtr 
  peek p = do
    i <- fromIntegral <$> {#get Problem->len#} p
    es <- castPtr <$> {#get Problem->elements#} p
    Problem <$> peekArray i es
  poke p (Problem x) = do
    {#set Problem.len#} p (fromIntegral $ length x)
    es <- castPtr <$> {#get Problem->elements#} p
    pokeArray es $ x

data Solver
data Chains = Chains {
  chainelements :: [Int]
} deriving (Show)

data SolverParameters = SP {
  parameterUniqueId :: CInt,
  annealingTime :: CInt,
  answerMode :: Maybe SolverParameterAnswerMode,
  autoScale :: CInt,
  beta :: CDouble,
  chains :: Ptr Chains,
  maxAnswers :: CInt,
  numReads :: CInt,
  numSpinReversalTransforms :: CInt,
  postprocess :: Maybe Postprocess,
  programmingThermalization :: CInt,
  readoutThermalization :: CInt,
  annealOffsets :: Ptr AnnealOffsets
} deriving (Eq, Show)

-- DWave seems to use INT_MIN for unspecified values, including for enum types. Ugh. 
-- We handle this by adding maybe types for our enum types.
menum :: Enum a => Maybe a -> CInt
menum Nothing = minBound
menum (Just e) = fromIntegral $ fromEnum e

enumm :: Enum a => CInt -> Maybe a
enumm e | e == minBound = Nothing
enumm e = Just $ toEnum $ fromIntegral e

{#pointer *Chains as ChainsPtr -> Chains #}
{#pointer *AnnealOffsets as AnnealOffsetsPtr -> AnnealOffsets #}

instance Storable SolverParameters where
  sizeOf sp = 4
  alignment sp = 8
  peek p = SP <$> {#get QuantumSolverParameters->parameter_unique_id#}  p
              <*> {#get QuantumSolverParameters->annealing_time#} p
              <*> (enumm <$> {#get QuantumSolverParameters->answer_mode#} p)
              <*> {#get QuantumSolverParameters->auto_scale#} p
              <*> {#get QuantumSolverParameters->beta #} p
              <*> {#get QuantumSolverParameters->chains#} p
              <*> {#get QuantumSolverParameters->max_answers#} p
              <*> {#get QuantumSolverParameters->num_reads#} p
              <*> {#get QuantumSolverParameters->num_spin_reversal_transforms#} p
              <*> (enumm <$> {#get QuantumSolverParameters->postprocess#} p)
              <*> {#get QuantumSolverParameters->programming_thermalization#} p
              <*> {#get QuantumSolverParameters->readout_thermalization#} p
              <*> {#get QuantumSolverParameters->anneal_offsets#} p
  poke p (SP pid at am as b ch ma nr nsrt pp pt rt ao) = do
    {#set QuantumSolverParameters.parameter_unique_id#} p pid
    {#set QuantumSolverParameters.annealing_time#} p at
    {#set QuantumSolverParameters.answer_mode#} p $ menum am
    {#set QuantumSolverParameters.auto_scale#} p as
    {#set QuantumSolverParameters.beta#} p b
    {#set QuantumSolverParameters.chains#} p ch
    {#set QuantumSolverParameters.max_answers#} p ma
    {#set QuantumSolverParameters.num_reads#} p nr
    {#set QuantumSolverParameters.num_spin_reversal_transforms#} p nr
    {#set QuantumSolverParameters.postprocess#} p $ menum pp
    {#set QuantumSolverParameters.programming_thermalization#} p pt
    {#set QuantumSolverParameters.readout_thermalization#} p rt
    {#set QuantumSolverParameters.anneal_offsets#} p ao

sapiCtx :: C.Context
sapiCtx = mempty { CC.ctxTypesTable = M.fromList 
  [ (T.TypeName "sapi_QuantumSolverParameters", [t| SolverParameters |] ),
    (T.TypeName "sapi_FindEmbeddingParameters", [t| FindEmbeddingParameters |] ) ]
  }

