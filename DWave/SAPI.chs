{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module DWave.SAPI (
   SAPI
  ,EitherIO(..)
  ,runSapi
  ,Failable(..)
  ,globalInit
  ,remoteConnection
  ,random
  ,randomR
  ,solveQubo
  ,solveIsing
  ,findEmbedding
  ,def
  ,Problem(..)
  ,ProblemEntry(..)
  ,SolverParameters(..)
  ,getSolver
  ,listSolvers
  ,getHardwareAdjacency
  ,dwUrl
  ,dwToken
  ,IsingResult
  ,QuboResult
  ,liftIO
  ,err
  ,Code(..)
  ,SolverParameterAnswerMode(..)
  ,Postprocess(..)
  ,AnnealOffsets(..)
  ,Connection
  ,Solver
  ,Chains(..)
) where

import System.Environment
import System.IO.Unsafe
import Unsafe.Coerce
import Data.Monoid
import Data.Set (fromList, Set)
import Data.Default
import DWave.SAPI.Types
import Control.Applicative
import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String
import Foreign.Ptr
import qualified Language.C.Inline as C
import qualified System.Random as R
import qualified Language.C.Inline.Context as CC
import qualified Language.C.Types as T
import qualified Data.Map as M

#include "dwave_sapi.h"
#include "stdio.h"

{#pointer *sapi_Connection as ConnectionPtr -> Connection #}
{#pointer *sapi_ProblemEntry as ProblemEntryPtr -> ProblemEntry #}
{#pointer *sapi_Problem as ProblemPtr -> Problem #}
{#pointer *sapi_Solver as SolverPtr -> Solver #}
{#pointer *sapi_IsingResult as IsingResultPtr -> IsingResult #}
{#pointer *sapi_IsingResult as QuboResultPtr -> QuboResult #}
{#pointer *sapi_Embeddings as EmbeddingsPtr -> Embeddings #}

C.context (C.baseCtx <> C.vecCtx <> C.funCtx <> sapiCtx)
C.include "<dwave_sapi.h>"

{#context prefix = "sapi"#}

runSapi = runEitherIO

data EitherIO e a = EitherIO {
  runEitherIO :: IO (Either e a)
}

instance Functor (EitherIO e) where
  fmap f (EitherIO x) = EitherIO $ fmap (fmap f) x

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  f <*> x = EitherIO $ liftA2 (<*>) (runEitherIO f) (runEitherIO x)

instance Monad (EitherIO e) where
  return = pure
  x >>= f = EitherIO $ runEitherIO x >>= either (return . Left) (runEitherIO . f)

liftIO :: IO a -> EitherIO e a
liftIO x = EitherIO (fmap Right x)

type SAPI = EitherIO String

class Failable a b where
  check :: String -> a -> SAPI b

instance Failable CInt () where
  check s i = case toEnum $ fromEnum i of
    Ok -> return () 
    err -> EitherIO $ return $ Left $ s ++ " returned " ++ show err

err :: String -> SAPI a
err = EitherIO . return . Left

instance Failable (Ptr a) (Ptr a) where
  check s p | p == nullPtr = EitherIO $ return $ Left $ s ++ " returned null"
  check s p = return p

instance Failable a b => Failable (a, CInt) b where
  check s (a,b) = (\() x -> x) <$> check s b <*> check s a

globalInit :: SAPI ()
globalInit = check "globalInit" =<< liftIO {#call globalInit#}

dwErr :: String
dwErr = "Couldn't find DW_* environment variable to use this function you must set the appropriate environment variables with the qOp tool `dw`"

dwUrl :: SAPI String
dwUrl = maybe (err dwErr) pure 
  =<< liftIO (lookupEnv "DW_INTERNAL__HTTPLINK")

dwToken :: SAPI String
dwToken = maybe (err dwErr) pure 
  =<< liftIO (lookupEnv "DW_INTERNAL__TOKEN")

remoteConnection :: String -> String -> Maybe String -> SAPI (Ptr Connection)
remoteConnection url token proxy = check "remoteConnection" =<< (liftIO $ 
  withCString url $ \url' -> 
  withCString token $ \token' -> 
  alloca $ \conptr -> 
  maybe ($ nullPtr) withCString proxy $ \proxy' ->
  allocaBytes 256 $ \errmsg -> do
    code <- {#call remoteConnection#} url' token' proxy' conptr errmsg
    (,) <$> peek conptr <*> pure code)

peekStrings :: Ptr CString -> IO [String]
peekStrings ss | ss == nullPtr = return []
peekStrings ss = ps ss 0 where 
  ps ss i = do
    p <- peekElemOff ss i 
    if p == nullPtr then return [] else (:) <$> peekCString p <*> ps ss (i+1)

listSolvers :: Ptr Connection -> SAPI [String]
listSolvers con = do
  ssptr <- check "listSolvers" =<< (liftIO $ {#call listSolvers#} con)
  liftIO $ peekStrings ssptr

getSolver :: Ptr Connection -> String -> SAPI (Ptr Solver)
getSolver c s = check "getSolver" =<< (liftIO $ do
  s' <- newCString s
  {#call getSolver#} c s')

getHardwareAdjacency :: Ptr Solver -> SAPI (Set (Int, Int))
getHardwareAdjacency s = do
  p <- liftIO $ alloca $ \pp -> {#call getHardwareAdjacency#} s pp >> peek pp
  fromList . map (\(ProblemEntry i j _) -> (i,j)) . elements <$> liftIO (peek p)

{#pointer *sapi_FindEmbeddingParameters as FindEmbeddingParametersPtr -> FindEmbeddingParameters #}
{#pointer *sapi_SolverParameters as SolverParametersPtr -> SolverParameters #}

{-instance Default FindEmbeddingParameters where
  def = unsafePerformIO $ peek [C.pure| const sapi_FindEmbeddingParameters*
           {&SAPI_FIND_EMBEDDING_DEFAULT_PARAMETERS} |]
-}

instance Default SolverParameters where
  def = unsafePerformIO $ peek [C.pure| const sapi_QuantumSolverParameters*
           {&SAPI_QUANTUM_SOLVER_DEFAULT_PARAMETERS} |]

solveQubo :: Ptr Solver -> Problem -> SolverParameters -> String -> SAPI QuboResult
solveQubo s p@(Problem ps) sp str = do
  ptr <- check "solveQubo" =<< (liftIO $ alloca $ \pp -> 
    withArray ps $ \ps' -> {#set Problem.elements #} pp ps' >> poke pp p  >> 
    (withCString str $ \s' -> 
    alloca $ \result ->
    alloca $ \spp -> do 
      poke spp sp
      code <- {#call solveQubo#} s pp spp result s'
      (,) <$> peek result <*> pure code)) 
  liftIO $ peek ptr

solveIsing :: Ptr Solver -> Problem -> SolverParameters -> String -> SAPI IsingResult 
solveIsing s p@(Problem ps) sp str = do
  (ptr :: QuboResultPtr) <- check "solveIsing" =<< (liftIO $ alloca $ \pp -> 
    withArray ps $ \ps' -> {#set Problem.elements #} pp ps' >> poke pp p  >> 
    (withCString str $ \s' -> 
    alloca $ \result ->
    alloca $ \spp -> do 
      poke spp sp
      code <- {#call solveIsing#} s pp spp result s'
      (,) <$> peek result <*> pure code)) 
  liftIO $ peek (unsafeCoerce ptr)

findEmbedding :: Problem -> Set (Int, Int) -> String -> SAPI Embeddings
findEmbedding p@(Problem ps) g str = do
  ptr <- check "findEmbeddings" =<< (liftIO $ alloca $ \pp -> 
    withArray ps $ \ps' -> {#set Problem.elements #} pp ps' >> poke pp p  >> 
    (withCString str $ \s' -> 
    alloca $ \result -> do
      let ep = [C.pure| const sapi_FindEmbeddingParameters*
           {&SAPI_FIND_EMBEDDING_DEFAULT_PARAMETERS} |]
      code <- {#call findEmbedding#} pp pp ep result s'
      (,) <$> peek result <*> pure code)) 
  liftIO $ peek ptr

random :: R.Random a => SAPI a 
random = liftIO R.randomIO

randomR :: R.Random a => (a, a) -> SAPI a
randomR = liftIO . R.randomRIO
