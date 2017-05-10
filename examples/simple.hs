{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import DWave.SAPI
import DWave.SAPI.Types
import Data.Set
import Data.List

-- Assumes your URL and key are in your environment 
example :: SAPI IsingResult 
example = do
  globalInit
  rc <- remoteConnection <$> dwUrl <*> dwToken <*> pure Nothing >>= id
  solvers <- listSolvers rc
  liftIO $ putStrLn "Supported solvers:" >> mapM_ print solvers
  solver <- if elem "DW2X" solvers 
    then getSolver rc "DW2X"
    else err "DW2X not in supported solvers"
  adj <- getHardwareAdjacency solver
  liftIO $ putStr "Number of edges available: " >> print (length adj)
  prob <- Problem <$> mapM (\e->uncurry ProblemEntry <$> pure e <*> random) 
         (take 50 $ sort $ elems adj)
  solveIsing solver prob defaultParameters "test"
  
main :: IO ()
main = do
  result <- runEitherIO example
  either (putStrLn . ("Example failed with error: " ++)) print result

