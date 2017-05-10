{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import DWave.SAPI
import Data.Set

-- Assumes your URL and key are in your environment 
example :: SAPI IsingResult 
example = do
  globalInit
  url <- dwUrl
  token <- dwToken
  rc <- remoteConnection url token Nothing 
  solvers <- listSolvers rc
  liftIO $ putStrLn "Supported solvers:" >> mapM_ print solvers
  solver <- getSolver rc "DW2X"
  adj <- getHardwareAdjacency solver
  liftIO $ putStr "Number of edges available: " >> print (length adj)
  prob <- Problem <$> mapM (\e->uncurry ProblemEntry <$> pure e <*> random) 
         (take 50 $ elems adj)
  solveIsing solver prob defaultParameters "test"
  
main :: IO ()
main = runEitherIO example
   >>= either (putStrLn . ("Example failed with error: " ++)) print 

