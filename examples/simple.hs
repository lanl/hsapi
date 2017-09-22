{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

import DWave.SAPI
import qualified Data.Set as S

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
  liftIO $ do
    putStr "Number of edges available: " 
    print (length adj)
    putStrLn "Setting random values for first 50"
  prob <- Problem <$> mapM 
         (\(x,y)->ProblemEntry x y <$> random)
         (take 50 $ S.elems adj)
  solveIsing solver prob def "test"
  
main :: IO ()
main = runSapi example >>= either putStrLn print

