{-|

  CLI tool for CaRMa partner import.

  Loads CSV files into partner database, producing a report.

 -}


import Control.Monad

import System.Environment
import System.Exit

import Carma.Partner

usage :: String
usage = "Usage: " ++
        "carma-partner-import 8000 dealers.csv out.csv " ++
        "[DealerCities.json PSACarMakers.json TaxSchemes.json PSAServices.json]"


main :: IO ()
main = do
  args <- getArgs
  when (length args < 3) $ putStrLn usage >> exitFailure

  -- Load dictionaries
  let (cp:input:output:rest) = args
      carmaPort = read cp
  Just dicts <-
      case (length args) of
        7 -> do
          let (cityFile:carFile:taxFile:servFile:_) = rest
          loadIntegrationDicts $ Right (cityFile, carFile, taxFile, servFile)
        _ -> loadIntegrationDicts $ Left carmaPort

  processData carmaPort input output dicts
