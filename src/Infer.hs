-- If you want to follow along:
--
-- git clone https://github.com/well-typed/type-inference-munihac-2022.git
--
module Main where

-- import Control.Applicative hiding ((<|>))
-- import Control.Monad.Identity
-- import Control.Monad.State.Strict
-- import Control.Monad.Except
-- import Data.Map (Map)
import qualified Data.Map as Map
-- import Data.Set (Set)
-- import qualified Data.Set as Set

-- import Syntax
import Types
import Repl

-- The plan, approximately:
--
-- Types and polytypes
-- Environments
-- Substitutions
-- Type inference
-- A monadic interface
-- Unification
-- Implementation of the monad
-- Trying it out

initGamma :: Env
initGamma = Map.empty

main :: IO ()
main = repl initGamma dummyTypeOf

