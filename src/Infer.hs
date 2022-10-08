-- If you want to follow along:
--
-- git clone https://github.com/well-typed/type-inference-munihac-2022.git
--
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

import Control.Monad.Identity
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set

import Syntax
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

type Subst = Map Name MType

identity :: Subst
identity = Map.empty

compose :: Subst -> Subst -> Subst
compose s1 s2 = Map.union (fmap (subst s1) s2) s1

class Substitutable a where
  subst :: Subst -> a -> a
  free :: a -> Set Name

instance Substitutable MType where
  subst :: Subst -> MType -> MType
  subst s mt = case mt of
    TVar v -> case Map.lookup v s of
      Just t -> t
      Nothing -> TVar v
    Con c -> Con c
    Arr t1 t2 -> Arr (subst s t1) (subst s t2)
    List t -> List (subst s t)

  free :: MType -> Set Name
  free (TVar v) = Set.singleton v
  free (Con _) = Set.empty
  free (Arr t1 t2) = Set.union (free t1) (free t2)
  free (List t) = free t

instance Substitutable PType where
  subst :: Subst -> PType -> PType
  subst s (Forall vars mt) =
    Forall vars (subst (deleteVarsSubst vars s) mt)

  free :: PType -> Set Name
  free (Forall vars mt) = deleteVarsSet vars (free mt)

deleteVarsSet :: [Name] -> Set Name -> Set Name
deleteVarsSet []       s = s
deleteVarsSet (x : xs) s =
  deleteVarsSet xs (Set.delete x s)

deleteVarsSubst :: [Name] -> Subst -> Subst
deleteVarsSubst []       s = s
deleteVarsSubst (x : xs) s =
  deleteVarsSubst xs (Map.delete x s)

instance Substitutable Env where
  subst :: Subst -> Env -> Env
  subst s env = subst s <$> env

  free :: Env -> Set Name
  free env = Set.unions (map free (Map.elems env))

initEnv :: Env
initEnv =
  Map.fromList
    [ ("id", Forall ["a"] (TVar "a" ~> TVar "a"))
    , ("nil", Forall ["a"] (List (TVar "a")))
    , ("cons", Forall ["a"] (TVar "a" ~> List (TVar "a") ~> List (TVar "a")))
    , ("fix", Forall ["a"] ((TVar "a" ~> TVar "a") ~> TVar "a"))
    , ("caseList", Forall ["a", "r"] (List (TVar "a") ~> TVar "r" ~> (TVar "a" ~> List (TVar "a") ~> TVar "r") ~> TVar "r"))
    ]

infer :: InferMonad m => Env -> Expr -> m (Subst, MType)
infer env (Var x) =
  case Map.lookup x env of
    Nothing -> stop "encountered unknown variable"
    Just sigma -> do
      mt <- instantiate sigma
      return (identity, mt)
infer env (Lam x e) = do
  beta <- fresh
  (s1, tau1) <- infer (Map.insert x (Forall [] beta) env) e
  return (s1, Arr (subst s1 beta) tau1)
infer env (App e1 e2) = do
  (s1, tau1) <- infer env e1
  (s2, tau2) <- infer (subst s1 env) e2
  beta <- fresh
  s3 <- unify (subst s2 tau1) (Arr tau2 beta)
  return (s3 `compose` s2 `compose` s1, subst s3 beta)
infer env (Let x e1 e2) = do
  (s1, tau1) <- infer env e1
  pt <- generalise (subst s1 env) tau1
  let env' = Map.insert x pt env
  (s2, tau2) <- infer (subst s1 env') e2
  return (s2 `compose` s1, tau2)

instantiate :: InferMonad m => PType -> m MType
instantiate (Forall vars mt) = do
  newVars <- traverse (const fresh) vars
  let s = Map.fromList (zip vars newVars)
  return (subst s mt)

generalise :: InferMonad m => Env -> MType -> m PType
generalise env mt =
  return (Forall (Set.toList (free mt `Set.difference` free env)) mt)

-- TVar, Con, Arr, List

unify :: InferMonad m => MType -> MType -> m Subst
unify (TVar x) mt = varBind x mt
unify mt (TVar x) = varBind x mt
unify (Con c1) (Con c2) =
  if c1 == c2
    then return identity
    else stop "unification failed"
unify (Arr t11 t12) (Arr t21 t22) = do
  s1 <- unify t11 t21
  s2 <- unify (subst s1 t12) (subst s1 t22)
  return (s2 `compose` s1)
unify (List t1) (List t2) =
  unify t1 t2
unify _ _ = stop "unification failed"

varBind :: InferMonad m => Name -> MType -> m Subst
varBind x (TVar y) | x == y = return identity
varBind x mt = return (Map.singleton x mt)
  -- NOTE: this is problematic if mt mentions x

class Monad m => InferMonad m where
  fresh :: m MType
  stop :: String -> m a

newtype Infer a = MkInfer (StateT Int (ExceptT String Identity) a)
  deriving (Functor, Applicative, Monad, MonadState Int, MonadError String)

instance InferMonad Infer where
  fresh :: Infer MType
  fresh = do
    c <- get
    put (c + 1)
    return (TVar ("_v" <> show c))

  stop :: String -> Infer a
  stop = throwError

runInfer :: Infer a -> Either String a
runInfer (MkInfer m) =
  runIdentity (runExceptT (evalStateT m 0))

properTypeOf :: Env -> Expr -> Either String PType
properTypeOf env expr =
  runInfer $ do
    (_, mt) <- infer env expr
    generalise env mt

main :: IO ()
main = repl initEnv properTypeOf

