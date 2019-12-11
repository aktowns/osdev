{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module AST.Phases.Typed where

import AST
import AST.Phases.Parsed

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.Except(ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State(StateT, MonadState, runStateT, get, put)
import Control.Monad.Identity(Identity, runIdentity)

data Typed

type ToplTC = TopLevel Typed
type ExprTC = Expression Typed
type StmtTC = Statement Typed

data Scheme = Scheme [Text] Type

newtype TypeEnv = TypeEnv (Map Text Scheme)

type Subst = Map Text Type

data TIState = TIState { tiSupply :: Int
                       , tiSubst  :: Subst
                       } deriving (Show)

newtype TI a = TI { unTI :: ExceptT Text (StateT TIState Identity) a }
  deriving (Monad, Applicative, Functor, MonadError Text, MonadState TIState)

runTI :: TI a -> (Either Text a, TIState)
runTI m = runIdentity $ runStateT (runExceptT (unTI m)) initState
 where
  initState = TIState 0 Map.empty

newTyVar :: Text -> TI Type
newTyVar pfx = do
  s <- get
  put s{tiSupply = tiSupply s + 1}
  pure $ TyVar (pfx <> toS (show $ tiSupply s))

instantiate :: Scheme -> TI Type
instantiate (Scheme vars t) = do
  nvars <- mapM (\_ -> newTyVar "a") vars
  let s = Map.fromList (zip vars nvars)
  pure $ apply s t

class Types a where
  ftv   :: a -> Set Text
  apply :: Subst -> a -> a

instance Types Type where
  ftv TyVoid = Set.empty

  apply _ t  = t

instance Types Scheme where
  ftv (Scheme vars t)     = ftv t `Set.difference` Set.fromList vars

  apply s (Scheme vars t) = Scheme vars (apply (foldr Map.delete s vars) t)

instance Types a => Types [a] where
  ftv     = foldr (Set.union . ftv) Set.empty

  apply s = fmap (apply s)

nullSubst :: Subst
nullSubst = Map.empty

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = Map.map (apply s1) s2 `Map.union` s1

remove :: TypeEnv -> Text -> TypeEnv
remove (TypeEnv env) var = TypeEnv $ Map.delete var env

instance Types TypeEnv where
  ftv (TypeEnv env) = ftv (Map.elems env)

  apply s (TypeEnv env) = TypeEnv $ Map.map (apply s) env

generalize :: TypeEnv -> Type -> Scheme
generalize env t = Scheme vars t
 where
  vars = Set.toList $ ftv t `Set.difference` ftv env

mgu :: Type -> Type -> TI Subst
mgu (TyVar u) t   = varBind u t
mgu t (TyVar u)   = varBind u t
mgu TyVoid TyVoid = pure nullSubst
mgu t1 t2         = throwError $ toS $ "type unification failed " <> show t1 <> " ~ " <> show t2

varBind :: Text -> Type -> TI Subst
varBind u t | t == TyVar u         = pure nullSubst
            | u `Set.member` ftv t = throwError "occurs check"
            | otherwise            = pure $ Map.singleton u t

ti :: TypeEnv -> ExprPA -> TI (Subst, Type)
ti (TypeEnv env) (Identifier _ n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " <> n
    Just sigma -> do
      t <- instantiate sigma
      pure (nullSubst, t)
ti env (FunCall e n a) = do
  tv <- newTyVar "a"
  (s1, t1) <- ti env (Identifier e n)
  (s2, t2) <- ti (apply s1 env) (a !! 0)
  s3 <- mgu (apply s2 t1) (TyFun t2 tv)
  pure (s3 `composeSubst` s2 `composeSubst` s1, apply s3 tv)
