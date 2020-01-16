{-# LANGUAGE GeneralizedNewtypeDeriving, PatternSynonyms, TypeFamilies, MultiParamTypeClasses, RecordWildCards #-}
module AST.Phases.Typed where

import AST
import AST.Phases.Parsed

import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.Except(ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State(StateT, MonadState, runStateT, get, put)
import Control.Monad.Identity(Identity, runIdentity)
import Control.Monad (liftM)

import Debug.Trace (trace)

data Typed

type ToplTC = TopLevel Typed
type ExprTC = Expression Typed
type StmtTC = Statement Typed

data TypedSource = TypedSource { nodeSource :: NodeSource
                               , typed      :: Type 
                               }

instance Show TypedSource where
  show TypedSource{..} = show nodeSource <> " [" <> show typed <> "]"

--------
-- Toplevel

type instance XEnum    Typed = NodeSource
type instance XUnion   Typed = NodeSource
type instance XStruct  Typed = NodeSource
type instance XFunc    Typed = NodeSource
type instance XDecl    Typed = NodeSource
type instance XModule  Typed = NodeSource
type instance XTypeDef Typed = NodeSource
type instance XAlias   Typed = NodeSource
type instance XImport  Typed = NodeSource

--------
-- Expression

type instance XLiteral    Typed = TypedSource
type instance XBinary     Typed = NodeSource
type instance XUnary      Typed = NodeSource
type instance XIdentifier Typed = TypedSource
type instance XFunCall    Typed = TypedSource
type instance XArraySub   Typed = NodeSource
type instance XAssign     Typed = NodeSource
type instance XCast       Typed = NodeSource
type instance XMemberRef  Typed = NodeSource

pattern IdentifierTC :: TypedSource -> Text -> ExprTC
pattern IdentifierTC i1 i2 <- Identifier i1 i2
  where IdentifierTC i1 i2 = Identifier i1 i2

pattern LiteralTC :: TypedSource -> Lit -> ExprTC
pattern LiteralTC i1 i2 <- Literal i1 i2
  where LiteralTC i1 i2 = Literal i1 i2

pattern FunCallTC :: TypedSource -> ExprTC -> [ExprTC] -> ExprTC
pattern FunCallTC i1 i2 i3 <- FunCall i1 i2 i3
  where FunCallTC i1 i2 i3 = FunCall i1 i2 i3

---------

data Scheme = Scheme [Text] Type deriving (Show)

newtype TypeEnv = TypeEnv (Map Text Scheme) deriving (Show)

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

  apply s (TyVar n) =
    case Map.lookup n s of
      Nothing -> TyVar n
      Just t  -> t
  apply s (TyFun t1 t2) = TyFun (apply s t1) (apply s t2)
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

-- most general unifier
mgu :: Type -> Type -> TI Subst
mgu l r = {-trace ("comparing l '" <> show l <> "' and r '" <> show r <> "'")-} mgu' l r
 where
  mgu' (TyFun l r) (TyFun l' r') = do
    s1 <- mgu l l'
    s2 <- mgu (apply s1 r) (apply s1 r')
    pure $ s2 `composeSubst` s1
  mgu' (TyVar u) t   = varBind u t
  mgu' t (TyVar u)   = varBind u t
  mgu' TyVoid TyVoid = pure nullSubst
  mgu' (TyDef l) (TyDef r)
    | l == r = pure nullSubst -- TODO: overlapping synonyms/deref synonyms
  mgu' t1 t2         = throwError $ toS $ "type unification failed " <> show t1 <> " ~ " <> show t2

varBind :: Text -> Type -> TI Subst
varBind u t | t == TyVar u         = pure nullSubst
            | u `Set.member` ftv t = throwError "occurs check"
            | otherwise            = pure $ Map.singleton u t

tiLit :: Lit -> TI (Subst, Type)
tiLit (IntLiteral _ _ _) = pure (nullSubst, TyDef "Int32")
tiLit (StrLiteral _)     = pure (nullSubst, TyDef "String")
tiLit (CharLiteral _)    = pure (nullSubst, TyDef "Char")

scanM :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m ([a])
scanM _ z [] = pure $ pure z
scanM f z (x:xs) = do
  z' <- f z x
  liftM (\x' -> pure z ++ x') $ scanM f z' xs

mapAccumM :: (Monad m) => (acc -> x -> m (acc, y)) -> acc -> [x] -> m (acc, [y])
mapAccumM _ z [] = pure (z, [])
mapAccumM f z (x:xs) = do
  (z', y) <- f z x
  (z'', ys) <- mapAccumM f z' xs
  pure (z'', pure y ++ ys) 

tyFunArgs :: Type -> [Type]
tyFunArgs (TyFun h t@(TyFun _ _)) = h : tyFunArgs t
tyFunArgs (TyFun h _)             = [h]
tyFunArgs x = [x]

ts :: Type -> NodeSource -> TypedSource
ts ty ns = TypedSource ns ty

data Ty a = Ty Subst Type a deriving (Show)

sbs :: Ty a -> Subst
sbs (Ty x _ _) = x

typ :: Ty a -> Type
typ (Ty _ x _) = x

elm :: Ty a -> a
elm (Ty _ _ x) = x

inferExpression :: TypeEnv -> ExprPA -> TI (Ty ExprTC) -- (Subst, Type, ExprTC)
inferExpression (TypeEnv env) el@(IdentifierPA ns n) =
  case Map.lookup n env of
    Nothing -> throwError $ "unbound variable: " <> n
    Just sigma -> do
      t <- instantiate sigma
      pure $ Ty nullSubst t (IdentifierTC (ts t ns) n)
inferExpression _ el@(LiteralPA ns t) = do
  (s, t') <- tiLit t
  pure $ Ty s t' (LiteralTC (ts t' ns) t)
inferExpression env el@(FunCallPA ns n xs) = do
  tv <- newTyVar "a"
  (Ty s1 t1 e1) <- inferExpression env n
  args <- scanM (\(Ty sub _ _) arg -> inferExpression (apply sub env) arg) (Ty s1 t1 e1) xs
  case args of
    (h:t) | Just th <- head t, Just l <- last t -> do
      let cargs = foldl (\f (Ty _ t _) x -> f $ TyFun t x) (TyFun (typ th)) $ tail t
      s' <- mgu (apply (sbs l) t1) (cargs tv)
      let t' = apply s' tv
      let e = FunCallTC (ts t' ns) e1 (elm <$> tail args)
      pure $ Ty (foldl composeSubst s' (sbs <$> reverse args)) t' e

applyExpression :: Map Text Scheme -> ExprPA -> TI ExprTC
applyExpression env e = do
  (Ty _ _ e') <- inferExpression (TypeEnv env) e
  pure e'
  --pure $ typedExpression (apply s t) e

testenv :: Map Text Scheme
testenv = Map.fromList [ ("printf", Scheme [] (TyFun (TyDef "String") $ TyFun (TyDef "String") TyVoid))
                       , ("puts", Scheme [] (TyFun (TyDef "String") TyVoid))]

testfcall2 :: ExprPA
testfcall2 = FunCall (NodeSource {filename = "example.ra", line = 2, column = 39}) (Identifier (NodeSource {filename = "example.ra", line = 2, column = 39}) "printf") [Literal (NodeSource {filename = "example.ra", line = 2, column = 46}) (StrLiteral "Hello %s!\n"),Literal (NodeSource {filename = "example.ra", line = 2, column = 61}) (StrLiteral "World")]

testfcall1 :: ExprPA
testfcall1 = FunCall (NodeSource {filename = "example.ra", line = 2, column = 39}) (Identifier (NodeSource {filename = "example.ra", line = 2, column = 39}) "puts") [Literal (NodeSource {filename = "example.ra", line = 2, column = 46}) (StrLiteral "Hello World!\n")]

testbadfcall2 :: ExprPA
testbadfcall2 = FunCall (NodeSource {filename = "example.ra", line = 2, column = 39}) (Identifier (NodeSource {filename = "example.ra", line = 2, column = 39}) "printf") [Literal (NodeSource {filename = "example.ra", line = 2, column = 46}) (StrLiteral "Hello %s!\n"),Literal (NodeSource {filename = "example.ra", line = 2, column = 61}) (CharLiteral 'W')]