{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module TypeCheck where

import Control.Monad.Except(ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.State(StateT, MonadState, runStateT, get, put)
import Control.Monad.Identity(Identity, runIdentity)
import Control.Monad (liftM)

import qualified Data.Set as Set
import qualified Data.Map as Map

import Debug.Trace (trace)

import AST
import AST.Phases.Parsed
import AST.Phases.Typed

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
  ftv (TyVar n) = Set.singleton n
  ftv (TyDef _) = Set.empty
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

unaryOpMethod :: NodeSource -> UnaryOp -> ExprPA  -- TODO: UnaryOp should carry ns
unaryOpMethod ns Negate = IdentifierPA ns "op_negate"

binaryOpMethod :: NodeSource -> BinaryOp -> ExprPA -- TODO: BinaryOp should carry ns
binaryOpMethod ns Add = IdentifierPA ns "op_add"

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
inferExpression env el@(CastPA ns ty e) = do
  (Ty s1 t1 e1) <- inferExpression env e
  s' <- mgu (apply s1 t1) (apply s1 ty)
  pure $ Ty s' ty (CastTC (ts ty ns) ty e1)
inferExpression env el@(AssignPA ns e1 e2) = do
  (Ty s1 t1 e1') <- inferExpression env e1
  (Ty s2 t2 e2') <- inferExpression env e2
  s' <- mgu (apply s1 t1) (apply s2 t2)
  pure $ Ty s' t2 (AssignTC (ts (apply s2 t2) ns) e1' e2')
inferExpression env el@(UnaryPA ns fix op e) = do -- !a -> (!) a
  inferExpression env $ FunCallPA ns (unaryOpMethod ns op) [e]
inferExpression env el@(BinaryPA ns op e1 e2) = do -- 1 + 1 -> (+) 1 1
  inferExpression env $ FunCallPA ns (binaryOpMethod ns op) [e1, e2]
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

inferStatement :: TypeEnv -> StmtPA -> TI (Ty StmtTC)
inferStatement env (DeclarePA ns name ty expr) = do
  mexpr <- traverse (inferExpression env) expr
  msubst <- traverse (mgu ty) (typ <$> mexpr)
  pure $ Ty (fromMaybe nullSubst msubst) TyVoid (DeclareTC ns name ty (elm <$> mexpr))
inferStatement env (ReturnPA ns expr) = do
  mexpr <- traverse (inferExpression env) expr
  pure $ Ty nullSubst TyVoid (ReturnTC ns (elm <$> mexpr))
inferStatement env (WhilePA ns expr body) = do
  e1 <- inferExpression env expr
  bdy <- traverse (inferStatement env) body
  pure $ Ty nullSubst TyVoid (WhileTC ns (elm e1) (elm <$> bdy))
inferStatement env (ForPA ns init expr eff body) = do
  init' <- inferStatement env init
  expr' <- inferExpression env expr
  eff' <- inferExpression env eff
  body' <- traverse (inferStatement env) body
  pure $ Ty nullSubst TyVoid (ForTC ns (elm init') (elm expr') (elm eff') (elm <$> body'))
inferStatement env (IfPA ns te tb obs els) = do
  te' <- inferExpression env te
  tb' <- traverse (inferStatement env) tb
  obs' <- flip traverse obs $ \(ev, bdy) -> do
    ev' <- inferExpression env ev
    bdy' <- traverse (inferStatement env) bdy
    pure (elm ev', elm <$> bdy')
  els' <- traverse (traverse (inferStatement env)) els
  pure $ Ty nullSubst TyVoid (IfTC ns (elm te') (elm <$> tb') obs' ((fmap . fmap) elm els'))
inferStatement env (SExprPA ns expr) = do
  expr' <- inferExpression env expr
  pure $ Ty nullSubst TyVoid (SExprTC ns (elm expr'))

inferTopLevel :: TypeEnv -> ToplPA -> TI (Ty ToplTC)
inferTopLevel env (EnumPA ns name fields) = pure $ Ty nullSubst TyVoid (EnumTC ns name fields)
inferTopLevel env (UnionPA ns name fields) = pure $ Ty nullSubst TyVoid (UnionTC ns name fields)
inferTopLevel env (StructPA ns name fields) = pure $ Ty nullSubst TyVoid (StructTC ns name fields)
inferTopLevel env (FuncPA ns name retTy args body) = do
  body' <- traverse (inferStatement env) body
  pure $ Ty nullSubst TyVoid (FuncTC ns name retTy args (elm <$> body'))
inferTopLevel env (DeclPA ns name ty init) = do
  init' <- traverse (inferExpression env) init
  pure $ Ty nullSubst TyVoid (DeclTC ns name ty (elm <$> init'))
inferTopLevel env (ModulePA ns name body) = do
  body' <- traverse (inferTopLevel env) body
  pure $ Ty nullSubst TyVoid (ModuleTC ns name (elm <$> body'))
inferTopLevel env (TypeDefPA ns name ty) = pure $ Ty nullSubst TyVoid (TypeDefTC ns name ty)
inferTopLevel env (AliasPA ns lang s t) = pure $ Ty nullSubst TyVoid (AliasTC ns lang s t)
inferTopLevel env (ImportPA ns lang n) = pure $ Ty nullSubst TyVoid (ImportTC ns lang n)

applyExpression :: Map Text Scheme -> ExprPA -> TI ExprTC
applyExpression env e = do
  (Ty _ _ e') <- inferExpression (TypeEnv env) e
  pure e'
  --pure $ typedExpression (apply s t) e

applyStatement :: Map Text Scheme -> StmtPA -> TI StmtTC
applyStatement env e = do
  (Ty _ _ e') <- inferStatement (TypeEnv env) e
  pure e'
  --pure $ typedExpression (apply s t) e

applyTopLevel :: Map Text Scheme -> ToplPA -> TI ToplTC
applyTopLevel env e = do
  (Ty _ _ e') <- inferTopLevel (TypeEnv env) e
  pure e'

testenv :: Map Text Scheme
testenv = Map.fromList [ ("printf", Scheme [] (TyFun (TyDef "String") $ TyFun (TyDef "String") TyVoid))
                       , ("puts", Scheme [] (TyFun (TyDef "String") TyVoid))
                       , ("op_add", Scheme [] (TyFun (TyDef "Int32") $ TyFun (TyDef "Int32") (TyDef "Int32")))]

typeCheckExpression :: ExprPA -> Either Text ExprTC
typeCheckExpression expr = fst $ runTI (applyExpression testenv expr)

typeCheckStatement :: StmtPA -> Either Text StmtTC
typeCheckStatement stmt = fst $ runTI (applyStatement testenv stmt)

typeCheckTopLevel :: ToplPA -> Either Text ToplTC
typeCheckTopLevel topl = fst $ runTI (applyTopLevel testenv topl)
