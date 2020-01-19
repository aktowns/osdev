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

pattern EnumTC :: NodeSource -> Text -> [(Text, Maybe Integer)] -> ToplTC
pattern EnumTC i1 i2 i3 <- Enum i1 i2 i3
  where EnumTC i1 i2 i3 = Enum i1 i2 i3

pattern UnionTC :: NodeSource -> Text -> [(Text, [(Text, Type)])] -> ToplTC
pattern UnionTC i1 i2 i3 <- Union i1 i2 i3
  where UnionTC i1 i2 i3 = Union i1 i2 i3

pattern StructTC :: NodeSource -> Text -> [(Text, Type)] -> ToplTC
pattern StructTC i1 i2 i3 <- Struct i1 i2 i3
  where StructTC i1 i2 i3 = Struct i1 i2 i3

pattern FuncTC :: NodeSource -> Text -> Type -> ([(Text, Type)], Bool) -> [StmtTC] -> ToplTC
pattern FuncTC i1 i2 i3 i4 i5 <- Func i1 i2 i3 i4 i5
  where FuncTC i1 i2 i3 i4 i5 = Func i1 i2 i3 i4 i5

pattern DeclTC :: NodeSource -> Text -> Type -> Maybe ExprTC -> ToplTC
pattern DeclTC i1 i2 i3 i4 <- Decl i1 i2 i3 i4
  where DeclTC i1 i2 i3 i4 = Decl i1 i2 i3 i4

pattern ModuleTC :: NodeSource -> Text -> [ToplTC] -> ToplTC
pattern ModuleTC i1 i2 i3 <- Module i1 i2 i3
  where ModuleTC i1 i2 i3 = Module i1 i2 i3

pattern TypeDefTC :: NodeSource -> Text -> Type -> ToplTC
pattern TypeDefTC i1 i2 i3 <- TypeDef i1 i2 i3
  where TypeDefTC i1 i2 i3 = TypeDef i1 i2 i3

pattern AliasTC :: NodeSource -> Maybe Language -> (Type, Text, ([Type], Bool)) -> Text -> ToplTC
pattern AliasTC i1 i2 i3 i4 <- Alias i1 i2 i3 i4
  where AliasTC i1 i2 i3 i4 = Alias i1 i2 i3 i4

pattern ImportTC :: NodeSource -> Maybe Language -> Text -> ToplTC
pattern ImportTC i1 i2 i3 <- Import i1 i2 i3
  where ImportTC i1 i2 i3 = Import i1 i2 i3

--------
-- Expression

type instance XLiteral    Typed = TypedSource
type instance XBinary     Typed = TypedSource
type instance XUnary      Typed = TypedSource
type instance XIdentifier Typed = TypedSource
type instance XFunCall    Typed = TypedSource
type instance XArraySub   Typed = TypedSource
type instance XAssign     Typed = TypedSource
type instance XCast       Typed = TypedSource
type instance XMemberRef  Typed = TypedSource

pattern LiteralTC :: TypedSource -> Lit -> ExprTC
pattern LiteralTC i1 i2 <- Literal i1 i2
  where LiteralTC i1 i2 = Literal i1 i2

pattern BinaryTC :: TypedSource -> BinaryOp -> ExprTC -> ExprTC -> ExprTC
pattern BinaryTC i1 i2 i3 i4 <- Binary i1 i2 i3 i4
  where BinaryTC i1 i2 i3 i4 = Binary i1 i2 i3 i4

pattern UnaryTC :: TypedSource -> Fix -> UnaryOp -> ExprTC -> ExprTC
pattern UnaryTC i1 i2 i3 i4 <- Unary i1 i2 i3 i4
  where UnaryTC i1 i2 i3 i4 = Unary i1 i2 i3 i4

pattern IdentifierTC :: TypedSource -> Text -> ExprTC
pattern IdentifierTC i1 i2 <- Identifier i1 i2
  where IdentifierTC i1 i2 = Identifier i1 i2

pattern FunCallTC :: TypedSource -> ExprTC -> [ExprTC] -> ExprTC
pattern FunCallTC i1 i2 i3 <- FunCall i1 i2 i3
  where FunCallTC i1 i2 i3 = FunCall i1 i2 i3

pattern ArraySubTC :: TypedSource -> Text -> ExprTC -> ExprTC
pattern ArraySubTC i1 i2 i3 <- ArraySub i1 i2 i3
  where ArraySubTC i1 i2 i3 = ArraySub i1 i2 i3

pattern AssignTC :: TypedSource -> ExprTC -> ExprTC -> ExprTC
pattern AssignTC i1 i2 i3 <- Assign i1 i2 i3
  where AssignTC i1 i2 i3 = Assign i1 i2 i3

pattern CastTC :: TypedSource -> Type -> ExprTC -> ExprTC
pattern CastTC i1 i2 i3 <- Cast i1 i2 i3
  where CastTC i1 i2 i3 = Cast i1 i2 i3

pattern MemberRefTC :: TypedSource -> MemberType -> ExprTC -> ExprTC -> ExprTC
pattern MemberRefTC i1 i2 i3 i4 <- MemberRef i1 i2 i3 i4
  where MemberRefTC i1 i2 i3 i4 = MemberRef i1 i2 i3 i4

--------
-- Statement

type instance XDeclare Typed = NodeSource
type instance XReturn  Typed = NodeSource
type instance XWhile   Typed = NodeSource
type instance XFor     Typed = NodeSource
type instance XIf      Typed = NodeSource
type instance XSExpr   Typed = NodeSource

pattern DeclareTC :: NodeSource -> Text -> Type -> Maybe ExprTC -> StmtTC
pattern DeclareTC i1 i2 i3 i4 <- Declare i1 i2 i3 i4
  where DeclareTC i1 i2 i3 i4 = Declare i1 i2 i3 i4

pattern ReturnTC :: NodeSource -> Maybe ExprTC -> StmtTC
pattern ReturnTC i1 i2 <- Return i1 i2
  where ReturnTC i1 i2 = Return i1 i2

pattern WhileTC :: NodeSource -> ExprTC -> [StmtTC] -> StmtTC
pattern WhileTC i1 i2 i3 <- While i1 i2 i3
  where WhileTC i1 i2 i3 = While i1 i2 i3

pattern ForTC :: NodeSource -> StmtTC -> ExprTC -> ExprTC -> [StmtTC] -> StmtTC
pattern ForTC i1 i2 i3 i4 i5 <- For i1 i2 i3 i4 i5
  where ForTC i1 i2 i3 i4 i5 = For i1 i2 i3 i4 i5

pattern IfTC :: NodeSource -> ExprTC -> [StmtTC] -> [(ExprTC, [StmtTC])] -> Maybe [StmtTC] -> StmtTC
pattern IfTC i1 i2 i3 i4 i5 <- If i1 i2 i3 i4 i5
  where IfTC i1 i2 i3 i4 i5 = If i1 i2 i3 i4 i5

pattern SExprTC :: NodeSource -> ExprTC -> StmtTC
pattern SExprTC i1 i2 <- SExpr i1 i2
  where SExprTC i1 i2 = SExpr i1 i2

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

applyExpression :: Map Text Scheme -> ExprPA -> TI ExprTC
applyExpression env e = do
  (Ty _ _ e') <- inferExpression (TypeEnv env) e
  pure e'
  --pure $ typedExpression (apply s t) e

inferStatement :: TypeEnv -> StmtPA -> TI (Ty StmtTC)
inferStatement = undefined

inferTopLevel :: TypeEnv -> ToplPA -> TI (Ty ToplTC)
inferTopLevel = undefined
