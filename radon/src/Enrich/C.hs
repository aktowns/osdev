module Enrich.C where

import AST.Phases.Parsed
import AST.Phases.EnrichedC

-- at the moment just deal with C imports


enrich :: ToplPA -> ToplEC
enrich (EnumPA i1 i2 i3)       = EnumEC i1 i2 i3
enrich (UnionPA i1 i2 i3)      = UnionEC i1 i2 i3
enrich (StructPA i1 i2 i3)     = StructEC i1 i2 i3 
enrich (FuncPA i1 i2 i3 i4 i5) = FuncEC i1 i2 i3 i4 i5 
enrich (DeclPA i1 i2 i3 i4)    = DeclEC i1 i2 i3 i4 
enrich (ModulePA i1 i2 i3)     = ModuleEC i1 i2 i3
enrich (TypeDefPA i1 i2 i3)    = TypeDefEC i1 i2 i3
enrich (AliasPA i1 i2 i3 i4)   = AliasEC i1 i2 i3 i4
enrich (ImportPA i1 i2 i3)     = ImportEC i1 i2 i3

