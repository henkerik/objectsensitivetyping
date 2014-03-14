--{-# LANGUAGE TypeFamilies #-}

module Framework.Program where

type Label = Int


class Program program where
 --   type Type program
 --   type Node program 
 --   type Label program
 --   type Field program
 --   type Param program
 --   type Function program

    init  :: program -> Label
    final :: program -> [Label]
    flow  :: program -> [(Label, Label)]
    node  :: program -> Label -> Stmt
    entry :: program -> CallableUnit -> Label
    exit  :: program -> CallableUnit -> Label
    after :: program -> Label -> Label

    isCallNode       :: program -> Label -> Bool
    typeByLabel      :: program -> Label -> Type
    paramsByFunction :: program -> CallableUnit -> [Param]
    resolve          :: program -> CallableUnit -> CallableUnit

--    fieldsByType     :: program -> Type program -> [Field program]
--    isSubtype        :: program -> Type program -> Type program -> Bool