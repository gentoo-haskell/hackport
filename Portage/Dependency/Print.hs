{-# LANGUAGE CPP #-}

module Portage.Dependency.Print
  (
    dep2str
  , dep2str_noindent
  ) where

import Portage.Version
import Portage.Use

import Portage.PackageId

import qualified Distribution.Pretty as DP (Pretty(..))
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ( vcat, nest, render )
import Text.PrettyPrint as PP ((<>))

import Portage.Dependency.Types

dispSlot :: SlotDepend -> Disp.Doc
dispSlot AnySlot          = Disp.empty
dispSlot AnyBuildTimeSlot = Disp.text ":="
dispSlot (GivenSlot slot) = Disp.text (':' : slot)

dispLBound :: PackageName -> LBound -> Disp.Doc
dispLBound pn (StrictLB    v) = Disp.char '>' PP.<> DP.pretty pn <-> DP.pretty v
dispLBound pn (NonstrictLB v) = Disp.text ">=" PP.<> DP.pretty pn <-> DP.pretty v
dispLBound _pn ZeroB = error "unhandled 'dispLBound ZeroB'"

dispUBound :: PackageName -> UBound -> Disp.Doc
dispUBound pn (StrictUB    v) = Disp.char '<' PP.<> DP.pretty pn <-> DP.pretty v
dispUBound pn (NonstrictUB v) = Disp.text "<=" PP.<> DP.pretty pn <-> DP.pretty v
dispUBound _pn InfinityB = error "unhandled 'dispUBound Infinity'"

dispDAttr :: DAttr -> Disp.Doc
dispDAttr (DAttr s u) = dispSlot s PP.<> dispUses u

dep2str :: Int -> Dependency -> String
dep2str start_indent = render . nest start_indent . showDepend

dep2str_noindent :: Dependency -> String
dep2str_noindent = render . showDepend

(<->) :: Disp.Doc -> Disp.Doc -> Disp.Doc
a <-> b = a PP.<> Disp.char '-' PP.<> b

sp :: Disp.Doc
sp = Disp.char ' '

sparens :: Disp.Doc -> Disp.Doc
sparens doc = Disp.parens (sp PP.<> valign doc PP.<> sp)

valign :: Disp.Doc -> Disp.Doc
valign d = nest 0 d

showDepend :: Dependency -> Disp.Doc
showDepend (DependAtom (Atom pn range dattr))
    = case range of
        -- any version
        DRange ZeroB InfinityB -> DP.pretty pn       PP.<> dispDAttr dattr
        DRange ZeroB ub        -> dispUBound pn ub PP.<> dispDAttr dattr
        DRange lb InfinityB    -> dispLBound pn lb PP.<> dispDAttr dattr
        -- TODO: handle >=foo-0    special case
        -- TODO: handle =foo-x.y.* special case
        DRange lb ub          ->    showDepend (DependAtom (Atom pn (DRange lb InfinityB) dattr))
                                 PP.<> Disp.char ' '
                                 PP.<> showDepend (DependAtom (Atom pn (DRange ZeroB ub)    dattr))
        DExact v              -> Disp.char '~' PP.<> DP.pretty pn <-> DP.pretty v { versionRevision = 0 } PP.<> dispDAttr dattr

showDepend (DependIfUse u td fd)  = valign $ vcat [td_doc, fd_doc]
    where td_doc
              | is_empty_dependency td = Disp.empty
              | otherwise =                  DP.pretty u PP.<> Disp.char '?' PP.<> sp PP.<> sparens (showDepend td)
          fd_doc
              | is_empty_dependency fd = Disp.empty
              | otherwise = Disp.char '!' PP.<> DP.pretty u PP.<> Disp.char '?' PP.<> sp PP.<> sparens (showDepend fd)
showDepend (DependAnyOf deps)   = Disp.text "||" PP.<> sp PP.<> sparens (vcat $ map showDependInAnyOf deps)
showDepend (DependAllOf deps)   = valign $ vcat $ map showDepend deps

-- needs special grouping
showDependInAnyOf :: Dependency -> Disp.Doc
showDependInAnyOf d@(DependAllOf _deps) = sparens (showDepend d)
-- both lower and upper bounds are present thus needs 2 atoms
-- TODO: '=foo-x.y.*' will take only one atom, not two
showDependInAnyOf d@(DependAtom (Atom _pn (DRange lb ub) _dattr))
    | lb /= ZeroB && ub /= InfinityB
                                       = sparens (showDepend d)
-- rest are fine
showDependInAnyOf d                    =          showDepend d
