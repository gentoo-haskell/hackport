module Portage.Dependency.Print
  (
    dep2str
  , dep2str_denorm -- for debugging
  ) where

import Portage.Version
import Portage.Use

import Portage.PackageId

import Distribution.Text ( Text(..) )
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ( (<>), vcat, nest, render )

import Portage.Dependency.Normalize
import Portage.Dependency.Types

dispSlot :: SlotDepend -> Disp.Doc
dispSlot AnySlot          = Disp.empty
dispSlot AnyBuildTimeSlot = Disp.text ":="
dispSlot (GivenSlot slot) = Disp.text (':' : slot)

dispLBound :: PackageName -> LBound -> Disp.Doc
dispLBound pn (StrictLB    v) = Disp.char '>' <> disp pn <-> disp v
dispLBound pn (NonstrictLB v) = Disp.text ">=" <> disp pn <-> disp v
dispLBound _pn ZeroB = error "unhandled 'dispLBound ZeroB'"

dispUBound :: PackageName -> UBound -> Disp.Doc
dispUBound pn (StrictUB    v) = Disp.char '<' <> disp pn <-> disp v
dispUBound pn (NonstrictUB v) = Disp.text "<=" <> disp pn <-> disp v
dispUBound _pn InfinityB = error "unhandled 'dispUBound Infinity'"

dispDAttr :: DAttr -> Disp.Doc
dispDAttr (DAttr s u) = dispSlot s <> dispUses u

dispDUse :: DUse -> Disp.Doc
dispDUse (DUse (is_enabled, name)) = prefix is_enabled <> Disp.text name <> Disp.char '?'
    where prefix True  = Disp.empty
          prefix False = Disp.char '!'

dep2str :: Int -> Dependency -> String
dep2str start_indent = render . nest start_indent . showDepend . normalize_depend

dep2str_denorm :: Dependency -> String
dep2str_denorm = render . showDepend

(<->) :: Disp.Doc -> Disp.Doc -> Disp.Doc
a <-> b = a <> Disp.char '-' <> b

sp :: Disp.Doc
sp = Disp.char ' '

sparens :: Disp.Doc -> Disp.Doc
sparens doc = Disp.parens (sp <> valign doc <> sp)

valign :: Disp.Doc -> Disp.Doc
valign d = nest 0 d

showDepend :: Dependency -> Disp.Doc
showDepend (Atom pn range dattr)
    = case range of
        -- any version
        DRange ZeroB InfinityB -> disp pn          <> dispDAttr dattr
        DRange ZeroB ub        -> dispUBound pn ub <> dispDAttr dattr
        DRange lb InfinityB    -> dispLBound pn lb <> dispDAttr dattr
        -- TODO: handle >=foo-0    special case
        -- TODO: handle =foo-x.y.* special case
        DRange lb ub          ->    showDepend (Atom pn (DRange lb InfinityB) dattr)
                                 <> Disp.char ' '
                                 <> showDepend (Atom pn (DRange ZeroB ub)    dattr)
        DExact v              -> Disp.char '~' <> disp pn <-> disp v { versionRevision = 0 } <> dispDAttr dattr

showDepend (DependIfUse u dep)  = dispDUse u     <> sp <> sparens (showDepend dep)
showDepend (DependAnyOf deps)   = Disp.text "||" <> sp <> sparens (vcat $ map showDependInAnyOf deps)
showDepend (DependAllOf deps)   = valign $ vcat $ map showDepend deps

-- needs special grouping
showDependInAnyOf :: Dependency -> Disp.Doc
showDependInAnyOf d@(DependAllOf _deps) = sparens (showDepend d)
-- both lower and upper bounds are present thus needs 2 atoms
-- TODO: '=foo-x.y.*' will take only one atom, not two
showDependInAnyOf d@(Atom _pn (DRange lb ub) _dattr)
    | lb /= ZeroB && ub /= InfinityB
                                       = sparens (showDepend d)
-- rest are fine
showDependInAnyOf d                    =          showDepend d
