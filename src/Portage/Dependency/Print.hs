{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Portage.Dependency.Print
  (
    dep2str
  , dep2str_noindent
  , showDepend
  ) where

import Portage.Version
import Portage.Use

import Portage.PackageId

import Data.Default.Class
import Data.Maybe (catMaybes)
import Hackport.Pretty

import Portage.Dependency.Types

dispSlot :: SlotDepend -> Doc ann
dispSlot AnySlot          = emptyDoc
dispSlot AnyBuildTimeSlot = ":="
dispSlot (GivenSlot slot) = ":" <> pretty slot

dispLBound :: PackageName -> LBound -> Doc ann
dispLBound pn (StrictLB    v) = ">" <> pretty pn <-> pretty v
dispLBound pn (NonstrictLB v) = ">=" <> pretty pn <-> pretty v
dispLBound _pn ZeroB = error "unhandled 'dispLBound ZeroB'"

dispUBound :: PackageName -> UBound -> Doc ann
dispUBound pn (StrictUB    v) = "<" <> pretty pn <-> pretty v
dispUBound pn (NonstrictUB v) = "<=" <> pretty pn <-> pretty v
dispUBound _pn InfinityB = error "unhandled 'dispUBound Infinity'"

dispDAttr :: DAttr -> Doc ann
dispDAttr (DAttr s u) = dispSlot s <> dispUses u

dep2str :: Int -> Dependency -> String
dep2str start_indent = renderDoc . nest start_indent . showDepend

dep2str_noindent :: Dependency -> String
dep2str_noindent = renderDoc . showDepend

(<->) :: Doc ann -> Doc ann -> Doc ann
a <-> b = a <> "-" <> b

sp :: Doc ann
sp = " "

sparens :: Doc ann -> Doc ann
sparens doc = parens (sp <> valign doc <> sp)

valign :: Doc ann -> Doc ann
valign d = nest 0 d

showDepend :: Dependency -> Doc ann
showDepend (DependAtom (Atom pn range dattr))
    = case range of
        -- any version
        DRange ZeroB InfinityB -> pretty pn        <> dispDAttr dattr
        DRange ZeroB ub        -> dispUBound pn ub <> dispDAttr dattr
        DRange lb InfinityB    -> dispLBound pn lb <> dispDAttr dattr
        -- TODO: handle >=foo-0    special case
        -- TODO: handle =foo-x.y.* special case
        DRange lb ub          ->     showDepend (DependAtom (Atom pn (DRange lb InfinityB) dattr))
                                 <+> showDepend (DependAtom (Atom pn (DRange ZeroB ub) def))
        DExact v              -> "~" <> pretty pn <-> pretty v { versionRevision = 0 } <> dispDAttr dattr

showDepend (DependIfUse u td fd)  = valign $ vcat $ catMaybes [td_doc, fd_doc]
    where td_doc
              | is_empty_dependency td = Nothing
              | otherwise = Just $ vcat
                  [ pretty u <> "?" <+> "("
                  , indent 1 (emptyDoc <> showDepend td)
                  , ")"
                  ]
          fd_doc
              | is_empty_dependency fd = Nothing
              | otherwise = Just $ vcat
                  [ "!" <> pretty u <> "?" <+> "("
                  , indent 1 (emptyDoc <> showDepend fd)
                  , ")"
                  ]
showDepend (DependAnyOf deps)   = vcat $
    [ "||" <+> "("
    , indent 1 (vcat (map showDependInAnyOf deps))
    , ")"
    ]
showDepend (DependAllOf deps)   = vcat $ map showDepend deps

-- needs special grouping
showDependInAnyOf :: Dependency -> Doc ann
showDependInAnyOf d@(DependAllOf _deps) = sparens (showDepend d)
-- both lower and upper bounds are present thus needs 2 atoms
-- TODO: '=foo-x.y.*' will take only one atom, not two
showDependInAnyOf d@(DependAtom (Atom _pn (DRange lb ub) _dattr))
    | lb /= ZeroB && ub /= InfinityB
                                       = sparens (showDepend d)
-- rest are fine
showDependInAnyOf d                    =          showDepend d
