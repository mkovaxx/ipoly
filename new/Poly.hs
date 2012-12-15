
module Poly where

import Expr

type Coefficient = Rational
type Exponent = Integer

data SPoly = SSum (Map STerm Coefficient)
data STerm = SMul (Map SAtom Exponent)
data SAtom = SIndeterminate Name
           | SInnerProduct VAtom VAtom

data VPoly = VSum (Map VTerm Coefficient)
data VTerm = VMul STerm VAtom
data VAtom = VIndeterminate Name

