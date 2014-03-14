module Analysis.Filterable where

import Prelude hiding (filter)
import qualified Data.Set as S
import Analysis.Lattice.AbstractValue
import Analysis.Lattice.AbstractAddress
import Analysis.Coerce
import Analysis.Inject
import Analysis.SensitivitySpec
import PHP.IR

class Filterable a where
	filter :: Graph -> Type -> a -> a

instance Filterable AbstractValue where
	filter g ty value = inject (filter g ty $ coerce value::AbstractAddress)

instance Filterable AbstractAddress where
	filter g ty = S.filter (\hctx -> ty `S.member` getSubtypes g (typeByAddress g hctx))
