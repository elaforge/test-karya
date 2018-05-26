-- | Exports from "EL.Test.Testing".  This is meant to be imported
-- unqualified.
module EL.Test.Global (module EL.Test.Testing) where
import EL.Test.Testing
       (ModuleMeta(ModuleMeta), check, checkVal, equal, equalFmt, rightEqual,
        notEqual, equalf, stringsLike, leftLike, match, throws, ioEqual,
        ioHuman, pause, success, failure, expectRight, quickcheck, qcEqual,
        pprint)
