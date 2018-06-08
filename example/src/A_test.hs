module A_test where
import EL.Test.Global
import qualified A


test_f = do
    let f = A.f
    equal (f 4) 8
    equal (f 4) 9
