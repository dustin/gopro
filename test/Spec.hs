import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck as QC

tests :: [TestTree]
tests = [
    testProperty "a test property" (\x -> (x::Int) == id x)
    ]

main :: IO ()
main = defaultMain $ testGroup "All Tests" tests
