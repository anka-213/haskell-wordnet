import Distribution.Simple
import System.Directory (getCurrentDirectory)
import Paths_wordnet

main = do
    wd <- getDataFileName "foo"

    defaultMainWithHooks simpleUserHooks
        { postBuild = \_ _ _ _ -> putStrLn wd
        }
