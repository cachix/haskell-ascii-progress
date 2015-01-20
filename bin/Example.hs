import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import System.Console.AsciiProgress (Options(..), complete, def,
                                     newProgressBar, tick)

main :: IO ()
main = do
    pg <- newProgressBar def { pgWidth = 50 }
    loop pg
  where
    loop pg = do
        b <- complete pg
        unless b $ do
            threadDelay $ 200 * 1000
            tick pg
            loop pg
