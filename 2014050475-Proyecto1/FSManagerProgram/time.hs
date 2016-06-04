

import Data.Time
import Data.List.Split



main :: IO()
main = do
	myTime <- getZonedTime
	let a = show (splitOn " " (formatTime defaultTimeLocale "%M:%d:%Y %T" myTime))
	putStrLn (a)


