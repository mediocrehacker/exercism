module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int 
  deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour m = 
  Clock hour' m'
  where
    m' = mod m 60
    hour' = mod (hour + div m 60) 24 


toString :: Clock -> String
toString (Clock hour m) =
  fn (show hour) ++ (":") ++ fn (show m)
  where
    fn :: String -> String
    fn (x:[]) = "0" ++ [x]
    fn xs = xs

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour m (Clock hour' m') =
  Clock (mod hour'' 24) m'' 
  where
    m'' = mod (m + m') 60
    hour'' = hour + hour' + (div (m + m') 60)
