module UnicornHat
  ( UnicornHat
  , withHat
  , draw
  , changeBrightness
  )
  where

import Foreign
import Foreign.Storable
import Control.Exception
import Ws2811

newtype UnicornHat = UnicornHat (Ptr Ws2811)

initialUnicornHat :: Ws2811
initialUnicornHat = defaultWs2811
  { dmaNum = 5
  , channel0 = defaultChannel
      { gpioNum    = 18
      , count      = 64
      , brightness = 255
      }
  }

withHat :: (UnicornHat -> IO ()) -> IO ()
withHat k =
  with initialUnicornHat $ \ptr ->
  bracket_ (ws2811_init ptr) (ws2811_fini ptr) $
  k (UnicornHat ptr)

draw :: UnicornHat -> [[Ws2811Led]] -> IO ()
draw (UnicornHat hat) pic =
  do let chan0 = channelPtr hat 0
     leds <- getLeds chan0
     pokeArray leds (take 64 (flatten pic))
     _ <- ws2811_render hat
     return ()

changeBrightness :: UnicornHat -> Int -> IO ()
changeBrightness (UnicornHat hat) x
  | 0 <= x && x <= 255 = setBrightness (channelPtr hat 0) (fromIntegral x)
  | otherwise          = fail "UnicornHat.changeBrightness: invalid argument"

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:y:z) = x ++ reverse y ++ flatten z
flatten [x] = x
