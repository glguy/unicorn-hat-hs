{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "ws2811.h"

module Ws2811 where

import Foreign
import Foreign.C

data Ws2811
data Ws2811Channel
type Ws2811Led = (#type ws2811_led_t)

targetFreq :: Word32
targetFreq = (#const WS2811_TARGET_FREQ)

ws2811_size :: Int
ws2811_size = (#size ws2811_t)

getBrightness :: Ptr Ws2811Channel -> IO CInt
getBrightness = (#peek ws2811_channel_t, brightness)

setBrightness :: Ptr Ws2811Channel -> CInt -> IO ()
setBrightness = (#poke ws2811_channel_t, brightness)

getCount :: Ptr Ws2811Channel -> IO CInt
getCount = (#peek ws2811_channel_t, count)

setCount :: Ptr Ws2811Channel -> CInt -> IO ()
setCount = (#poke ws2811_channel_t, count)

getInvert :: Ptr Ws2811Channel -> IO CInt
getInvert = (#peek ws2811_channel_t, invert)

setInvert :: Ptr Ws2811Channel -> CInt -> IO ()
setInvert = (#poke ws2811_channel_t, invert)

getGpioNum :: Ptr Ws2811Channel -> IO CInt
getGpioNum = (#peek ws2811_channel_t, gpionum)

setGpioNum :: Ptr Ws2811Channel -> CInt -> IO ()
setGpioNum = (#poke ws2811_channel_t, gpionum)

getLeds :: Ptr Ws2811Channel -> IO (Ptr Ws2811Led)
getLeds = (#peek ws2811_channel_t, leds)

getFreq :: Ptr Ws2811 -> IO Word32
getFreq = (#peek ws2811_t, freq)

setFreq :: Ptr Ws2811 -> Word32 -> IO ()
setFreq = (#poke ws2811_t, freq)

getDmaNum :: Ptr Ws2811 -> IO CInt
getDmaNum = (#peek ws2811_t, dmanum)

setDmaNum :: Ptr Ws2811 -> CInt -> IO ()
setDmaNum = (#poke ws2811_t, dmanum)

channelPtr :: Ptr Ws2811 -> Int -> Ptr Ws2811Channel
channelPtr p n = (#ptr ws2811_t, channel) p
                 `plusPtr`
                 (n * (#size ws2811_channel_t)) 
     

foreign import ccall safe ws2811_init   :: Ptr Ws2811 -> IO CInt
foreign import ccall safe ws2811_fini   :: Ptr Ws2811 -> IO ()
foreign import ccall safe ws2811_render :: Ptr Ws2811 -> IO CInt
foreign import ccall safe ws2811_wait   :: Ptr Ws2811 -> IO CInt
