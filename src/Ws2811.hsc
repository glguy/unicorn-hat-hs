{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE ForeignFunctionInterface #-}
#include "ws2811.h"
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)

module Ws2811 where

import Foreign
import Foreign.C
import Control.Applicative

data Ws2811Device

data Ws2811 = Ws2811
  { freq :: Word32
  , dmaNum :: CInt
  , device :: Ptr Ws2811Device
  , channel0 :: Ws2811Channel
  , channel1 :: Ws2811Channel
  }

defaultWs2811 :: Ws2811
defaultWs2811 = Ws2811
  { freq   = targetFreq
  , dmaNum = 0
  , device = nullPtr
  , channel0 = defaultChannel
  , channel1 = defaultChannel
  }


instance Storable Ws2811 where
  sizeOf _ = (#size ws2811_t)
  alignment _ = (#alignment ws2811_t)
  peek p = Ws2811
       <$> getFreq p
       <*> getDmaNum p
       <*> getDevice p
       <*> peek (channelPtr p 0)
       <*> peek (channelPtr p 1)
  poke p w =
    do setFreq p (freq w)
       setDmaNum p (dmaNum w)
       setDevice p (device w)
       poke (channelPtr p 0) (channel0 w)
       poke (channelPtr p 1) (channel1 w)
                  
getDevice :: Ptr Ws2811 -> IO (Ptr Ws2811Device)
getDevice = (#peek ws2811_t, device)

setDevice :: Ptr Ws2811 -> Ptr Ws2811Device -> IO ()
setDevice = (#poke ws2811_t, device)

getFreq :: Ptr Ws2811 -> IO Word32
getFreq = (#peek ws2811_t, freq)

setFreq :: Ptr Ws2811 -> Word32 -> IO ()
setFreq = (#poke ws2811_t, freq)

getDmaNum :: Ptr Ws2811 -> IO CInt
getDmaNum = (#peek ws2811_t, dmanum)

setDmaNum :: Ptr Ws2811 -> CInt -> IO ()
setDmaNum = (#poke ws2811_t, dmanum)

channelPtr :: Ptr Ws2811 -> Int -> Ptr Ws2811Channel
channelPtr = advancePtr . (#ptr ws2811_t, channel)

data Ws2811Channel = Ws2811Channel
  { gpioNum, invert, count, brightness :: CInt
  , leds :: Ptr Ws2811Led
  }

defaultChannel :: Ws2811Channel
defaultChannel = Ws2811Channel
  { gpioNum    = 0
  , count      = 0
  , brightness = 0
  , invert     = 0
  , leds       = nullPtr
  }

instance Storable Ws2811Channel where
  sizeOf _  = (#size ws2811_channel_t)
  alignment _ = (#alignment ws2811_channel_t)
  peek p = Ws2811Channel
       <$> getGpioNum p
       <*> getInvert p
       <*> getCount p
       <*> getBrightness p
       <*> getLeds p
  poke p c =
    do setGpioNum p (gpioNum c)
       setCount p (count c)
       setInvert p (invert c)
       setBrightness p (brightness c)
       setLeds p (leds c)

type Ws2811Led = (#type ws2811_led_t)

targetFreq :: Word32
targetFreq = (#const WS2811_TARGET_FREQ)

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

setLeds :: Ptr Ws2811Channel -> Ptr Ws2811Led -> IO ()
setLeds = (#poke ws2811_channel_t, leds)
     
foreign import ccall safe ws2811_init   :: Ptr Ws2811 -> IO CInt
foreign import ccall safe ws2811_fini   :: Ptr Ws2811 -> IO ()
foreign import ccall safe ws2811_render :: Ptr Ws2811 -> IO CInt
foreign import ccall safe ws2811_wait   :: Ptr Ws2811 -> IO CInt
