module UnicornHat where

import Foreign
import Foreign.Storable
import Control.Exception
import Ws2811

unicornHatDma = 5
unicornHatGpio = 18
unicornHatLeds = 64

withHat :: (Ptr Ws2811 -> IO ()) -> IO ()
withHat k =
  allocaBytes ws2811_size $ \ptr ->
    do setFreq ptr targetFreq
       setDmaNum ptr unicornHatDma

       let chan0 = channelPtr ptr 0
       setGpioNum    chan0 unicornHatGpio
       setCount      chan0 unicornHatLeds
       setBrightness chan0 255
       setInvert     chan0 0

       let chan1 = channelPtr ptr 1
       setGpioNum    chan1 0
       setCount      chan1 0
       setBrightness chan1 0
       setInvert     chan1 0

       bracket_ (ws2811_init ptr) (ws2811_fini ptr) (k ptr)

