module Util where

rad :: Floating a => a -> a
rad x = x / 180 * pi

deg :: Floating a => a -> a
deg x = x / pi * 180

sin' :: Floating a => a -> a
sin' = sin . rad

cos' :: Floating a => a -> a
cos' = cos . rad
