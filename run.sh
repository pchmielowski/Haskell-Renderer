#!/bin/sh

stack build --ghc-options="-Wall -Werror" && stack exec raytracer-exe # && eog image.ppm

