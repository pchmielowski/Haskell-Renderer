#!/bin/sh

stack build --ghc-options="-Wall" && stack exec raytracer-exe # && eog image.ppm

