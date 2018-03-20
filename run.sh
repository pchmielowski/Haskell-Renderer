#!/bin/sh

stack build && stack exec raytracer-exe # && eog image.ppm
