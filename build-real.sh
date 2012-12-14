#!/bin/bash

cabal-dev add-source ../dsmc/ ../simple-vec3/ ../csg/ && cabal-dev install simple-vec3 csg dsmc dsmc-tools --reinstall
