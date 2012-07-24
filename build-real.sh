#!/bin/bash

cabal-dev add-source ../dsmc/ && cabal-dev install dsmc --reinstall && cabal-dev install
