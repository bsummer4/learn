#!/usr/bin/env bash

set -e

stack build
node "$(stack path --local-install-root)/bin/ghcjs.jsexe/all.js"
