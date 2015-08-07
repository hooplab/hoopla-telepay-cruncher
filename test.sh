#!/bin/sh
cabal build && ./dist/build/hoopla-telepay-cruncher/hoopla-telepay-cruncher summarize testinput_proper
