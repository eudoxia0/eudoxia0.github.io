#!/usr/bin/env bash

rm -rf _site
gem exec jekyll serve --livereload --future --incremental
