#!/usr/bin/env bash

rm -rf _site
bundler exec jekyll serve --livereload --future --drafts
