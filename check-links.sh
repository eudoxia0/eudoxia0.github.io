#!/bin/sh

lychee --verbose \
       --fallback-extensions=html \
       --root-dir=_site \
       --host-concurrency=1 \
       --max-concurrency=128 \
       --max-retries=6 \
       --retry-wait-time=6 \
       --exclude="antipope\.org" \
       --exclude="dreamwidth\.org" \
       --exclude="fredkozlowski\.com" \
       --exclude="langsec\.org" \
       --exclude="www\.gnu\.org" \
       _site/
