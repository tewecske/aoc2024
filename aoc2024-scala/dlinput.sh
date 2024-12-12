#!/bin/bash

mkdir src/main/resources/aoc/day$1
curl "https://adventofcode.com/2024/day/$1/input" \
  -H 'accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8,application/signed-exchange;v=b3;q=0.7' \
  -H 'accept-language: en-US,en;q=0.9,hu;q=0.8' \
  -H 'cache-control: max-age=0' \
  -H 'cookie: _ga=GA1.2.907846465.1733076998; _gid=GA1.2.2058882271.1733076998; session=53616c7465645f5f1b9c1eddce74df3df1d8fe42461c0b420dc5eff6115d5ef27a235f498ceb03d41bb60ea7aee4e1747e5b254744b53b810ef19986a656de6f; _gat=1; _ga_MHSNPJKWC7=GS1.2.1733943714.37.0.1733943714.0.0.0' \
  -H "referer: https://adventofcode.com/2024/day/$1" \
  -H "Referer: https://adventofcode.com/2024/day/$1/input" > src/main/resources/aoc/day$1/day${1}_input

