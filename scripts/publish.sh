#!/usr/bin/env bash
set -e

# 1. Render the book -> _book/
quarto render

# 2. Render every slide deck + PDF handout
for deck in slides/*.qmd; do
  quarto render "$deck"
done
Rscript -e 'for (f in Sys.glob("slides/*.html")) pagedown::chrome_print(paste0("file://", normalizePath(f), "?print-pdf"), output = sub("[.]html$", "_handout.pdf", f), verbose = 0)'

# 3. Copy decks and handouts into the published site
mkdir -p _book/slides
cp slides/*.html slides/*.pdf _book/slides/

echo "Site built in _book/  (slides at _book/slides/)"

# 4. Optionally: commit and push the sources, then publish the site
if [ "$1" = "--publish" ]; then
  git add -A
  git commit -m "Update course materials ($(date +%Y-%m-%d))" || echo "Nothing new to commit"
  git push
  quarto publish gh-pages --no-render --no-prompt
fi