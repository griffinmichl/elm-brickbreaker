#!/bin/sh

git checkout gh-pages

git merge master --no-commit

elm-make Main.elm --output=index.html

git add .

git commit -m "deploy"

git push origin gh-pages
