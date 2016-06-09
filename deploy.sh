#!/bin/sh

git checkout gh-pages

git merge master --no-commit

elm-make Main.elm --output=index.html

git add .

<<<<<<< HEAD
git commit -m "deploy"
=======
git commit
>>>>>>> master

git push origin gh-pages
