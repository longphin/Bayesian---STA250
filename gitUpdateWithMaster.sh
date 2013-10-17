#!/bin/bash
git pull

git remote -v
git fetch upstream
git branch -va

git merge upstream/master
git push

