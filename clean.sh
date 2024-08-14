#!/bin/bash
cd output
rm -rf ./*
git reset --hard $(git rev-list --max-parents=0 --abbrev-commit HEAD)
git push -f
