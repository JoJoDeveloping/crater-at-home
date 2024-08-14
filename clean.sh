#!/bin/bash
cd output
rm -rf ./*
git reset --hard ef8baf51c4896de7c65d18b5d0674d574429e26a #$(git rev-list --max-parents=0 --abbrev-commit HEAD)
git push -f
