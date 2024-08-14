#!/bin/bash

set -eu

# Start up docker-buildx
docker run --rm --privileged tonistiigi/binfmt:latest --install all
docker buildx create --driver docker-container --use

# Full base image is just all the packages in packages.txt
docker buildx build --no-cache --file ./Dockerfile.base --platform linux/amd64,linux/arm64 --tag ghcr.io/saethlin/crates-build-env:latest --push .

