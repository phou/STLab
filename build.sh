#!/bin/bash

set -euo pipefail

# Backup effpi and Teatrino
cp -r effpi effpi_bak
cp -r Teatrino Teatrino_bak

# Build the docker image
docker build --no-cache -t ecoop23-artefact .
docker save ecoop23-artefact | gzip > artefact.tar.gz

# Restore backups
rm -rf effpi && mv effpi_bak effpi
rm -rf Teatrino && mv Teatrino_bak Teatrino
