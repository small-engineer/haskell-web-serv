#!/bin/sh
set -eu

APP_BASE=/opt/haskell-web-serv
APP_DIR="$APP_BASE/repo"
REPO_URL="https://github.com/small-engineer/haskell-web-serv.git"
CNTR="haskell-web-serv"

mkdir -p "$APP_BASE"

if [ ! -d "$APP_DIR/.git" ]; then
  rm -rf "$APP_DIR"
  git clone "$REPO_URL" "$APP_DIR"
fi

cd "$APP_DIR"

git fetch origin
git reset --hard origin/main

docker rm -f "$CNTR" 2>/dev/null || true
docker rmi "$CNTR:latest" 2>/dev/null || true

docker build -t "$CNTR:latest" .

docker run -d \
  --name "$CNTR" \
  --restart unless-stopped \
  -p 80:8080 \
  -e "JWT_SECRET=${JWT_SECRET:-changeme}" \
  "$CNTR:latest"
