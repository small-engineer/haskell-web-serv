#!/bin/bash
set -eux

APP_DIR=/opt/haskell-web-serv/repo

if [ ! -d "$APP_DIR" ]; then
  echo "ERROR: $APP_DIR が存在しません" >&2
  exit 1
fi

cd "$APP_DIR"

# 最新 main を取得
git fetch origin
git checkout main
git reset --hard origin/main

# Docker が無ければインストール（初回保険）
if ! command -v docker >/dev/null 2>&1; then
  apt-get update
  DEBIAN_FRONTEND=noninteractive apt-get install -y docker.io
  systemctl enable docker
  systemctl start docker
fi

# JWT_SECRET をサーバー側の .env から読み込む
ENV_FILE=/opt/haskell-web-serv/.env

if [ -f "$ENV_FILE" ]; then
  . "$ENV_FILE"
fi

if [ -z "${JWT_SECRET:-}" ]; then
  echo "ERROR: JWT_SECRET が設定されていません (.env か環境変数で指定してください)" >&2
  exit 1
fi

# イメージビルド
docker build -t haskell-web-serv "$APP_DIR"

# 既存コンテナ停止・削除
if docker ps -a --format '{{.Names}}' | grep -q '^haskell-web-serv$'; then
  docker rm -f haskell-web-serv || true
fi

# 再起動
docker run -d \
  --name haskell-web-serv \
  --restart unless-stopped \
  -p 80:8080 \
  -e JWT_SECRET="$JWT_SECRET" \
  haskell-web-serv
