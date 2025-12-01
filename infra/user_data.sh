#!/bin/bash
set -eux

LOG=/var/log/haskell-web-serv-user-data.log
exec > >(tee -a "$LOG") 2>&1

apt-get update
apt-get install -y docker.io git
systemctl enable docker
systemctl start docker

mkdir -p /opt/haskell-web-serv
cd /opt/haskell-web-serv

if [ ! -d repo ]; then
  git clone "${app_repo_url}" repo
else
  cd repo
  git fetch --all
  git reset --hard origin/main || true
  cd ..
fi

cd repo

docker build -t haskell-web-serv .

if docker ps -a --format '{{.Names}}' | grep -q '^haskell-web-serv$'; then
  docker stop haskell-web-serv || true
  docker rm haskell-web-serv || true
fi

docker run -d \
  --name haskell-web-serv \
  --restart unless-stopped \
  -p 80:8080 \
  -e JWT_SECRET="${jwt_secret}" \
  haskell-web-serv
