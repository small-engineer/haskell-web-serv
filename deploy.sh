#!/bin/sh

set -eu

ROOT_DIR=$(cd "$(dirname "$0")" && pwd)

SERVICE_NAME="mnist-web-dev"
AWS_REGION="ap-northeast-1"
LOCAL_IMAGE_NAME="mnist-web-app"
TF_VARS_FILE="main.tfvars"
INFRA_DIR="${ROOT_DIR}/infra"

echo "==> Terraform: Lightsail コンテナサービスの存在確認／作成"

cd "${INFRA_DIR}"
terraform init -input=false

terraform apply \
  -auto-approve \
  -var-file="${TF_VARS_FILE}" \
  -target="aws_lightsail_container_service.svc"

cd "${ROOT_DIR}"

echo "==> Docker image build (linux/amd64)"

docker buildx build \
  --platform linux/amd64 \
  -t "${LOCAL_IMAGE_NAME}:latest" \
  --load \
  "${ROOT_DIR}"

echo "==> JWT_SECRET 生成"

if command -v openssl >/dev/null 2>&1; then
  JWT_SECRET=$(openssl rand -hex 32)
else
  JWT_SECRET=$(hexdump -n 32 -v -e '/1 "%02x"' /dev/urandom)
fi

JWT_LEN=$(printf '%s' "${JWT_SECRET}" | wc -c | tr -d ' ')
echo "JWT_SECRET length: ${JWT_LEN}"

echo "==> Docker image を Lightsail に push 中..."

aws lightsail push-container-image \
  --region "${AWS_REGION}" \
  --service-name "${SERVICE_NAME}" \
  --label "${LOCAL_IMAGE_NAME}" \
  --image "${LOCAL_IMAGE_NAME}:latest"

echo "==> 最新イメージ ID を取得中..."

IMAGE_ID=$(
  aws lightsail get-container-images \
    --region "${AWS_REGION}" \
    --service-name "${SERVICE_NAME}" \
    --query 'containerImages[-1].image' \
    --output text
)

if [ -z "${IMAGE_ID}" ] || [ "${IMAGE_ID}" = "None" ]; then
  echo "ERROR: 取得できるコンテナイメージがありません" >&2
  exit 1
fi

echo "==> image id: ${IMAGE_ID}"

cd "${INFRA_DIR}"

echo "==> Terraform apply"

terraform apply \
  -auto-approve \
  -var-file="${TF_VARS_FILE}" \
  -var="image=${IMAGE_ID}" \
  -var="jwt_secret=${JWT_SECRET}"

echo "==> デプロイ完了"
