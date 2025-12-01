terraform {
  required_version = ">= 1.6.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = ">= 5.0"
    }
  }
}

provider "aws" {
  region = var.region
}

resource "aws_lightsail_container_service" "svc" {
  name  = var.service_name
  power = var.power
  scale = var.scale

  tags = {
    Project = var.project
    Env     = var.env
  }
}

resource "aws_lightsail_container_service_deployment_version" "svc_deploy" {
  service_name = aws_lightsail_container_service.svc.name

  container {
    container_name = "app"
    image          = var.image

    command = var.command

    environment = merge(
      var.environment,
      length(var.jwt_secret) > 0 ? { JWT_SECRET = var.jwt_secret } : {}
    )

    ports = {
      "${var.container_port}" = "HTTP"
    }
  }

  public_endpoint {
    container_name = "app"
    container_port = var.container_port

    health_check {
      path                = var.health_check_path
      interval_seconds    = 10
      timeout_seconds     = 5
      healthy_threshold   = 2
      unhealthy_threshold = 2
    }
  }
}

output "lightsail_container_service_name" {
  value = aws_lightsail_container_service.svc.name
}

output "lightsail_container_service_url" {
  value = aws_lightsail_container_service.svc.url
}
