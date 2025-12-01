terraform {
  required_version = ">= 1.5.0"

  required_providers {
    aws = {
      source  = "hashicorp/aws"
      version = "~> 5.0"
    }
  }
}

provider "aws" {
  region = var.aws_region
}

resource "aws_lightsail_instance" "haskell_web_serv" {
  name              = "haskell-web-serv"
  availability_zone = "${var.aws_region}a"
  blueprint_id      = "ubuntu_22_04"
  bundle_id         = "nano_3_0"
  key_pair_name     = var.key_pair_name

  user_data = templatefile(
    "${path.module}/user_data.sh",
    {
      app_repo_url = var.app_repo_url
      jwt_secret   = var.jwt_secret
    }
  )
}

resource "aws_lightsail_instance_public_ports" "haskell_web_serv" {
  instance_name = aws_lightsail_instance.haskell_web_serv.name

  port_info {
    from_port = 22
    to_port   = 22
    protocol  = "tcp"
  }

  port_info {
    from_port = 80
    to_port   = 80
    protocol  = "tcp"
  }
}

output "public_ip" {
  value       = aws_lightsail_instance.haskell_web_serv.public_ip_address
  description = "Public IP of haskell-web-serv Lightsail instance"
}
