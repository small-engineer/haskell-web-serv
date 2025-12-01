variable "aws_region" {
  type        = string
  default     = "ap-northeast-1"
  description = "AWS region to deploy Lightsail instance"
}

variable "key_pair_name" {
  type        = string
  description = "Existing Lightsail/EC2 key pair name for SSH"
}

variable "app_repo_url" {
  type        = string
  description = "Git repository URL of haskell-web-serv (this repo)"
  default     = "https://github.com/small-engineer/haskell-web-serv.git"
}

variable "jwt_secret" {
  type        = string
  sensitive   = true
  description = "JWT secret used by the application"
}
