variable "region" {
  type        = string
  description = "AWS region"
  default     = "ap-northeast-1"
}

variable "project" {
  type        = string
  description = "Project name tag"
  default     = "mnist-web"
}

variable "env" {
  type        = string
  description = "Environment name"
  default     = "dev"
}

variable "service_name" {
  type        = string
  description = "Lightsail container service name"
  default     = "mnist-web-dev"
}

variable "power" {
  type        = string
  description = "Lightsail container power (nano/micro/small/medium/large/xlarge)"
  default     = "nano"
}

variable "scale" {
  type        = number
  description = "Number of container instances"
  default     = 1
}

variable "container_port" {
  type        = number
  description = "Port exposed by the container and service public endpoint"
  default     = 8080
}

variable "health_check_path" {
  type        = string
  description = "Health check path for public endpoint"
  default     = "/"
}

variable "image" {
  type        = string
  description = "Lightsail container image id (output of aws lightsail push-container-image)"
}

variable "command" {
  type        = list(string)
  description = "Override container command (empty uses image default)"
  default     = []
}

variable "environment" {
  type        = map(string)
  description = "Base environment variables for the container (without JWT_SECRET)"
  default     = {}
}

variable "jwt_secret" {
  type        = string
  description = "JWT secret to inject into container environment"
  default     = ""
}
