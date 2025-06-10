terraform {
  required_providers {
    azurerm = {
      source  = "hashicorp/azurerm"
      version = "~> 4.0"
    }
  }
}

variable "subscription_id" {
  description = "The Azure subscription ID to use for the resources."
  type        = string
}

variable "environment_vars" {
  description = "Environment variables to set in the container app."
  type        = map(string)
  default = {
    APP__NAME                                = "hCommenter-Api"
    APP__ENVIRONMENT                         = "Test"
    APP__PORT                                = 80
    APP__BACKEND                             = "SQLite"
    LOGGING__SEVERITY                        = "Info"
    LOGGING__VERBOSITY                       = "V0"
    LOGGING__GRAFANA_URL                     = "logs-prod-026.grafana.net"
    LOGGING__GRAFANA_ACC                     = 1094195
    LOGGING__ENABLE_LOG_DEBUGGING_IN_CONSOLE = false
  }
}

variable "secrets" {
  description = "Secrets to set in the container app."
  sensitive   = true
  type = map(object({
    env_name = string
    secret   = string
  }))
  default = {
    grafana-token = {
      env_name = "LOGGING__GRAFANA_TOKEN"
      secret   = "" # To be populated later
    }
  }
}

provider "azurerm" {
  features {}
  subscription_id                 = var.subscription_id
  resource_provider_registrations = "core"
  resource_providers_to_register = [
    "Microsoft.App",
    "Microsoft.OperationalInsights"
  ]
}

resource "azurerm_resource_group" "hcommenter" {
  name     = "hcommenter-test-rg"
  location = "Australia East"

  tags = {
    environment = "test"
    project     = "hcommenter"
  }
}

resource "azurerm_log_analytics_workspace" "hcommenter" {
  name                = "hcommenter-test-law"
  location            = azurerm_resource_group.hcommenter.location
  resource_group_name = azurerm_resource_group.hcommenter.name
  sku                 = "PerGB2018"

  retention_in_days = 30
  tags              = azurerm_resource_group.hcommenter.tags
}

resource "azurerm_container_app_environment" "hcommenter" {
  name                       = "hcommenter-test-app-env"
  location                   = azurerm_resource_group.hcommenter.location
  resource_group_name        = azurerm_resource_group.hcommenter.name
  log_analytics_workspace_id = azurerm_log_analytics_workspace.hcommenter.id

  tags = azurerm_resource_group.hcommenter.tags
}

resource "azurerm_container_app" "hcommenter" {
  name                         = "hcommenter-test-api-app"
  resource_group_name          = azurerm_resource_group.hcommenter.name
  container_app_environment_id = azurerm_container_app_environment.hcommenter.id
  revision_mode                = "Single"

  tags = azurerm_resource_group.hcommenter.tags

  dynamic "secret" {
    for_each = var.secrets

    content {
      name  = secret.key
      value = secret.value.secret
    }
  }

  template {
    revision_suffix = "template"

    container {
      name   = "hcommenter-test-api-app"
      image  = "nginx:alpine" # Minimal image for initial deploy
      cpu    = "0.25"
      memory = "0.5Gi"

      // App startup check
      startup_probe {
        path          = "/health"
        port          = 80
        transport     = "HTTP"
        initial_delay = 10
      }

      // Per revision
      readiness_probe {
        path          = "/health"
        port          = 80
        transport     = "HTTP"
        initial_delay = 10
      }

      // Regular checks
      liveness_probe {
        path          = "/health"
        port          = 80
        transport     = "HTTP"
        initial_delay = 20
      }

      dynamic "env" {
        for_each = var.environment_vars

        content {
          name  = env.key
          value = env.value
        }
      }

      dynamic "env" {
        for_each = var.secrets

        content {
          name        = env.value.env_name
          secret_name = env.key
        }
      }
    }

    min_replicas = 0
    max_replicas = 1
  }

  ingress {
    external_enabled           = true
    allow_insecure_connections = true
    target_port                = 80

    traffic_weight { # Note: irrelevant as revision_mode is Single
      latest_revision = true
      percentage      = 100
    }
  }

  identity {
    type = "SystemAssigned"
  }
}

