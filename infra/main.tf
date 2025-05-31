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

  template {
    container {
      name   = "hcommenter"
      image  = "ghcr.io/cekkel/hcommenter-api:main"
      cpu    = "0.25"
      memory = "0.5Gi"

      // App startup check
      startup_probe {
        path          = "/health"
        port          = 8080
        transport     = "HTTP"
        initial_delay = 10
      }

      // Per revision
      readiness_probe {
        path          = "/health"
        port          = 8080
        transport     = "HTTP"
        initial_delay = 10
      }

      // Regular checks
      liveness_probe {
        path          = "/health"
        port          = 8080
        transport     = "HTTP"
        initial_delay = 20
      }
    }

    min_replicas = 0
    max_replicas = 1
  }

  ingress {
    external_enabled           = true
    allow_insecure_connections = true
    target_port                = 8080

    traffic_weight { # Note: irrelevant as revision_mode is Single
      latest_revision = true
      percentage      = 100
    }
  }

  identity {
    type = "SystemAssigned"
  }
}

