#!/bin/bash

# Exit on any error
set -e

# Version and commit details
JUST_VERSION=1.25.2
INSTALL_COMMIT=5695384271b99f64dbb4f543f3975beb8d9c1d99
INSTALL_URL="https://raw.githubusercontent.com/casey/just/${INSTALL_COMMIT}/www/install.sh"
INSTALL_SHA256="2f811850e7833bf2191df55683f861d09b8a9cd2d1aac5f2adff597b3d675aa4"
INSTALL_FILE="install.sh"

# Download the install script
echo "Downloading install.sh from commit ${INSTALL_COMMIT}..."
curl -fsSL "$INSTALL_URL" -o "$INSTALL_FILE"

# Verify the script's integrity
echo "Verifying SHA256 checksum..."
echo "$INSTALL_SHA256  $INSTALL_FILE" | sha256sum -c -

# Run the installation
echo "Installing Just version ${JUST_VERSION}..."
JUST_INSTALL_VERSION=$JUST_VERSION bash "$INSTALL_FILE"

echo "Adding executable to PATH..."
export PATH="$PATH:$HOME/bin"

echo "Moving to /usr/bin..."
sudo mv "$HOME/bin/just" /usr/bin/just

echo "Installed Just version $(just --version)."
echo "At $(which just)"

# Clean up
echo "Cleaning up..."
rm "$INSTALL_FILE"
