#!/usr/bin/env Rscript

# requirements.R
# Download source tarballs into ./r_packages (skip existing)
# Then install only packages that are not already installed.

target_dir <- "r_packages"
cran_repo  <- "https://cloud.r-project.org"

# Package list
all_pkgs <- c(
  "classInt", "crosstalk", "e1071", "lazyeval", "leaflet",
  "leaflet.providers", "png", "proxy", "raster", "s2",
  "S7", "sf", "sp", "terra", "tigris", "units", "wk",
  "stringr", "dplyr", "vroom", "httr", "tidyverse",
  "RColorBrewer", "tibble", "jsonlite", "tidyr", "purrr",
  "readr", "ggplot2"
)

# Remove duplicates
all_pkgs <- unique(all_pkgs)

# Create target directory
if (!dir.exists(target_dir)) {
  dir.create(target_dir, recursive = TRUE)
}

# Identify missing tarballs
missing_pkgs <- c()
for (pkg in all_pkgs) {
  existing <- list.files(target_dir, pattern = sprintf("^%s_.*\\.tar\\.gz$", pkg))
  if (length(existing) > 0) {
    message("✔ ", pkg, " already downloaded (skipping)")
  } else {
    missing_pkgs <- c(missing_pkgs, pkg)
  }
}

# Download missing packages
if (length(missing_pkgs) > 0) {
  message("📥 Downloading ", length(missing_pkgs), " missing packages ...")
  dl <- download.packages(
    pkgs    = missing_pkgs,
    destdir = target_dir,
    type    = "source",
    repos   = cran_repo,
    quiet   = FALSE
  )
} else {
  message("✅ All packages already present.")
}

# Get list of available tarballs
all_tarballs <- list.files(target_dir, pattern = "\\.tar\\.gz$", full.names = TRUE)

if (length(all_tarballs) == 0) {
  stop("No tarballs found in ", target_dir)
}

# Identify which packages are already installed
installed_pkgs <- installed.packages()[, "Package"]
pkgs_to_install <- c()

for (tarball in all_tarballs) {
  # Extract package name from tarball filename (format: pkg_version.tar.gz)
  pkg_name <- sub("_.*", "", basename(tarball))
  if (pkg_name %in% installed_pkgs) {
    message("✔ ", pkg_name, " already installed (skipping)")
  } else {
    pkgs_to_install <- c(pkgs_to_install, tarball)
  }
}

# Install only missing packages
if (length(pkgs_to_install) > 0) {
  message("🔧 Installing ", length(pkgs_to_install), " missing packages from local source tarballs ...")
  install.packages(pkgs_to_install, repos = NULL, type = "source")
  message("🎉 Installation complete.")
} else {
  message("✅ All packages already installed.")
}