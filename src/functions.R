library(tidyverse)

sumna <- function(x) {sum(x, na.rm = T)}
maxna <- function(x) {max(x, na.rm = T)}
minna <- function(x) {min(x, na.rm = T)}
meanna <- function(x) {mean(x, na.rm = T)}
medianna <- function(x) {median(x, na.rm = T)}
sdna <- function(x) {sd(x, na.rm = T)}
rangena <- function(x) {range(x, na.rm = T)}

# Adds leading zeros to integers for nicer filenames
fix_int <- function(n, digits = 4) {
  vapply(n, function(x) {
    stringr::str_flatten(c(rep("0", digits-nchar(as.character(x))), as.character(x)))
  }, character(1))
}

# For citing R packages within a quarto document
cite_pack <- function(package_name) {
  version <- packageVersion(package_name)
  paste0("package `", package_name, "` version ", version, " [@", package_name, "]")
}

cite_packages <- function(packages) {
  # Generate citations for each package
  citations <- sapply(packages, function(pkg) {
    version <- packageVersion(pkg)
    paste0("`", pkg, "`, version ", version, " [@", pkg, "]")
  })
  
  # Handle different cases based on number of packages
  if (length(packages) == 1) {
    return(paste0("package ", citations[1]))
  } else if (length(packages) == 2) {
    return(paste0("packages ", citations[1], " and ", citations[2]))
  } else {
    # More than 2 packages: use commas and "and" before last
    first_part <- paste(citations[-length(citations)], collapse = ", ")
    last_part <- citations[length(citations)]
    return(paste0("packages ", first_part, ", and ", last_part))
  }
}
