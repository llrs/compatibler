license_dependencies <- function(pkg, ap){
    ts <- tools::package_dependencies(pkg, which = c("Depends", "Imports"))
    ap[ts$desc[unlist(ts, FALSE, FALSE) %in% rownames(ap)], "License"]
}

tidy <- function(license) {
c(name = , version, file)
}
combination <- function(license) {
    gsub(".*[+|].*", "\\1", license)
}

file <- function(license) {
    grepl("file", license, fixed = TRUE, ignore.case = TRUE)
}

clean_version <- function(license) {
    gsub("version", "", license, fixed = TRUE, ignore.case = TRUE)
    gsub("License", "", license, fixed = TRUE, ignore.case = TRUE)
}

detect_version <- function(license) {
    gsub(".+(-| )([0-9]{1,}\\.?[0-9]?\\.[0-9]?)", "\\2", license)
}

equal_major <- function(license) {
    gsub(".+(\\(>= ?(.+))).*", replacement = "\\2", license)
}
equal_equal <- function(x) {
    gsub(".+(\\(== ?(.+))).*", replacement = "\\2", x)
}
equal_minor <- function(license) {
    gsub(".+(\\(<= ?(.+))).*", replacement = "\\2", license)
}
