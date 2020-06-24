license_dependencies <- function(pkg, ap){
    ts <- tools::package_dependencies(pkg, which = c("Depends", "Imports"))
    ap[ts$desc[unlist(ts, FALSE, FALSE) %in% rownames(ap)], "License"]
}

tidy <- function(license) {
    original <- license
    licenses <- trimws(name_license(license))
    f <- file(licenses)
    combination <- combination(license)
    lic <- licenses[!f]
    file <- any(f)
    l <- lapply(lic, function(x) {
        c(name = clean_version(x),
          version = detect_version(x),
          comparison = detect_comparison(x),
          accepted = detect_reference(x))
    })
}

combination <- function(license) {
    s <- strsplit(license, split = "")[[1]]
    s[s %in% c("|", "+")]
}

name_license <- function(license) {
    pos <- gregexpr( "\\+|\\|", license)[[1]]

    if (pos < 0) {
        pos <- NULL
    }
    pos <- c(0, pos, nchar(license)+1)
    v <- vector("character", length(pos) -1)
    for (i in seq(from = 2, to = length(pos))) {
        v[i-1] <- substr(license, pos[i -1]+1, pos[i]-1)
    }
    v[v!= ""]
}

file <- function(license) {
    grepl("file", license, fixed = TRUE)
}

clean <- function(license) {
    gsub("version|file|LICENSE|License", "", license, fixed = FALSE)
}

clean_version <- function(license) {
    gsub(" ?\\([><=] ?= ([0-9]+\\.?){1,}\\) ?", "", license)
}

detect_version <- function(license) {
    ver <- gsub(".+(-| )([0-9]{1,}\\.?[0-9]?\\.[0-9]?)", "\\2", license)
    if (!grepl("[0-9]", ver)) {
        NULL
    }
    ver
}

detect_comparison <- function(license) {
    x <- gsub(".+([><=]=).+", replacement = "\\1", license)
    if (x == license) {
        return(character(0))
    }
    x
}

detect_reference <- function(license) {
    gsub(".+(\\([><=]= ?(.+))).*", replacement = "\\2", license)
}

equal_major <- function(license) {
    gsub(".+(\\(>= ?(.+))).*", replacement = "\\2", license)
}
equal_equal <- function(license) {
    gsub(".+(\\(== ?(.+))).*", replacement = "\\2", license)
}
equal_minor <- function(license) {
    gsub(".+(\\(<= ?(.+))).*", replacement = "\\2", license)
}
