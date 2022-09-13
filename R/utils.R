.file_pattern_map <- data.frame(
    ext_pattern = paste0(
        c("[Rr][Dd][Aa]", "[Rr][Dd][Ss]", "[Hh]5", "[Mm][Tt][Xx]\\.[Gg][Zz]"),
        "$"
    ),
    ## currently MTX DispatchClass recipe unavailable
    Dispatch = c("Rda", "Rds", "H5File", "FilePath"),
    stringsAsFactors = FALSE
)

.getDispatchClass <- function(resource_files, ext_map = .file_pattern_map) {
    hitMatrix <- vapply(
        ext_map[["ext_pattern"]],
        function(pat) grepl(pat, resource_files),
        logical(length(resource_files))
    )
    ext_map[["Dispatch"]][apply(hitMatrix, 1L, which)]
}

.get_Description <- function(data_name) {
    paste(data_name, "data specific to the project")
}

.getSourceType <- function(filepaths) {
    lfiles <- strsplit(basename(filepaths), "\\.")
    exts <- vapply(lfiles,
                   function(x) { paste(x[-1], collapse = ".") }, character(1L))
    uexts <- toupper(exts)
    uexts <- gsub("[Hh]5", "HDF5", uexts)
    uexts <- gsub("[Mm][Tt][Xx]\\.[Gg][Zz]", "MTX", uexts)
    vTypes <- AnnotationHubData::getValidSourceTypes()
    uTypes <- toupper(vTypes)
    allvalid <- all(uexts %in% uTypes)
    if (!allvalid)
        stop(
            "Source types not supported: ", paste0(exts[!allvalid],
            collapse = ", "), "\n See 'AnnotationHubData::getValidSources()'",
            call. = FALSE
        )
    vTypes[match(uexts, uTypes)]
}

.loadDataList <- function(filepaths) {
    recipelist <- list(
        "\\.[Rr][Dd][Aa]" = .loadRDA,
        "\\.[Rr][Dd][Ss]" = .loadRDS,
        "\\.[Hh]5" = .loadH5,
        "\\.[Mm][Tt][Xx]\\.[Gg][Zz]" = .loadMTX.GZ
    )
    hitMatrix <- vapply(names(recipelist),
                        function(pat) grepl(pat, filepaths),
                        logical(length(filepaths))
    )
    allrecipes <- recipelist[apply(hitMatrix, 1L, which)]
    Map(function(x, y) { x(y) }, x = allrecipes, y = filepaths)
}

.stdLength <- function(metalist, replength) {
    lapply(metalist, function(field) {
        if (length(field) == 1L && length(replength))
            rep(field, replength)
        else
            field
    })
}

.loadRDS <- function(filepath) {
    readRDS(filepath)
}

.loadRDA <- function(filepath) {
    basefile <- gsub("\\.[Rr][Dd][Aa]", "", basename(filepath))
    OBJENV <- new.env(parent = emptyenv())
    load(filepath, envir = OBJENV)
    OBJENV[[basefile]]
}

.loadH5 <- function(filepath) {
    if (grepl("tenx", filepath))
        HDF5Array::TENxMatrix(filepath, "pbmc")
    else
        HDF5Array::HDF5Array(filepath, "assay001")
}

.loadMTX.GZ <- function(filepath) {
    .read_mtx(filepath)
}

any.na <- function(x) {
    any(is.na(x))
}

.getRDataClass <- function(dataList) {
    vapply(dataList, function(dataName) {
        if (is.matrix(dataName))
            "matrix"
        else
            class(dataName)
    }, character(1L))
}
