doc_helper <- function(
    DataProvider, TaxonomyId, Species, SourceUrl, SourceType, DataType, ...
) {
    args <- list(...)
    saf <- args[["stringsAsFactors"]]
    saf <- if(!is.null(saf)) saf else FALSE

    input_vals <- list(
        DataProvider = DataProvider, TaxonomyId = TaxonomyId,
        Species = Species, SourceUrl = SourceUrl,
        SourceType = SourceType, DataType = DataType
    )
    clens <- lengths(input_vals)
    zlen <- !clens
    if (any(zlen))
        stop(
            "Provide values for: ",
            paste(names(input_vals)[zlen], collapse = ", ")
        )

    nonstd <- !clens %in% c(max(clens), 1L)
    if (any(nonstd))
        stop("Lengths of inputs must either be 1 or the max length")

    input_vals[clens == 1L] <- lapply(input_vals[clens == 1L],
                                      function(x) {
                                          rep(x, max(clens))
                                      })

    as.data.frame(input_vals, stringsAsFactors = saf)
}
## alist() with formals()<-
## fancyFUN <- function() {}
## formals(fancyFUN) <- alist()

#' @importFrom BiocBaseUtils isTRUEorFALSE isScalarCharacter isCharacter
MetaHubCreate <- function(
    base_dir, data_dirs, ext_pattern, doc_file, split_by, version, pkg_name,
    remote, ...
) {
    stopifnot(
        dir.exists(base_dir), file.exists(doc_file), isTRUEorFALSE(remote),
        isScalarCharacter(base_dir), isScalarCharacter(doc_file),
        isScalarCharacter(version), isCharacter(split_by)
    )
    if (remote) {
        locations <- namelist <- fpathlist <- ""
        replengths <- 1L
    } else {
        locations <- file.path(base_dir, data_dirs, paste0("v", version))
        stopifnot(
            all(dir.exists(locations)),
            isScalarCharacter(ext_pattern)
        )
        fpathlist <- lapply(locations, function(locs) {
            list.files(
                locs, pattern = ext_pattern, full.names = TRUE, recursive = TRUE
            )
        })
        replengths <- lengths(fpathlist)
        namelist <- lapply(fpathlist, basename)
    }
    docFrame <- read.csv(doc_file, header = TRUE)
    docList <- split(docFrame, docFrame[, split_by])
    versions <- version
    if (!length(ext_pattern))
        ext_pattern <- ""

    .EHubMetaGenerate(
        PackageName = pkg_name,
        ext_pattern = ext_pattern, docList = docList,
        namelist = namelist, fpathlist = fpathlist, replengths = replengths,
        versions = versions
    )

}

#' Generate the metadata.csv file from a documentation file
#'
#' This function takes a specific folder structure and generates the
#' metadata.csv file for adding to ExperimentHub.
#'
#' @param directory The base folder for _all_ datasets
#'
#' @param dataDirs character() A vector of folder names contained in directory
#'     that corresponds to each project. For multiple versions, repeat the
#'     name of the folder.
#'
#' @param version character() A vector of subfolder versions that is parallel
#'     to `dataDirs` argument, typically `v1.0.0`.
#'
#' @param ext_pattern character(1) A string that matches files within the
#'     above folders to find the data.
#'
#' @param doc_file character(1) A path to the documentation `data.frame` that
#'     tells the function how to fill in the standard columns for data
#'     annotation, for example `DataProvider`, `TaxonomyId`, etc.
#'
#' @param pkg_name character(1) The name of the current package
#'
#' @param dry.run logical(1) Whether to (over)write the `metadata.csv` file or
#'     return as output.
#'
#' @param append logical(1) Whether to append to the current `metadata.csv`
#'     file
#'
#' @return Saves a file under `/inst/extdata/metadata.csv`
#'
#' @examples
#'
#' make_metadata(
#'     directory = "~/gh/CyCIFData",
#'     version = "1.0.0",
#'     remote = TRUE,
#'     split_by = "SourceVersion",
#'     doc_file = "inst/extdata/docuData/CyCIFData_v1.csv",
#'     dry.run = TRUE,
#' )
#'
#' if (interactive()) {
#'     make_metadata(
#'         directory = "~/data/scmm",
#'         dataDirs = "pbmc",
#'         version = "1.0.0",
#'         ext_pattern = "\\.[Rr][Dd][AaSs]$|\\.[Mm][Tt][Xx]\\.[Gg][Zz]$",
#'         doc_file = "inst/extdata/docuData/singlecellmultimodalv6.csv",
#'         pkg_name = "SingleCellMultiModal",
#'         dry.run = TRUE,
#'     )
#' }
#'
#' @md
#'
#' @export
make_metadata <- function(
    directory = ".",
    dataDirs = character(0L),
    version = "1.0.0",
    ext_pattern = character(0L),
    doc_file,
    split_by = "SourceVersion",
    remote = FALSE,
    dry.run = TRUE,
    append = FALSE,
    ...
) {
    pkg_name <- devtools::as.package(directory)[["package"]]
    exdata <- file.path(directory, "inst/extdata")
    doc_file <- file.path(directory, doc_file)

    if (!dir.exists(exdata))
        dir.create(exdata, recursive = TRUE)

    if (missing(doc_file) || !file.exists(doc_file))
        stop("Provide a 'doc_file' CSV for generating the metadata")

    metapath <- file.path(exdata, "metadata.csv")

    metadat <- MetaHubCreate(
        base_dir = directory,
        data_dirs = dataDirs,
        ext_pattern = ext_pattern,
        doc_file = doc_file,
        split_by = split_by,
        version = version,
        pkg_name = pkg_name,
        remote = remote,
        ...
    )

    if (!dry.run) {
        if (!append)
            file.remove(metapath)
        readr::write_csv(metadat, metafile, append = append, na="NA")
    }

    metadat
}

## Check metadata.csv file with:
## ExperimentHubData::makeExperimentHubMetadata(
##     file.path(Sys.getenv("HOME"), "gh/SingleCellMultiModal"), "metadata.csv"
## )
