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
    base_dir, data_dirs, ext_pattern, doc_file, split_by, version = "1.0.0",
    pkg_name, ...
) {
    stopifnot(
        dir.exists(base_dir), file.exists(doc_file),
        isScalarCharacter(base_dir), isScalarCharacter(doc_file),
        isScalarCharacter(version), isCharacter(split_by)
    )
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
    namelist <- lapply(fpathlist, basename)
    docFrame <- read.csv(doc_file, header = TRUE)
    DataTypes <- data_dirs
    docList <- split(docFrame, docFrame[, split_by])
    if ("SourceVersion" %in% split_by) {
        names(docList) <- gsub(paste0("\\.", version), "", names(docList))
        docList <- docList[DataTypes]
    }
    replengths <- vapply(docList, nrow, integer(1L))
    versions <- version
    if (!length(ext_pattern))
        ext_pattern <- ""

    metaList <- Map(
        function(DataType, doc_file, resnames, filepaths, replength, version) {

            message("Working on: ", basename(DataType), " v", version)

            hubmeta <- R6::R6Class(
                "EHubMeta",
                public = list(
                    Title = NA_character_,
                    Description = NA_character_,
                    BiocVersion = as.character(BiocManager::version()),
                    Genome = NA_character_,
                    SourceType = NA_character_,
                    SourceUrl = character(1L),
                    SourceVersion = version,
                    Species = character(1L),
                    TaxonomyId = character(1L),
                    Coordinate_1_based = NA,
                    DataProvider = character(1L),
                    Maintainer = NA_character_,
                    RDataClass = NA_character_,
                    DispatchClass = NA_character_,
                    Location_Prefix = NA_character_,
                    RDataPath = NA_character_,
                    ResourceName = NA_character_,
                    DataType = DataType,

                    initialize = function(doc_file)
                    {
                        lapply(names(doc_file), function(i) {
                            assign(i, doc_file[[i]], self)
                        })
                        if (any.na(self$Title))
                            self$Title <-
                                gsub(ext_pattern, "", basename(filepaths))
                        if (is.na(self$Description))
                            self$Description <- paste(
                                self$Title, "data specific to the",
                                toupper(self$DataType), "project"
                            )
                        if (any.na(self$DispatchClass) && length(filepaths))
                            self$DispatchClass <- .getDispatchClass(resnames)
                        if (any.na(self$SourceType))
                            self$SourceType <- .getSourceType(filepaths)
                        if (any.na(self$Maintainer))
                            self$Maintainer <- utils::maintainer(pkg_name)
                        if (any.na(self$RDataClass)) {
                            dataList <- .loadDataList(filepaths)
                            self$RDataClass <- .getRDataClass(dataList)
                        }
                        if (any.na(self$ResourceName) && length(resnames))
                            self$ResourceName <- resnames
                        else if (!length(resnames) && length(self$RDataPath))
                            self$ResourceName <- basename(self$RDataPath)
                        if (any.na(self$Location_Prefix))
                            self$Location_Prefix <- NULL
                        if (any.na(self$RDataPath))
                             self$RDataPath <- file.path(
                                pkg_name, self$DataType,
                                paste0("v", version), self$ResourceName
                            )
                    },
                    generate = function() {
                        lnames <- !names(self) %in%
                            c(".__enclos_env__", "clone", "generate",
                              "initialize")
                        initList <- mget(names(self)[lnames], envir = self)
                        initList <- Filter(function(x) !is.null(x), initList)
                        flist <- .stdLength(initList, replength)
                        do.call(data.frame, c(flist, stringsAsFactors = FALSE))
                    }
                ),
                lock_objects = FALSE
            )
            nhub <- hubmeta$new(doc_file)
            nhub$generate()
        }, DataType = DataTypes, doc_file = docList, resnames = namelist,
        filepaths = fpathlist, replength = replengths, version = versions
    )

    do.call(
        function(...) {
            rbind.data.frame(..., make.row.names = FALSE,
                             stringsAsFactors = FALSE)
        },
        metaList
    )

    # .EHubMetaGenerate(
    #     PackageName = pkg_name,
    #     ext_pattern = ext_pattern, docList = docList,
    #     namelist = namelist, fpathlist = fpathlist, replengths = replengths,
    #     versions = versions
    # )

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
#' if (interactive()) {
#'     ## Example for CyCIFData
#'     cy_meta <- make_metadata(
#'         pkg_dir = "~/gh/CyCIFData",
#'         data_dir = "~/data/CyCIFData",
#'         data_sub_dirs = c("HDF5", "Zarr", "Expression"),
#'         version = "1.0.0",
#'         ext_pattern = "\\.[Dd][Cc][Ff]",
#'         split_by = c("DataType", "SourceVersion"),
#'         doc_file = "inst/extdata/docuData/CyCIFData_v4.csv",
#'         dry.run = TRUE,
#'     )
#'     ## write documentation data.frame to disk
#'     write.csv(
#'       cy_meta,
#'       file = "~/gh/CyCIFData/inst/extdata/metadata.csv",
#'       row.names = FALSE
#'     )
#'     ## validate metadata.csv file
#'     ExperimentHubData::makeExperimentHubMetadata(
#'         "~/gh/CyCIFData", "metadata.csv"
#'     )
#'
#'     ## Example for SingleCellMultiModal
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
    pkg_dir = ".",
    data_dir = character(0L),
    data_sub_dirs = character(0L),
    version = "1.0.0",
    ext_pattern = character(0L),
    doc_file,
    split_by = "SourceVersion",
    dry.run = TRUE,
    append = FALSE,
    ...
) {
    pkg_name <- devtools::as.package(pkg_dir)[["package"]]
    exdata <- file.path(pkg_dir, "inst/extdata")
    doc_file <- file.path(pkg_dir, doc_file)

    if (!dir.exists(exdata))
        dir.create(exdata, recursive = TRUE)

    if (missing(doc_file) || !file.exists(doc_file))
        stop("Provide a 'doc_file' CSV for generating the metadata")

    metapath <- file.path(exdata, "metadata.csv")

    metadat <- MetaHubCreate(
        base_dir = data_dir,
        data_dirs = data_sub_dirs,
        ext_pattern = ext_pattern,
        doc_file = doc_file,
        split_by = split_by,
        version = version,
        pkg_name = pkg_name,
        ...
    )

    if (!dry.run) {
        if (!append)
            file.remove(metapath)
        readr::write_csv(metadat, metafile, append = append, na="NA")
    }

    metadat
}
