.EHubMeta <- function(
    PackageName, DataType, doc_file, ext_pattern, resnames, filepaths,
    replength, version, ...
) {
    message("Working on: ", basename(DataType), " v", version)
    hubmeta <- R6::R6Class("EHubMeta",
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
            DispatchClass = .getDispatchClass(resnames),
            Location_Prefix = NA_character_,
            RDataPath = NA_character_,
            ResourceName = resnames,
            DataType = DataType,

            initialize = function(doc_file)
            {
                lapply(names(doc_file), function(i) {
                    assign(i, doc_file[[i]], self)
                })
                if (is.na(self$Title))
                    self$Title <- gsub(ext_pattern, "", basename(filepaths))
                if (is.na(self$Description))
                    self$Description <- .get_Description(
                        self$Title, toupper(self$DataType)
                    )
                if (any.na(self$SourceType))
                    self$SourceType <- .getSourceType(filepaths)
                if (any.na(self$SourceVersion))
                    self$SourceVersion <- "1.0.0"
                if (any.na(self$Maintainer))
                    self$Maintainer <- utils::maintainer(PackageName)
                if (any.na(self$RDataClass)) {
                    dataList <- .loadDataList(filepaths)
                    self$RDataClass <- .getRDataClass(dataList)
                }
                if (is.na(self$Location_Prefix))
                    self$Location_Prefix <- NULL
                if (is.na(self$RDataPath))
                    self$RDataPath <- file.path(
                        PackageName,
                        self$DataType, paste0("v", version),
                        self$ResourceName
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
}
