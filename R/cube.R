#' @import R6
#' @import rtson
#' @export
Cube = R6Class(
  "Cube",
  public = list(
    rowData = NULL,
    colData = NULL,
    data = NULL,
    dataNames = NULL,

    initialize = function(rowData=NULL,colData=NULL,dataNames=NULL,data=NULL,json=NULL){

      if (is.null(json)){
        self$rowData = rowData
        self$colData = colData
        self$dataNames = dataNames
        self$data = data
      } else {
        self$rowData = AnnotatedFrame$new(json=json$rowData)
        self$colData = AnnotatedFrame$new(json=json$colData)
        self$dataNames = as.character(json$dataNames)
        self$data = lapply(json$data, matrix.fromTSON  )
      }

      if (is.null(self$rowData)) stop('rowData must not be null')
      if (is.null(self$colData)) stop('colData must not be null')
      if (is.null(self$dataNames)) stop('dataNames must not be null')
      if (is.null(self$data)) stop('data must not be null')

      if (!inherits(self$rowData,'AnnotatedFrame' )) stop('rowData must be an AnnotatedFrame')
      if (!inherits(self$colData,'AnnotatedFrame' )) stop('colData must be an AnnotatedFrame')

      if (!is.character(self$dataNames)) stop('dataNames must be a character')
      if (length(self$dataNames) < 1) stop('dataNames is empty')

      if (!is.list(self$data)) stop('data must be a list of matrix')

      lapply(self$data, function(each){if (!is.matrix(each)) stop('data must be a list of matrix')})

      if (length(self$dataNames) != length(self$data)) stop('dataNames and data must have same length')

      ncols = nrow(self$colData$data)
      nrows = nrow(self$rowData$data)

      lapply(self$data, function(each){
        if (dim(each)[1] != nrows) stop(cat0('matrix nrow mismatch found ', as.character(dim(each)[1]) , ' expected ',  as.character(nrows)))
        if (dim(each)[2] != ncols) stop(cat0('matrix ncol mismatch found ', as.character(dim(each)[2]) , ' expected ',  as.character(ncols)))
      })
    },

    toJson = function(){
      return (
        list(kind=tson.character("Cube"),
             rowData=self$rowData$toJson(),
             colData=self$colData$toJson(),
             dataNames=self$dataNames,
             data=lapply(self$data, matrix.asTSON)
        )
      )
    },


    print = function(...) {
      cat("<Cube>  dataNames = ", paste(self$dataNames , collapse=', ') , "\n", sep = "")
      cat("        rowData = ", sep = "")
      self$rowData$print()
      cat("        colData = ", sep = "")
      self$colData$print()
      invisible(self)
    }
  )
)
