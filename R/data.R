library(R6)

#' @import R6
#' @export
AnnotatedData = R6Class(
  "AnnotatedData",
  public = list(
    XAXIS = 'xAxis',
    COLOR = 'color',
    metadata = NULL,
    data = NULL,

    initialize = function(metadata=NULL,data=NULL,json=NULL){

      if (is.null(json)){
        self$metadata = metadata
        self$data = data
      } else {
        self$metadata=data.frame.fromTSON(json$varMetadata)
        self$data = data.frame.fromTSON(json$data)
      }

      if (!inherits(self$metadata,'data.frame' )) stop('metadata must be a data.frame')
      if (!inherits(self$data,'data.frame' )) stop('data must be a data.frame')
      if (length(colnames(self$data)) != nrow(self$metadata)){

        stop('wrong dim : length(colnames(data)) != nrow(metadata)')
      }

      if (is.null(self$metadata$groupingType)) {
        stop('wrong metadata : column groupingType is required')
      }

    },

    getcolumnNames = function(groupingType){
      return(colnames(self$data)[self$metadata$groupingType==groupingType])
    },

    getLabels = function(groupingType){
      if (is.null(self$metadata$labelDescription)) return (self$getcolumnNames(groupingType))
      return(self$metadata$labelDescription[self$metadata$groupingType==groupingType])
    },

    toJson = function(){
      return (list(kind=tson.character("AnnotatedData"),
                   varMetadata=data.frame.asTSON(self$metadata),
                   data=data.frame.asTSON(self$data)))
    },
    print = function(...) {
      cat("<AnnotatedData>  nrows ", nrow(self$data), " ncols = ", ncol(self$data), "\n", sep = "")
      cat("                 columnNames = ", paste(self$columnNames , collapse=', ') ,"\n", sep = "")
      if (!is.null(self$metadata$labelDescription)){
        cat("                 labelDescriptions = ", paste(self$labelDescriptions, collapse=', ') ,"\n", sep = "")
      }
      cat("                 groupingTypes = ", paste(self$groupingTypes, collapse=', ') ,"\n", sep = "")
      invisible(self)
    }
  ),

  active = list(
    columnNames = function() colnames(self$data),
    groupingTypes = function() as.character(self$metadata$groupingType),
    labelDescriptions = function(){
      if (!is.null(self$metadata$labelDescription)){
        return(as.character(self$metadata$labelDescription))
      } else {
        return(NULL)
      }
    },
    hasLabelDescriptions = function() !is.null(self$metadata$labelDescription),
    hasXAxis = function(value){
      if (missing(value)){
        label = self$getLabels(self$XAXIS)
        return(length(label) > 0)
      }
      else stop('AnnotatedData : hasXAxis is read only')
    },
    xAxisLabel = function(value){
      if (missing(value)){
        label = self$getLabels(self$XAXIS)
        if (length(label) == 0) stop('xAxis is not defined')
        return(label)
      }
      else stop('AnnotatedData : xAxisLabel is read only')
    },
    xAxisColumnName = function(value){
      if (missing(value)){
        label = self$getcolumnNames(self$XAXIS)
        if (length(label) == 0) stop('xAxis is not defined')
        return(label)
      }
      else stop('AnnotatedData : xAxisLabel is read only')
    },
    hasColors = function(value){
      if (missing(value)){
        label = self$getLabels(self$COLOR)
        return(length(label) > 0)
      }
      else stop('AnnotatedData : hasXAxis is read only')
    },
    colorLabels = function(value){
      if (missing(value)) return(self$getLabels(self$COLOR))
      else stop('AnnotatedData : xAxisLabel is read only')
    },
    colorColumnNames = function(value){
      if (missing(value)) return(self$getcolumnNames(self$COLOR))
      else stop('AnnotatedData : xAxisLabel is read only')
    }
  )
)
