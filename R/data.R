#' @import R6
#' @import rtson
#' @export
AnnotatedFrame = R6Class(
  "AnnotatedFrame",
  public = list(
    metadata = NULL,
    data = NULL,

    initialize = function(metadata=NULL,data=NULL,json=NULL){
      self$init(metadata=metadata,data=data,json=json)
    },

    init = function(metadata=NULL,data=NULL,json=NULL){
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
    },

    toJson = function(){
      return (list(kind=tson.character("AnnotatedFrame"),
                   varMetadata=data.frame.asTSON(self$metadata),
                   data=data.frame.asTSON(self$data)))
    },

    print = function(...) {
      cat("<AnnotatedFrame>  nrows ", nrow(self$data), " ncols = ", ncol(self$data), "\n", sep = "")
      cat("                 columnNames = ", paste(self$columnNames , collapse=', ') ,"\n", sep = "")
      if (!is.null(self$metadata$labelDescription)){
        cat("                 labelDescriptions = ", paste(self$labelDescriptions, collapse=', ') ,"\n", sep = "")
      }
      invisible(self)
    }
  ),

  active = list(
    columnNames = function() colnames(self$data),
    labelDescriptions = function(){
      if (self$hasLabelDescriptions){
        return(as.character(self$metadata$labelDescription))
      } else {
        return(NULL)
      }
    },
    hasLabelDescriptions = function() !is.null(self$metadata$labelDescription)
  )
)

#' @export
AnnotatedData = R6Class(
  "AnnotatedData",
  inherit = AnnotatedFrame,
  public = list(
    XAXIS = 'xAxis',
    COLOR = 'color',
    QT = 'QuantitationType',
    ARRAY = 'Array',
    SPOT = 'Spot',

    initialize = function(metadata=NULL,data=NULL,json=NULL){
      self$init(metadata=metadata,data=data,json=json)
    },

    init = function(metadata=NULL,data=NULL,json=NULL){
      super$init(metadata=metadata,data=data,json=json)
      if (is.null(self$metadata$groupingType)) stop('wrong metadata : column groupingType is required')
    },

    getData = function(outlier=FALSE){
      if (!outlier){
        return(subset(self$data, !IsOutlier))
      }
      return(self$data)
    },

    getColors = function(){
      if (self$hasColors){
        return (self$data[self$colorColumnNames])
      } else {
        stop('getColors failed : no color')
      }
    },

    getcolumnNames = function(groupingType){
      return(colnames(self$data)[self$metadata$groupingType==groupingType])
    },

    getLabels = function(groupingType){
      if (is.null(self$metadata$labelDescription)) return (self$getcolumnNames(groupingType))
      labels = self$metadata$labelDescription[self$metadata$groupingType==groupingType]
      return(as.character(labels))
    },

    toJson = function(){
      list=super$toJson()
      list$kind=tson.character("AnnotatedData")
      return (list)
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
    groupingTypes = function() as.character(self$metadata$groupingType),
    hasXAxis = function(){
      label = self$getLabels(self$XAXIS)
      return(length(label) > 0)
    },
    xAxisLabel = function(){
      label = self$getLabels(self$XAXIS)
      if (length(label) == 0) stop('xAxis is not defined')
      return(label)
    },
    xAxisColumnName = function(){
      label = self$getcolumnNames(self$XAXIS)
      if (length(label) == 0) stop('xAxis is not defined')
      return(label)
    },
    hasColors = function(){
      label = self$getLabels(self$COLOR)
      return(length(label) > 0)
    },
    colorLabels = function() self$getLabels(self$COLOR),
    colorColumnNames = function() self$getcolumnNames(self$COLOR),
    qtColumnNames = function() self$getcolumnNames(self$QT),

    hasArrays = function(){
      label = self$getLabels(self$ARRAY)
      return(length(label)>0)
    },
    arrayLabels = function() self$getLabels(self$ARRAY),
    arrayColumnNames = function() self$getcolumnNames(self$ARRAY),

    hasSpots = function(){
      label = self$getLabels(self$SPOT)
      return(length(label)>0)
    },
    spotLabels = function() self$getLabels(self$SPOT),
    spotColumnNames = function() self$getcolumnNames(self$SPOT)
  )
)

