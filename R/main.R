#' @import dplyr
#' @export
getPropertyValue = function(properties=properties, name=name, prop.is.numeric = FALSE) {
  for ( i in 1:length(properties) ) {
    if (properties[[i]][1] == name){
      val = properties[[i]][2]
      if (prop.is.numeric){
        return(as.numeric(val))
      } else {
        return(val)
      }
    }
  }
  stop(paste("property not found: ", name))
  return (NULL)
}

#' @export
getXAxisLabel = function(annotatedDataframe){
  metaData = Biobase::varMetadata(annotatedDataframe)
  label = colnames(Biobase::pData(annotatedDataframe))[metaData$groupingType=='xAxis']
  if (length(label) == 0) stop('xAxis is not defined')
  return(label)
}

#' @export
hasXAxis = function(annotatedDataframe){
  metaData = Biobase::varMetadata(annotatedDataframe)
  label = colnames(Biobase::pData(annotatedDataframe))[metaData$groupingType=='xAxis']
  return(length(label) > 0)
}

#' @export
dplyrDoWithProgress = function(group_by_data, fun=NULL, progress=NULL){

  result = NULL

  ngroups = (nrow(group_by_data %>% do(data.frame(nrow=0)))) / 100
  env = new.env()
  env$i = 0
  env$part = 0
  env$progress = progress

  funWithProgress = function(aFrame){
    env$i  = env$i  + 1

    if (env$i >= ngroups){
      env$i  = 0
      env$part = env$part + 1
      progress$inc(0.01, detail = paste( env$part, '%' ))
    }

    return(fun(aFrame))
  }

  result = group_by_data %>% do(funWithProgress(.))

  return(result)
}
