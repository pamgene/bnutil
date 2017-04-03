#' @export
cubeForTest = function(){
  colData = AnnotatedFrame$new(data=data.frame(sample=c('sample1','sample2')),
                               metadata=data.frame(labelDescription=c('Samples')))

  rowData = AnnotatedFrame$new(data=data.frame(gene=c('gene1','gene2','gene3')),
                               metadata=data.frame(labelDescription=c('Genes')))

  data = list(
    matrix(c(1.0,10.0,100.0,2.0,20.0,200.0), ncol=2),
    matrix(c(1.0,2.2,3.2,4.2,5.3,6.7), ncol=2)
  )

  dataNames = c('qt1','qt2')

  cube = Cube$new(colData=colData,rowData=rowData,dataNames=dataNames, data=data)
  return(cube)
}
