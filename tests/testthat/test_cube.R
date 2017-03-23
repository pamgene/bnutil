library(bnutil)
library(rtson)

context("cube test")

test_that("AnnotatedFrame", {

  colData = AnnotatedFrame$new(data=data.frame(sample=c('s1','s2')),
                               metadata=data.frame(labelDescription=c('mySamples')))

  rowData = AnnotatedFrame$new(data=data.frame(gene=c('gene1','gene2','gene3')),
                               metadata=data.frame(labelDescription=c('myGenes')))

  data = list(
    matrix(c(11.01,12.2,63.2,44.2,45.3,60.7), ncol=2),
    matrix(c(1.0,2.2,3.2,4.2,5.3,6.7), ncol=2)
  )

  dataNames = c('qt1','qt2')

  cube = Cube$new(colData=colData,rowData=rowData,dataNames=dataNames, data=data)

  bytes = rtson::toTSON(cube$toJson())
  cube = Cube$new(json=rtson::fromTSON(bytes))

  # print(cube)

  expect_that(cube$colData$metadata, equals(colData$metadata))
  expect_that(cube$colData$data, equals(colData$data))

  expect_that(cube$rowData$metadata, equals(rowData$metadata))
  expect_that(cube$rowData$data, equals(rowData$data))

  expect_that(cube$dataNames, equals(dataNames))
  expect_that(cube$data, equals(data))

})
