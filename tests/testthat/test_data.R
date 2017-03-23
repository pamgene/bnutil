library(bnutil)
library(rtson)

context("data test")

test_that("AnnotatedFrame", {

  data = data.frame(rowSeq=c(1,1,1,1),
                    colSeq=c(1,1,2,2),
                    myXAxis=c(1.0,2.0,1.0,2.0),
                    value=c(0.42,42.0,1.42,42.1),
                    IsOutlier=c(F,F,F,F))

  metadata = data.frame(labelDescription=c("rowSeq",
                                           "colSeq",
                                           "myXAxis",
                                           "value",
                                           "IsOutlier"))

  frame = AnnotatedFrame$new(data=data, metadata=metadata)
  bytes = rtson::toTSON(frame$toJson())
  frame = AnnotatedFrame$new(json=rtson::fromTSON(bytes))

  # print(frame)

  expect_that(frame$columnNames, equals(colnames(data)))
  expect_that(frame$labelDescriptions, equals(as.character(metadata$labelDescription)))

  metadata = data.frame(labelDescription=c("rowSeq",
                                           "colSeq",
                                           "myXAxis",
                                           "value",
                                           "IsOutlier"),
                        groupingType=c("rowSeq",
                                       "colSeq",
                                       "xAxis",
                                       "value",
                                       "IsOutlier"))

  annotatedData = AnnotatedData$new(data=data, metadata=metadata)
  bytes = rtson::toTSON(annotatedData$toJson())
  annotatedData = AnnotatedData$new(json=rtson::fromTSON(bytes))

  # print(annotatedData)

  expect_that(annotatedData$columnNames, equals(colnames(data)))
  expect_that(annotatedData$labelDescriptions, equals(as.character(metadata$labelDescription)))
  expect_that(annotatedData$groupingTypes, equals(as.character(metadata$groupingType)))
})

