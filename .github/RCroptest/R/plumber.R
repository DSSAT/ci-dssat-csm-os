#' Start the RCroptest API
#'
#' @return Starts listening on the defined port and HTTP endpoints
#' @export
#'
start <- function() {
  routes <- plumber::plumber$new()

  routes$handle(method = "POST", path = "/api/summary/model",
                handler = apiSummaryModel,
                serializer = plumber::serializer_json())
  routes$handle(method = "POST", path = "/api/summary/statistics",
                handler = apiSummaryStatistics,
                serializer = plumber::serializer_json())
  routes$handle(method = "POST", path = "/api/summary/differences",
                handler = apiSummaryDifferences,
                serializer = plumber::serializer_json())
  routes$handle(method = "POST", path = "/api/evaluate/model",
                handler = apiEvaluateModel,
                serializer = plumber::serializer_json())
  routes$handle(method = "POST", path = "/api/evaluate/statistics",
                handler = apiEvaluateStatistics,
                serializer = plumber::serializer_json())

  host <- getOption('RCroptest.api.host', default = '127.0.0.1')
  port <- getOption('RCroptest.api.port', default = 9917)
  routes$run(host = host, port = port, swagger = FALSE)
}

