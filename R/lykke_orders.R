# define market order -----------------------------------------------------
#' @import httr
#' @import jsonlite
#' @export
market_order <- function(AssetPairId, Asset, OrderAction, Volume, APIKey) {

  if(Volume <= 0) stop("Volume must be positive")

  url <- httr::modify_url("https://hft-service-dev.lykkex.net/",
                          path = "api/Orders/market")

  resp <- httr::POST(url,
                     body = list(AssetPairId = AssetPairId,
                                 Asset = Asset,
                                 OrderAction = OrderAction,
                                 Volume = Volume),
                     httr::add_headers(`api-key` = APIKey),
                     encode = "json")

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)

  structure(list(content = parsed,
                 response = resp),
            class = "lykke_order")
}

# define limit order ------------------------------------------------------
#' @import httr
#' @import jsonlite
#' @export
limit_order <- function(AssetPairId, OrderAction, Volume, Price, APIKey) {

  if(OrderAction != "Buy" & OrderAction != "Sell") {
    stop("OrderAction must be either 'Buy' or 'Sell'")
  }
  if(Price <= 0 | !is.numeric(Price)) {
    stop("Price must be a positive number")
  }
  if(Volume <= 0 | !is.numeric(Volume)) {
    stop("Volume must be a positive number")
  }

  url <- httr::modify_url("https://hft-service-dev.lykkex.net/",
                          path = "api/Orders/limit")

  resp <- httr::POST(url,
                     body = list(AssetPairId = AssetPairId,
                                 OrderAction = OrderAction,
                                 Volume = Volume,
                                 Price = Price),
                     httr::add_headers(`api-key` = APIKey),
                     encode = "json")

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)

  structure(list(content = parsed,
                 response = resp),
            class = "lykke_order")
}

# define order cancellation -----------------------------------------------
#' @import httr
#' @import jsonlite
#' @export
cancel_order <- function(Id, APIKey) {

  url <- httr::modify_url("https://hft-service-dev.lykkex.net/",
                          path = paste0("api/Orders/", Id, "/Cancel"))
  resp <- httr::POST(url,
                     httr::add_headers(`api-key` = APIKey),
                     encode = "json")

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)

  structure(list(content = parsed,
                 response = resp),
            class = "lykke_order")
}

# set print method for lykke order types ----------------------------------
#' @export
print.lykke_order <- function(x, ...) {
  cat("<Lykke Order>\n", sep = "")
  str(x$content)
  invisible(x)
}
