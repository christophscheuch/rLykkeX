# define function to get information from lykke api -----------------------
#' @import httr
#' @import jsonlite
#' @importFrom rlang is_empty
#' @export
lykke_api <- function(path, api_key = NULL) {
  url <- httr::modify_url("https://hft-service-dev.lykkex.net/",
                    path = paste0("api/", path))

  if ((path == "Wallets" | grepl("Orders", path)) & rlang::is_empty(path)) {
    stop("API key need to access Wallets / Orders")
  }

  if (path == "Wallets" | grepl("Orders", path)) {
    resp <- httr::GET(url, httr::add_headers(`api-key` = api_key))
  } else {
    resp <- httr::GET(url)
  }

  if (httr::http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }

  parsed <- jsonlite::fromJSON(httr::content(resp, "text"),
                               simplifyVector = FALSE)

  if (httr::http_error(resp)) {
    stop(sprintf("Lykke API request failed [%s]\n%s\n<%s>",
                 status_code(resp),
                 parsed$message,
                 parsed$documentation_url),
         call. = FALSE)
  }

  structure(list(content = parsed,
                 path = path,
                 response = resp),
            class = "lykke_api")
}

# set print method for lists from lykke api  ------------------------------
#' @export
print.lykke_api <- function(x, ...) {
  cat("<Lykke ", x$path, ">\n", sep = "")
  str(x$content)
  invisible(x)
}

# define helper function to get order book information --------------------
#' @import httr
#' @import dplyr
#' @import tibble
#' @importFrom rlang is_empty
#' @export
get_order_book <- function(AssetPair) {

  ob <- lykke_api(paste0("OrderBooks/", AssetPair))

  # some initial checks
  if (rlang::is_empty(ob$content[[1]]$Prices) &
      rlang::is_empty(ob$content[[2]]$Prices)) {
    stop("Bid and ask sides are empty")
  }
  if (rlang::is_empty(ob$content[[1]]$Prices)) stop("Bid side is empty")
  if (rlang::is_empty(ob$content[[2]]$Prices)) stop("Ask side is empty")

  ask <- tibble(Price = unlist(sapply(ob$content[[1]]$Prices,
                                      "[", "Price")),
                Volume = unlist(sapply(ob$content[[1]]$Prices,
                                       "[", "Volume"))) %>%
    mutate(OrderAction = "Sell",
           Volume = abs(Volume),
           Timestamp = ob$content[[1]]$Timestamp) %>%
    arrange(Price) %>%
    mutate(Depth = cumsum(Volume))

  bid <- tibble(Price = unlist(sapply(ob$content[[2]]$Prices,
                                      "[", "Price")),
                Volume = unlist(sapply(ob$content[[2]]$Prices,
                                       "[", "Volume"))) %>%
    mutate(OrderAction = "Buy",
           Volume = abs(Volume),
           Timestamp = ob$content[[1]]$Timestamp) %>%
    arrange(-Price) %>%
    mutate(Depth = cumsum(Volume))

  # collect useful information in a list again
  structure(list(AssetPair = AssetPair,
                 Timestamp = ob$content[[1]]$Timestamp,
                 Orderbook = bind_rows(ask, bid) %>% arrange(Price),
                 BestBid = max(bid$Price),
                 BestAsk = min(ask$Price)),
            class = "orderbook")
}

# define method for plotting orderbook depths -----------------------------
#' @import ggplot2
#' @import dplyr
#' @importFrom scales comma
#' @export
plot.orderbook <- function(x, window = 0.3) {
  # plot orderbook around midquote +- window factor
  ggplot() +
    geom_line(data = x$Orderbook %>%
                filter(OrderAction == "Sell" &
                         Price <= (1 + window) * (x$BestBid + x$BestAsk) / 2),
              aes(x = Price, y = Depth), size = 1, color = "red") +
    geom_line(data = x$Orderbook %>%
                filter(OrderAction == "Buy"  &
                         Price >= (1 - window) * (x$BestBid + x$BestAsk) / 2),
              aes(x = Price, y = Depth), size = 1, color = "blue") +
    labs(title = paste0(x$AssetPair, " (", x$Timestamp, ")")) +
    scale_x_continuous(expand = c(0, 0), labels = scales::comma) +
    scale_y_continuous(expand = c(0, 0),  labels = scales::comma) +
    theme_bw() + theme(plot.title = element_text(hjust = 0.5))
}

# helper function to get status of lykke api ------------------------------
#' @export
lykke_api_check <- function() {!lykke_api("IsAlive")$content$IsDebug}

# helper function to get all asset pairs from lykke -----------------------
#' @import tibble
#' @export
get_asset_pairs <- function() {
  tibble(Id = unlist(sapply(lykke_api("AssetPairs")$content, "[", "Id")))
}

# helper function to get wallet information -------------------------------
#' @import dplyr
#' @import tibble
#' @export
get_wallet_info <- function(api_key) {
  dplyr::bind_rows(lykke_api("Wallets", api_key)$content) %>%
    tibble::as_tibble()
}
