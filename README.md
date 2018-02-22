
<!-- README.md is generated from README.Rmd. Please edit that file -->
cryptor
=======

[![Travis-CI Build Status](https://travis-ci.org/blairj09/cryptor.svg?branch=master)](https://travis-ci.org/blairj09/cryptor) [![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github//blairj09/cryptor/?branch=master&svg=true)](https://ci.appveyor.com/project/blairj09/cryptor) [![Coverage status](https://codecov.io/gh/blairj09/cryptor/branch/master/graph/badge.svg)](https://codecov.io/github/blairj09/cryptor?branch=master)

`cryptor` provides a basic wrapper around the public API provided by [CryptoCompare](https://www.cryptocompare.com). CryptoCompare has provided a very comprehensive [public cryptocurrency API](https://min-api.cryptocompare.com). This package strives to create a clean and intuitive R interface for that API.

Installation
------------

You can install `cryptor` from github with:

``` r
# install.packages("devtools")
devtools::install_github("blairj09/cryptor")
```

Usage
-----

This package comes with several functions that map directly to CryptoCompare [API endpoints](https://min-api.cryptocompare.com). These functions center on price and other coin details for various cryptorcurrencies. CryptoCompare integrates with several exchanges and the exchange to be queried can often be requested. However, the default exchange of CCCAGG often works the best. The following outlines common use cases for various functions.

``` r
library(cryptor)
```

### Available Exchanges

Possible exchanges to query can be requested using the `get_exchanges()` function. This returns a comprehensive tibble of available exchanges along with each market available on that exchange.

``` r
exchanges <- get_exchanges()
head(exchanges)
#> # A tibble: 6 x 3
#>   exchange coin  market
#>   <chr>    <chr> <chr> 
#> 1 Cryptsy  42    BTC   
#> 2 Cryptsy  42    XRP   
#> 3 Cryptsy  RDD   LTC   
#> 4 Cryptsy  RDD   USD   
#> 5 Cryptsy  RDD   XRP   
#> 6 Cryptsy  RDD   BTC
```

In this case, the `exchange` column contains the name of the exchange while the `coin` column contains the coin and the `market` column contains the market supported for `coin` on a given `exchange`. All possible exchanges can be identified by running `unique(get_exchanges()$exchage)`.

### Available Coins

All coins supported by the API and correspanding details can be obtained using the `get_coins()` function.

``` r
coins <- get_coins()
head(coins)
#> # A tibble: 6 x 16
#>       id url      image_url    name  symbol coin_name full_name  algorithm
#>    <int> <chr>    <chr>        <chr> <chr>  <chr>     <chr>      <chr>    
#> 1   4321 /coins/… /media/1231… 42    42     42 Coin   42 Coin (… Scrypt   
#> 2 749869 /coins/… /media/2701… 300   300    300 token 300 token… <NA>     
#> 3  33639 /coins/… /media/3520… 365   365    365Coin   365Coin (… X11      
#> 4  21227 /coins/… /media/3510… 404   404    404Coin   404Coin (… Scrypt   
#> 5  20909 /coins/… /media/3509… 611   611    SixEleven SixEleven… SHA256   
#> 6  28223 /coins/… /media/3515… 808   808    808       808 (808)  SHA256   
#> # ... with 8 more variables: proof_type <chr>, fully_premined <int>,
#> #   total_coin_supply <chr>, pre_mined_value <chr>,
#> #   total_coins_free_float <chr>, sort_order <int>, sponsored <lgl>,
#> #   is_trading <lgl>
```

### Current Price Data

There are two main functions for retreiving current price data. `get_price()` simply returns the price of the requested coins. `get_price_details()` returns current price data along with open, close, and other 24 hour price statistics. Price functions take two main parameters: `fsym(s)` and `tsym(s)`. These parameters specify the requested coin(s) (`fsym(s)`) and the currency the price is reported in (`tsym(s)`). The following examples illustrate getting price data for Bitcoin (BTC) and Etherium (ETH) in terms of US Dollars (USD).

``` r
get_price(c("BTC", "ETH"), "USD")
#> # A tibble: 2 x 3
#>   fromsymbol tosymbol price
#>   <chr>      <chr>    <dbl>
#> 1 BTC        USD      9925.
#> 2 ETH        USD       805.
```

``` r
get_price_details(c("BTC", "ETH"), "USD")
#> # A tibble: 2 x 29
#>    type market fromsymbol tosymbol flags price lastupdate         
#>   <int> <chr>  <chr>      <chr>    <int> <dbl> <dttm>             
#> 1     5 CCCAGG BTC        USD          2 9925. 2018-02-22 15:16:55
#> 2     5 CCCAGG ETH        USD          1  805. 2018-02-22 15:16:53
#> # ... with 22 more variables: lastvolume <dbl>, lastvolumeto <dbl>,
#> #   lasttradeid <int>, volumeday <dbl>, volumedayto <dbl>,
#> #   volume24hour <dbl>, volume24hourto <dbl>, openday <dbl>,
#> #   highday <dbl>, lowday <dbl>, open24hour <dbl>, high24hour <dbl>,
#> #   low24hour <dbl>, lastmarket <chr>, change24hour <dbl>,
#> #   changepct24hour <dbl>, changeday <dbl>, changepctday <dbl>,
#> #   supply <dbl>, mktcap <dbl>, totalvolume24h <dbl>,
#> #   totalvolume24hto <dbl>
```

### Historical Price Data

Historical price data can be retreived using `get_historical_price()`. Price can be obtained at the daily, hourly, or minute level. Minute level data is only available for the past 7 days. Anything beyond 7 days must be either daily or hourly data. The following example demonstrates how to get NEO to USD data for the past 7 days.

*As a note, it seems that the API sometimes returns all historical entries, regardless of the limit provided.*

``` r
btc_price_history <- get_historical_price("NEO", "USD")
head(btc_price_history)
#> # A tibble: 6 x 7
#>   time                close  high   low  open volumefrom  volumeto
#>   <dttm>              <dbl> <dbl> <dbl> <dbl>      <dbl>     <dbl>
#> 1 2018-01-23 00:00:00  122.  130.  109.  123.    231021. 27649351.
#> 2 2018-01-24 00:00:00  138.  146.  117.  122.    322866. 43525209.
#> 3 2018-01-25 00:00:00  137.  146.  134.  138.    226992. 31562447.
#> 4 2018-01-26 00:00:00  137.  141.  124.  137.    217251. 28874901.
#> 5 2018-01-27 00:00:00  141.  143.  134.  137.    118856. 16445792.
#> 6 2018-01-28 00:00:00  153.  154.  140.  141.    127609. 18775018.
```

### Snapshot Data

CrytpoCompare provides endpoints for "snapshot data". These endpoints provide summarized data about individual coins and coin pairs.

``` r
btc_snapshot <- get_coin_snapshot(1182)
names(btc_snapshot)
#>  [1] "page_title"                    "page_description"             
#>  [3] "base_url"                      "base_image_url"               
#>  [5] "og_image_url"                  "og_image_width"               
#>  [7] "og_image_height"               "id"                           
#>  [9] "document_type"                 "h1text"                       
#> [11] "danger_top"                    "warning_top"                  
#> [13] "info_top"                      "symbol"                       
#> [15] "url"                           "base_angular_url"             
#> [17] "name"                          "image_url"                    
#> [19] "description"                   "features"                     
#> [21] "technology"                    "total_coin_supply"            
#> [23] "difficulty_adjustment"         "block_reward_reduction"       
#> [25] "algorithm"                     "proof_type"                   
#> [27] "start_date"                    "twitter"                      
#> [29] "affiliate_url"                 "website"                      
#> [31] "last_block_explorer_update_ts" "block_number"                 
#> [33] "block_time"                    "net_hashes_per_second"        
#> [35] "total_coins_mined"             "previous_total_coins_mined"   
#> [37] "block_reward"                  "status"                       
#> [39] "white_paper"
```

``` r
get_pair_snapshot("BTC", "ETH")
#> $coin_data
#> # A tibble: 1 x 6
#>   algorithm proof_type block_number net_hashes_per_second total_coins_min…
#>   <chr>     <chr>             <int>                 <dbl>            <int>
#> 1 SHA256    PoW              510421          21528020746.         16880175
#> # ... with 1 more variable: block_reward <dbl>
#> 
#> $aggregated_data
#> # A tibble: 1 x 21
#>    type market fromsymbol tosymbol flags price lastupdate         
#>   <int> <chr>  <chr>      <chr>    <int> <dbl> <dttm>             
#> 1     5 CCCAGG BTC        ETH          2  12.0 2018-02-22 15:10:29
#> # ... with 14 more variables: lastvolume <dbl>, lastvolumeto <dbl>,
#> #   lasttradeid <dbl>, volumeday <dbl>, volumedayto <dbl>,
#> #   volume24hour <dbl>, volume24hourto <dbl>, openday <dbl>,
#> #   highday <dbl>, lowday <dbl>, open24hour <dbl>, high24hour <dbl>,
#> #   low24hour <dbl>, lastmarket <chr>
#> 
#> $exchange_data
#> # A tibble: 2 x 15
#>    type market        fromsymbol tosymbol flags price lastupdate         
#>   <int> <chr>         <chr>      <chr>    <int> <dbl> <dttm>             
#> 1     2 Bleutrade     BTC        ETH          2  11.9 2018-02-22 15:10:29
#> 2     2 LocalBitcoins BTC        ETH          2  12.6 2018-02-22 13:01:42
#> # ... with 8 more variables: lastvolume <dbl>, lastvolumeto <dbl>,
#> #   lasttradeid <dbl>, volume24hour <dbl>, volume24hourto <dbl>,
#> #   open24hour <dbl>, high24hour <dbl>, low24hour <dbl>
```

### Social Data

One of the most impressive features of the CryptoCompare API is the ability to query some basic social stats for a given coin. The data provided contains details about the coin's github repositories, page view data from CryptoCompare's main website, and a listing of similar coins. This data can be accessed as follows:

``` r
# Social stats for Bitcoin
eth_social_stats <- get_social(1182)
str(eth_social_stats)
#> List of 6
#>  $ similar_items         :Classes 'tbl_df', 'tbl' and 'data.frame':  10 obs. of  6 variables:
#>   ..$ id            : int [1:10] 7605 5031 3808 127356 4614 202330 5038 27368 4433 24854
#>   ..$ name          : chr [1:10] "Ethereum" "Ripple" "Litecoin" "IOTA" ...
#>   ..$ full_name     : chr [1:10] "Ethereum (ETH)" "Ripple (XRP)" "Litecoin (LTC)" "IOTA (IOT)" ...
#>   ..$ image_url     : chr [1:10] "/media/20646/eth_logo.png" "/media/19972/ripple.png" "/media/19782/litecoin-logo.png" "/media/1383540/iota_logo.png" ...
#>   ..$ url           : chr [1:10] "/coins/eth/" "/coins/xrp/" "/coins/ltc/" "/coins/iot/" ...
#>   ..$ following_type: int [1:10] 1 1 1 1 1 1 1 1 1 1
#>  $ cryptopian_followers  :Classes 'tbl_df', 'tbl' and 'data.frame':  11 obs. of  5 variables:
#>   ..$ id       : int [1:11] 693768 248526 387130 212040 749031 739209 729752 702016 771804 762923 ...
#>   ..$ name     : chr [1:11] "nanpunpa940" "Mark_____" "bjamman1" "dedgevergeer" ...
#>   ..$ image_url: chr [1:11] "https://images.cryptocompare.com/693768/3813ed09-0d58-497b-bf61-8b51c5a003e2.jpg" "https://images.cryptocompare.com/248526/59a1bde1-716e-4bb4-a331-c0d408c24bb4.jpg" "https://images.cryptocompare.com/387130/ce167ec7-a8cd-4bf1-82c6-1cfe3b6a356a.jpg" "https://images.cryptocompare.com/212040/fe868cb4-4f20-4dea-81b1-88aabf629e5d.jpg" ...
#>   ..$ url      : chr [1:11] "/profile/nanpunpa940/" "/profile/Mark_____/" "/profile/bjamman1/" "/profile/dedgevergeer/" ...
#>   ..$ type     : chr [1:11] "Cryptopian" "Cryptopian" "Cryptopian" "Cryptopian" ...
#>  $ page_views            :Classes 'tbl_df', 'tbl' and 'data.frame':  8 obs. of  2 variables:
#>   ..$ page : chr [1:8] "overview" "markets" "analysis" "charts" ...
#>   ..$ views: int [1:8] 14179038 1016460 668495 4602808 429823 50035 2967169 41619
#>  $ crypto_compare_summary:Classes 'tbl_df', 'tbl' and 'data.frame':  1 obs. of  5 variables:
#>   ..$ points    : int 3411445
#>   ..$ followers : int 52117
#>   ..$ posts     : int 46222
#>   ..$ comments  : int 95297
#>   ..$ page_views: int 23955447
#>  $ social_media          :List of 3
#>   ..$ Twitter :Classes 'tbl_df', 'tbl' and 'data.frame': 1 obs. of  9 variables:
#>   .. ..$ following       : int 127
#>   .. ..$ account_creation: int 1313643968
#>   .. ..$ name            : chr "Bitcoin"
#>   .. ..$ lists           : int 6329
#>   .. ..$ statuses        : int 19523
#>   .. ..$ favourites      : int 232
#>   .. ..$ followers       : int 783544
#>   .. ..$ link            : chr "https://twitter.com/bitcoin"
#>   .. ..$ points          : int 815208
#>   ..$ Reddit  :Classes 'tbl_df', 'tbl' and 'data.frame': 1 obs. of  10 variables:
#>   .. ..$ posts_per_hour    : num 14.9
#>   .. ..$ comments_per_hour : num 312
#>   .. ..$ posts_per_day     : num 358
#>   .. ..$ comments_per_day  : num 7481
#>   .. ..$ name              : chr "Bitcoin"
#>   .. ..$ link              : chr "https://www.reddit.com/r/bitcoin/"
#>   .. ..$ active_users      : int 9884
#>   .. ..$ community_creation: int 1284042626
#>   .. ..$ subscribers       : int 748475
#>   .. ..$ points            : int 793088
#>   ..$ Facebook:Classes 'tbl_df', 'tbl' and 'data.frame': 1 obs. of  6 variables:
#>   .. ..$ likes        : int 37226
#>   .. ..$ link         : chr "https://www.facebook.com/bitcoins/"
#>   .. ..$ is_closed    : chr "false"
#>   .. ..$ talking_about: int 174
#>   .. ..$ name         : chr "Bitcoin P2P Cryptocurrency"
#>   .. ..$ points       : int 37226
#>  $ repo_summary          :Classes 'tbl_df', 'tbl' and 'data.frame':  4 obs. of  18 variables:
#>   ..$ created_at         : int [1:4] 1304525025 1363239994 1384835603 1292771803
#>   ..$ open_total_issues  : int [1:4] 25 28 228 882
#>   ..$ parent             : logi [1:4] NA NA NA NA
#>   ..$ size               : int [1:4] 3003 1174 16944 83032
#>   ..$ closed_total_issues: int [1:4] 139 1287 982 11489
#>   ..$ stars              : int [1:4] 2395 753 2047 27973
#>   ..$ last_update        : POSIXct[1:4], format: "2018-02-22 14:23:12" ...
#>   ..$ forks              : int [1:4] 781 265 1351 16589
#>   ..$ url                : chr [1:4] "https://github.com/bitcoinjs/bitcoinjs-lib" "https://github.com/petertodd/python-bitcoinlib" "https://github.com/bitcoinj/bitcoinj" "https://github.com/bitcoin/bitcoin"
#>   ..$ closed_issues      : int [1:4] 46 621 399 3139
#>   ..$ closed_pull_issues : int [1:4] 93 666 583 8350
#>   ..$ fork               : chr [1:4] "false" "false" "false" "false"
#>   ..$ last_push          : int [1:4] 1519115258 1519197806 1519310206 1519310896
#>   ..$ source             : logi [1:4] NA NA NA NA
#>   ..$ open_pull_issues   : int [1:4] 8 28 13 283
#>   ..$ language           : chr [1:4] "JavaScript" "Python" "Java" "C++"
#>   ..$ subscribers        : int [1:4] 159 95 274 2999
#>   ..$ open_issues        : int [1:4] 15 200 17 599
```

### Pairs Data

### API Limits

The API has hour, minute, and second call rate limits. These limits, along with calls made against the limit, can be retrieved using `get_api_limit()` as follows:

``` r
# Second limit
get_api_limit("second")
#> # A tibble: 2 x 5
#>   call_metric histo price  news strict
#>   <chr>       <int> <int> <int>  <int>
#> 1 calls_made      0     0     0      0
#> 2 calls_left     15    50     5      1

# Minute limit
get_api_limit("minute")
#> # A tibble: 2 x 5
#>   call_metric histo price  news strict
#>   <chr>       <int> <int> <int>  <int>
#> 1 calls_made      1     2     0      0
#> 2 calls_left    299   998   100     20

# Hour limit
get_api_limit("hour")
#> # A tibble: 2 x 5
#>   call_metric histo  price  news strict
#>   <chr>       <int>  <int> <int>  <int>
#> 1 calls_made      2      8     0      0
#> 2 calls_left   7998 149992  3000    500
```
