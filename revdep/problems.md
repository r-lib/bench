# httr2

<details>

* Version: 0.2.2
* GitHub: https://github.com/r-lib/httr2
* Source code: https://github.com/cran/httr2
* Date/Publication: 2022-09-25 17:50:03 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "httr2")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘httr2-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: resp_raw
    > ### Title: Show the raw response
    > ### Aliases: resp_raw
    > 
    > ### ** Examples
    > 
    > resp <- request("https://httpbin.org/json") %>% req_perform()
    Error in `resp_abort()`:
    ! HTTP 504 Gateway Timeout.
    Backtrace:
        ▆
     1. ├─request("https://httpbin.org/json") %>% req_perform()
     2. └─httr2::req_perform(.)
     3.   └─httr2:::resp_abort(resp, error_body(req, resp))
     4.     └─rlang::abort(...)
    Execution halted
    ```

## Newly fixed

*   checking re-building of vignette outputs ... ERROR
    ```
    Error(s) in re-building vignettes:
      ...
    --- re-building ‘httr2.Rmd’ using rmarkdown
    Quitting from lines 117-120 (httr2.Rmd) 
    Error: processing vignette 'httr2.Rmd' failed with diagnostics:
    HTTP 504 Gateway Timeout.
    --- failed re-building ‘httr2.Rmd’
    
    SUMMARY: processing the following file failed:
      ‘httr2.Rmd’
    
    Error: Vignette re-building failed.
    Execution halted
    ```

