#nocov start
.onLoad <- function(...) {
  s3_register("tidyr::unnest", "bench_mark")
  s3_register("dplyr::filter", "bench_mark")
  s3_register("dplyr::group_by", "bench_mark")
  s3_register("ggplot2::autoplot", "bench_mark")

  s3_register("ggplot2::scale_type", "bench_expr")
  s3_register("ggplot2::scale_type", "bench_time")
  s3_register("ggplot2::scale_type", "bench_bytes")

  s3_register("knitr::knit_print", "bench_mark")

  s3_register("vctrs::vec_proxy", "bench_expr")
  s3_register("vctrs::vec_restore", "bench_expr")
}
#nocov end
