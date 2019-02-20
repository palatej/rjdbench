#' @export
jd3_denton<-function(s, t, d=1, mul=TRUE, modified=TRUE, conversion="Sum"){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "denton"
                  ,jd_s, jd_t, as.integer(d), mul, modified, conversion)
  ts_jd2r(jd_rslt)
}

#' @export
jd3_cholette<-function(s, t, rho=1, lambda=1, bias="None", conversion="Sum"){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "cholette"
                  ,jd_s, jd_t, rho, lambda, bias, conversion)
  ts_jd2r(jd_rslt)
}
