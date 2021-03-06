#' @include rslts.R
NULL

#' Benchmarking by means of the Denton method.
#'
#' @param s Disaggregated series. Mandatory
#' @param t Aggregation constraint. Mandatory
#' @param d Differencing order. 1 by default
#' @param mul Multiplicative or additive benchmarking. Multiplicative by default
#' @param modified Modified (TRUE) or unmodified (FALSE) Denton. Modified by default
#' @param conversion Conversion rule. Should be "Sum" or "Average". Sum by default.
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#' @return The benchmarked series is returned
#'
#' @export
jd3_denton<-function(s, t, d=1, mul=TRUE, modified=TRUE, conversion="Sum", obsposition=1){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "denton"
                  ,jd_s, jd_t, as.integer(d), mul, modified, conversion, as.integer(obsposition))
  ts_jd2r(jd_rslt)
}

#' Benchmarking by means of the Denton method without indicator
#'
#'
#' @param nfreq
#' @param t Aggregation constraint. Mandatory
#' @param d Differencing order. 1 by default
#' @param mul Multiplicative or additive benchmarking. Multiplicative by default
#' @param modified Modified (TRUE) or unmodified (FALSE) Denton. Modified by default
#' @param conversion Conversion rule. Should be "Sum" or "Average". Sum by default.
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#'
#' @return The benchmarked series is returned
#' @export
#'
#' @examples
jd3_denton2<-function(nfreq, t, d=1, mul=TRUE, modified=TRUE, conversion="Sum", obsposition=1){
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "denton"
                  ,as.integer(nfreq), jd_t, as.integer(d), mul, modified, conversion, as.integer(obsposition))
  ts_jd2r(jd_rslt)
}

#' Benchmarking following the growth rate preservation principle.
#' This method corresponds to the method of Cauley and Trager, using the solution
#' proposed by Di Fonzo and Marini.
#'
#' @param s
#' @param t
#' @param conversion
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#'
#' @return
#' @export
#'
#' @examples
jd3_grp<-function(s, t, conversion="Sum", obsposition=1, eps=1e-12, iter=500, denton=T){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "grp"
                  ,jd_s, jd_t, conversion, as.integer(obsposition), eps, as.integer(iter), as.logical(denton))
  ts_jd2r(jd_rslt)
}

#' Benchmarking by means of cubic splines
#'
#' @param s
#' @param t
#' @param conversion
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#'
#' @return
#' @export
#'
#' @examples
jd3_cubicspline<-function(s, t, conversion="Sum", obsposition=1){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "cubicSpline"
                  ,jd_s, jd_t, conversion, as.integer(obsposition))
  ts_jd2r(jd_rslt)
}

#' Benchmarking (without indicator) by means of cubic splines
#'
#' @param nfreq
#' @param t
#' @param conversion
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#'
#' @return
#' @export
#'
#' @examples
jd3_cubicspline2<-function(nfreq, t, conversion="Sum", obsposition=1){
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "cubicSpline"
                  ,as.integer(nfreq), jd_t, conversion, as.integer(obsposition))
  ts_jd2r(jd_rslt)
}


#' @title Cholette method
#'
#' @description Benchmarking by means of the Cholette method.
#'
#' @param s Disaggregated series. Mandatory
#' @param t Aggregation constraint. Mandatory
#' @param rho
#' @param lambda
#' @param bias
#' @param conversion
#' @param obsposition Postion of the observation in the aggregated period (only used with "UserDefined" conversion)
#'
#' @details
#' \deqn{\sum_{i,t}\left(\left(\frac{{x_{i,t}-z}_{i,t}}{\left|z_{i,t}\right|^\lambda}\right)-\rho\left(\frac{{x_{i,t-1}-z}_{i,t-1}}{\left|z_{i,t-1}\right|^\lambda}\right)\right)^2}
#'
#' @export
#'
#'
jd3_cholette<-function(s, t, rho=1, lambda=1, bias="None", conversion="Sum", obsposition=1){
  jd_s<-ts_r2jd(s)
  jd_t<-ts_r2jd(t)
  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/timeseries/TsData;", "cholette"
                  ,jd_s, jd_t, rho, lambda, bias, conversion, as.integer(obsposition))
  ts_jd2r(jd_rslt)
}

#' Multi-variate Cholette
#'
#' @param xlist
#' @param tcvector
#' @param ccvector
#' @param rho
#' @param lambda
#'
#' @return
#' @export
#'
#' @examples
jd3_mcholette<-function(xlist, tcvector=NULL, ccvector=NULL, rho=1, lambda=1) {
  if(!is.list(xlist) | length(xlist)<3 ) {
    stop("incorrect argument, first argument should be a list of at least 3 time series")}

  #create the input
  jdic=.jnew("demetra.util.r.Dictionary")
  for(i in seq_along(xlist)){
    .jcall(jdic, "V", "add", names(xlist[i]), ts_r2jd(xlist[[i]]))
  }
  if (is.null(tcvector)){
    ntc=0
    jtc<-.jcast(.jnull(), "[Ljava/lang/String;")
  }else if (! is.vector(tcvector)){
    stop("incorrect argument, constraints should be presented within a character vector")
  }else{
    ntc<-length(tcvector)
    jtc<-.jarray(tcvector, "java/lang/String")
  }
  if (is.null(ccvector)){
    ncc=0
    jcc<-.jcast(.jnull(), "[Ljava/lang/String;")
  }else if (! is.vector(ccvector)){
    stop("incorrect argument, constraints should be presented within a character vector")
  }else{
    ncc<-length(ccvector)
    jcc<-.jarray(ccvector, "java/lang/String")
  }
  if(ntc+ncc==0) {
    stop("both constraint types are empty, include at least one temporal or contemporaneous constraint")}

  jd_rslt<-.jcall("demetra/benchmarking/r/Benchmarking", "Ldemetra/util/r/Dictionary;", "multiCholette"
                  ,jdic,  jtc, jcc, rho, lambda)
  if (is.jnull(jd_rslt))
    return (NULL)
  rlist=list()
  rnames=.jcall(jd_rslt, "[S", "names")
  for(i in seq_along(rnames)){
    jts<-.jcall(jd_rslt, "Ldemetra/timeseries/TsData;", "get", rnames[i])
    if (! is.jnull(jts)){
      rlist[[rnames[i]]]<-ts_jd2r(jts)
    }
  }
  return (rlist)
}

