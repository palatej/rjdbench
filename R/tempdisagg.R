#' @include rslts.R
NULL


#' Temporal disaggregation of a time series by regression models. Includes Chow-Lin, Fernandez, Litterman and some variants of those algorithms
#'
#' @param series The time series taht will be disaggregated
#' @param constant Constant term (T/F)
#' @param trend Linear trend (T/F)
#' @param indicators High-frequency indicator used in the temporal disaggregation
#' @param model Model of the error term (at the disaggregated level). "Ar1" = Chow-Lin, "Rw" = Fernandez, "RwAr1" = Litterman
#' @param freq Annual frequency of the disaggregated variable. Used if no indicator is provided
#' @param conversion Conversion mode (Usually "Sum" or "Average")
#' @param conversion.obsposition Only used with "UserDefined" mode. Position of the observed indicator in the aggregated periods (for instance 7th month of the year)
#' @param rho Only used with Ar1/RwAr1 models. Initial value of the parameter
#' @param rho.fixed Fixed rho (T/F)
#' @param rho.truncated Range for Rho evaluation (in [rho.truncated, 1[)
#' @param zeroinitialization Initial values of auto-regressive models are fixed to 0
#' @param diffuse.algorithm Algorithm used for diffuse initialization
#' @param diffuse.regressors Indicates if the coefficients of the regression model are diffuse (T) or fixed unknown (F, default)
#'
#' @return
#' @export
#'
#' @examples
jd3_tempdisagg<-function(series, constant=T, trend=F, indicators=NULL,
                         model=c("Ar1", "Rw", "RwAr1"), freq=4,
                         conversion=c("Sum", "Average", "Last", "First", "UserDefined"), conversion.obsposition=1,
                         rho=0, rho.fixed=F, rho.truncated=0,
                         zeroinitialization=F, diffuse.algorithm=c("SqrtDiffuse", "Diffuse", "Augmented"), diffuse.regressors=F){
  model=match.arg(model)
  conversion=match.arg(conversion)
  diffuse.algorithm=match.arg(diffuse.algorithm)
  if (model!="Ar1" && !zeroinitialization){
    constant=F
  }
  jseries<-ts_r2jd(series)
  jlist<-list()
  if (!is.null(indicators)){
    if (is.list(indicators)){
      for (i in 1:length(indicators)){
        jlist[[i]]<-ts_r2jd(indicators[[i]])
      }
    }else if (is.ts(indicators)){
      jlist[[1]]<-ts_r2jd(indicators)
    }else{
      stop("Invalid indicators")
    }
    jindicators<-.jarray(jlist, contents.class = "demetra/timeseries/TsData")
  }else{
    jindicators<-.jnull("[Ldemetra/timeseries/TsData;")
  }
  jrslt<-.jcall("demetra/benchmarking/r/TemporalDisaggregation", "Ldemetra/benchmarking/r/TemporalDisaggregation$Results;",
                "process", jseries, constant, trend, jindicators, model, as.integer(freq), conversion, as.integer(conversion.obsposition),rho, rho.fixed, rho.truncated,
                zeroinitialization, diffuse.algorithm, diffuse.regressors)

  # Build the S3 result
  bcov<-proc_matrix(jrslt, "covar")
  vars<-proc_vector(jrslt, "regnames")
  coef<-proc_vector(jrslt, "coeff")
  se<-sqrt(diag(bcov))
  t<-coef/se
  m<-data.frame(coef, se, t)
  m<-`row.names<-`(m, vars)

  regression<-list(
    type=model,
    conversion=conversion,
    model=m,
    cov=bcov
  )
  estimation<-list(
    disagg=proc_ts(jrslt, "disagg"),
    edisagg=proc_ts(jrslt, "edisagg"),
    regeffect=proc_ts(jrslt, "regeffect"),
    smoothingpart=proc_numeric(jrslt, "smoothingpart"),
    parameter=proc_numeric(jrslt, "parameter"),
    eparameter=proc_numeric(jrslt, "eparameter")
    # res= TODO
  )
  likelihood<-proc_likelihood(jrslt, "likelihood.")

  return(structure(list(
    regression=regression,
    estimation=estimation,
    likelihood=likelihood),
    class="JDTempDisagg"))



}

#' Temporal disaggregation wirhout indicator
#'
#' @param series
#' @param indicator
#' @param model
#' @param conversion
#' @param conversion.obsposition
#' @param rho
#' @param rho.fixed
#' @param rho.truncated
#'
#' @return
#' @export
#'
#' @examples
jd3_tempdisagg2<-function(series, indicator, model=c("Ar1", "Rw"),
                         conversion=c("Sum", "Average", "Last", "First", "UserDefined"), conversion.obsposition=1,
                         rho=0, rho.fixed=F, rho.truncated=0){
  model=match.arg(model)
  conversion=match.arg(conversion)
  jseries=ts_r2jd(series)
  jlist<-list()
  jindicator<-ts_r2jd(indicator)
  jrslt<-.jcall("demetra/benchmarking/r/TemporalDisaggregation", "Ldemetra/timeseries/TsData;",
                "processI", jseries, jindicator, model, conversion, as.integer(conversion.obsposition),rho, rho.fixed, rho.truncated)
  return (ts_jd2r(jrslt))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
logLik.JDTempDisagg<-function(object){
  if (is.null(object@internal)){
    NaN
  }else{
    proc_numeric(object@internal, "likelihood.ll")}
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
coef.JDTempDisagg<-function(object){
  return (object$regression$model$coef)
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
print.JDTempDisagg<-function(object){
  if (is.null(object$regression$model)){
    cat("Invalid estimation")
  }else{
    cat("Model:", object$regression$type, "\n")
    print(object$regression$model)
  }
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
summary.JDTempDisagg<-function(object){
  if (is.null(object)){
    cat("Invalid estimation")

  }else{
    cat("\n")
    cat("Likelihood statistics","\n")
    cat("\n")
    cat("Number of observations: ", object$likelihood$nobs, "\n")
    cat("Number of effective observations: ", object$likelihood$neffective, "\n")
    cat("Number of estimated parameters: ", object$likelihood$nparams, "\n")
    cat("Standard error: ", object$likelihood$ser, "\n")
    cat("AIC: ", object$likelihood$aic, "\n")
    cat("BIC: ", object$likelihood$bic, "\n")

    cat("\n")
    cat("\n")
    cat("Model:", object$regression$type, "\n")
    p<-object$estimation$parameter
    if (! is.nan(p)){
      cat("Rho :",p," (", object$estimation$eparameter, ")\n")
      cat("\n")
      cat("\n")
    }
    cat("Regression model","\n")
    print(object$regression$model)

  }
}






