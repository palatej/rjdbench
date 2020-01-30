
if (! isClass("JD3_TemporalDisaggregation")){
  setClass(
    Class="JD3_TemporalDisaggregation",
    contains = "JD3_ProcResults"
  )
}

#' @param series
#'
#' @param constant
#' @param trend
#' @param indicators
#' @param model
#' @param freq
#' @param conversion
#' @param conversion.obsposition
#' @param rho
#' @param rho.fixed
#' @param rho.truncated
#' @param zeroinitialization
#' @param diffuse.algorithm
#' @param diffuse.regressors
#'
#' @export
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
  jseries=ts_r2jd(series)
  jlist<-list()
  if (!is.null(indicators)){
    for (i in 1:length(indicators)){
      jlist[[i]]<-ts_r2jd(indicators[[i]])
    }
    jindicators<-.jarray(jlist, contents.class = "demetra/timeseries/TsData")
  }else{
    jindicators<-.jnull("[Ldemetra/timeseries/TsData;")
  }
  jrslt<-.jcall("demetra/benchmarking/r/TemporalDisaggregation", "Ldemetra/benchmarking/r/TemporalDisaggregation$Results;",
                "process", jseries, constant, trend, jindicators, model, as.integer(freq), conversion, as.integer(conversion.obsposition),rho, rho.fixed, rho.truncated,
                zeroinitialization, diffuse.algorithm, diffuse.regressors)
  return (new (Class = "JD3_TemporalDisaggregation", internal = jrslt))
}

#' Title
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

#' Log-likelihood and linked statistics
#'
#' @param JD3_TemporalDisaggregation
#'
#' @return
#' @export
#'
#' @examples
setMethod("logLik", "JD3_TemporalDisaggregation", function(object){
  if (is.null(object@internal)){
    NaN
  }else{
    proc_numeric(object@internal, "likelihood.ll")}
})

#' Coefficients of the regression model
#'
#' @param JD3_TemporalDisaggregation
#'
#' @return
#' @export
#'
#' @examples
setMethod("coef", "JD3_TemporalDisaggregation", function(object){
  if (is.null(object@internal)){
    NULL
  }else{
    proc_vector(object@internal, "c")}
})

#' Main output of a temporal disaggregation by regression model
#'
#' @param JD3_TemporalDisaggregation
#'
#' @return
#' @export
#'
#' @examples
setMethod("show", "JD3_TemporalDisaggregation", function(object){
  if (is.jnull(object@internal)){
    cat("Invalid estimation")
  }else{
    cat("Model", "\n")
    nx<-proc_numeric(object@internal,"nx")
    for (i in 1:nx){
      n<-paste("coeff(", i, ")", sep="")
      c<-proc_reg(object@internal, n)
      cat(attr(c, "name"), format(round(c[i], 4), scientific = FALSE), format(round(c[2], 4), scientific = FALSE), format(round(c[1]/c[2], 4), scientific = FALSE), format(round(c[3], 4), scientific = FALSE), "\n")
    }
  }
})

#' Detailed output of a temporal disaggregation by regression model
#'
#' @param JD3_TemporalDisaggregation
#'
#' @return
#' @export
#'
#' @examples
setMethod("summary", "JD3_TemporalDisaggregation",
          function(object){
            if (is.null(object)){
              cat("Invalid estimation")

            }else{
              getItemsLL<- c("Number of observations" = "likelihood.nobs",
                             "Number of effective observations" = "likelihood.neffective",
                             "Number of estimated parameters" = "likelihood.nparams",
                             "Loglikelihood " =	"likelihood.ll",
                             "Standarderror" = "",
                             "AIC" = "likelihood.aic",
                             "AICC"= "likelihood.aicc",
                             "BICC"= "likelihood.bicc" )
              cat("\n")
              cat("Likelihood statistics","\n")
              cat("\n")

              for (i in seq_along(getItemsLL)){
                myItemName <- names(getItemsLL)[i]
                if (myItemName != "Standarderror"){
                  myItem <-result(object,getItemsLL[i])
                } else {

                  myItemName <- "Standard error of the regression (ML estimate)"
                  myItem <-  sqrt(result(object,"likelihood.ssqerr")/result(object,"likelihood.neffective"))
                }
                cat(myItemName,myItem,"\n")
              }

              cat("\n")
              cat("\n")
              p<-result(object,"ml.parameters")
              if (! is.null(p)){
                cat("Model","\n")
                cat("\n")
                cat("Rho :",p,"\n")
                cat("\n")
                cat("\n")
              }
              cat("Regression model","\n")
              cat("\n")

              nx<-result(object,"nx")
              if (nx>0){
                cur<-"coeff(1)"
                modelMatrix <- as.double(format(round(result(object,cur),4), scientific=FALSE))
                if (nx >1){
                  for (i in 2:nx){
                    cur<-paste("coeff(", i, ")", sep="")
                    modelMatrix <- rbind(modelMatrix,as.double(format(round(result(object,cur),4), scientific=FALSE)))
                  }
                }
                dim(modelMatrix)<-c(nx, 3)

                colnames(modelMatrix)<-c("Coefficients","T-stat","P[|T|>t]")
                rownames(modelMatrix)<-paste("var-",1:nx, sep="")
                show(modelMatrix)
              }
            }
          }

)




