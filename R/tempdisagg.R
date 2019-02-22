
if (! isClass("JD3_TemporalDisaggregation")){
  setClass(
    Class="JD3_TemporalDisaggregation",
    contains = "JD3_ProcResults"
  )
}

jd3_tempdisagg<-function(series, constant=T, trend=F, indicators=NULL,
                         model="Ar1", freq=4, conversion="Sum", conversion.obsposition=1, rho=0, rho.fixed=F, rho.truncated=0,
                         zeroinitialization=F, diffuse.algorithm="SqrtDiffuse", diffuse.regressors=F){
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
  jrslt<-.jcall("demetra/benchmarking/r/TemporalDisaggregation", "Ldemetra/tempdisagg/univariate/TemporalDisaggregationResults;",
                 "process", jseries, constant, trend, jindicators, model, as.integer(freq), conversion, as.integer(conversion.obsposition),rho, rho.fixed, rho.truncated,
                 zeroinitialization, diffuse.algorithm, diffuse.regressors)
  return (new (Class = "JD3_TemporalDisaggregation", internal = jrslt))
}

