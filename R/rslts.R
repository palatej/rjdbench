proc_obj<-function(rslt, name){
  if (is.null(jdbench_env$jd_clobj)){
    jdbench_env$jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")
  }
  s<-.jcall(rslt, "Ljava/lang/Object;", "getData", name, jdbench_env$jd_clobj)
  return (s)
}

proc_numeric<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return (NULL)
  else
    return (.jcall(s, "D", "doubleValue"))
}

proc_vector<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  .jevalArray(s)
}

proc_int<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(-1)
  .jcall(s, "I", "intValue")
}

proc_bool<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(FALSE)
  .jcall(s, "Z", "booleanValue")
}

proc_ts<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return (NULL)
  if (.jinstanceof(s, "demetra/timeseries/TsData"))
    return(ts_jd2r(.jcast(s,"demetra/timeseries/TsData")))
  else
    return (NULL)
}

proc_str<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  .jcall(s, "S", "toString")
}

proc_desc<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  .jevalArray(s)
}

proc_test<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  desc<-.jcall(s, "S", "getDescription")
  val<-.jcall(s, "D", "getValue")
  pval<-.jcall(s, "D", "getPvalue")
  all<-c(val, pval)
  attr(all, "description")<-desc
  all
}

proc_parameter<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  val<-.jcall(s, "D", "getValue")
  e<-.jcall(s, "D", "getStandardError")
  c(val, e)
}

proc_parameters<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  p<-.jcastToArray(s)
  len<-length(p)
  all<-array(0, dim=c(len,2))
  for (i in 1:len){
    all[i, 1]<-.jcall(p[[i]], "D", "getValue")
    all[i, 2]<-.jcall(p[[i]], "D", "getStandardError")
  }
  all
}

proc_reg<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  desc<-.jcall(s, "S", "getDescription")
  val<-.jcall(s, "D", "getValue")
  e<-.jcall(s, "D", "getStdError")
  p<-.jcall(s, "D", "getPValue")
  all<-c(val, e, p)
  attr(all, "name")<-desc
  return (all)
}

proc_matrix<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return(NULL)
  return (matrix_jd2r(s))
}

proc_data<-function(rslt, name){
  s<-proc_obj(rslt, name)
  if (is.jnull(s))
    return (NULL)
  if (.jinstanceof(s, "demetra/timeseries/TsData"))
    return(ts_jd2r(.jcast(s,"demetra/timeseries/TsData")))
  else if (.jinstanceof(s, "java/lang/Number"))
    return (.jcall(s, "D", "doubleValue"))
  else if (.jinstanceof(s, "demetra/math/matrices/MatrixType"))
    return(matrix_jd2r(.jcast(s,"demetra/math/matrices/MatrixType")))
  else if (.jinstanceof(s, "[Ldemetra/data/ParameterEstimation;")){
    p<-.jcastToArray(s)
    len<-length(p)
    all<-array(0, dim=c(len,2))
    for (i in 1:len){
      all[i, 1]<-.jcall(p[[i]], "D", "getValue")
      all[i, 2]<-.jcall(p[[i]], "D", "getStandardError")
    }
    return (all)
  }
  else if (.jinstanceof(s, "demetra/data/ParameterEstimation")){
    desc<-.jcall(s, "S", "getDescription")
    val<-.jcall(s, "D", "getValue")
    e<-.jcall(s, "D", "getStandardError")
    p<-.jcall(s, "D", "getPvalue")
    all<-c(val, e, p)
    attr(all, "name")<-desc
    return (all)
  }
  else if (.jcall(.jcall(s, "Ljava/lang/Class;", "getClass"), "Z", "isArray"))
    return (.jevalArray(s, silent=TRUE))
  else
    return (.jcall(s, "S", "toString"))
}

proc_dictionary<-function(name){
  jmapping<-.jcall(name, "Ldemetra/information/InformationMapping;", "getMapping")
  jmap<-.jnew("java/util/LinkedHashMap")
  .jcall(jmapping, "V", "fillDictionary", .jnull("java/lang/String"), .jcast(jmap, "java/util/Map"), TRUE )
  jkeys<-.jcall(jmap, "Ljava/util/Set;", "keySet")
  size<-.jcall(jkeys, "I", "size")
  keys<-array(dim=size)
  jiter<-.jcall(jkeys, "Ljava/util/Iterator;", "iterator")
  for (i in 1:size){
    keys[i]=.jcall(.jcall(jiter, "Ljava/lang/Object;", "next"), "Ljava/lang/String;", "toString")
  }
  return (keys)
}
