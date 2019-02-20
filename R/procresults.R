
if (! isClass("JD3_Object")){
  setClass(
    Class="JD3_Object",
    representation = representation(internal = "jobjRef" )
  )
}

if (! isClass("JD3_ProcResults")){
  setClass(
    Class="JD3_ProcResults",
    contains = "JD3_Object"
  )
}


if (! isGeneric("result" )){
  setGeneric(name="result", def = function( object, id, ... ){standardGeneric("result")})
  setMethod("result", signature = c(object="JD3_ProcResults", id="character"), function(object, id){
    if (is.null(object@internal)){
      NULL
    }else{
      proc_data(object@internal, id)}
  })
#  lockBinding("result", .GlobalEnv)
}

if (!isGeneric("dictionary")){
  setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})
  setMethod("dictionary", "JD3_ProcResults", function(object){
    if (is.null(object@internal)){
      NULL
    }else{
      proc_dictionary(.jclass(object@internal))
    }

  })

#  lockBinding("dictionary", .GlobalEnv)
}

jd3_result<-function(object, id){
  if (is.null(jdbench_env$jd_clobj)){
    jdbench_env$jd_clobj<-.jcall("java/lang/Class", "Ljava/lang/Class;", "forName", "java.lang.Object")

  }
  return (result(object, id))
}



jd3_dictionary<-function(object){
  return (dictionary(object))
}
