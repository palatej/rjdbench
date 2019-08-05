
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
}

#' Title
#'
#' @param object JD3_ProcResults.
#' @param id character.
#'
#' @return
#' @export
#'
#' @examples
  setMethod("result", signature = c(object="JD3_ProcResults", id="character"), function(object, id){
    if (is.null(object@internal)){
      return (NULL)
    }else{
      return (proc_data(object@internal, id))
    }
  })
  #  lockBinding("result", .GlobalEnv)


if (!isGeneric("dictionary")){
  setGeneric(name="dictionary", def = function( object, ... ){standardGeneric("dictionary")})
}
#' Title
#'
#' @param JD3_ProcResults
#'
#' @return
#' @export
#'
#' @examples
setMethod("dictionary", "JD3_ProcResults", function(object){
    if (is.null(object@internal)){
      NULL
    }else{
      proc_dictionary(.jclass(object@internal))
    }

  })

