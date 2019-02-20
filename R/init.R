if(!require(rJava)){
  install.packages("rJava")
}
library("rJava")
.jinit()
.jaddClassPath("./Java/demetra-design-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-api-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-basic-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-modelling-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-regarima-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-ssf-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-toolkit-r-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-benchmarking-api-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-benchmarking-core-1.0.0-SNAPSHOT.jar")
.jaddClassPath("./Java/demetra-benchmarking-r-1.0.0-SNAPSHOT.jar")

jdbench_env <- new.env(parent = emptyenv())
