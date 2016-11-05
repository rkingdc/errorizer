#' @export
errorize <- function(FUN, stopOnError=TRUE, onErrorReturn=NULL, ...){
  fname2 <- gsub('^.*::', '', deparse(substitute(FUN)))

  FUN2 <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    tryCatch(do.call(FUN, args),
             error = function(e){
               stime <- Sys.time()
               flname <- sprintf('./%s_error_%.f.Rds', fname2,  as.numeric(stime))
               saveRDS(object = list(error   = as.character(e),
                                     time    = stime,
                                     fxn     = FUN,
                                     arglst  = args),
                       file = flname,
                       ...)
               if (stopOnError) {
                 stop(sprintf('Wrote to %s on catching "%s"',
                              flname, as.character(e)))
               } else {
                 warning(sprintf('Wrote to %s on catching "%s"',
                              flname, as.character(e)))
                 return(onErrorReturn)
               }
             },
             warning = function(w){
               stime <- Sys.time()
               flname <- sprintf('./%s_warning_%.f.Rds', fname2, as.numeric(stime))
               saveRDS(object = list(warning = as.character(w),
                                     time    = stime,
                                     fxn     = FUN,
                                     arglst  = args),
                       file = flname,
                       ...)
               warning(sprintf('Wrote to %s on catching "%s"',
                               flname, as.character(w)))
             })
  }
  formals(FUN2) <- formals(FUN)
  FUN2
}
