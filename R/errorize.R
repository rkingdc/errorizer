#' @export
errorize <- function(FUN, stopOnError=TRUE, onErrorReturn=NULL){
  fname  <- deparse(substitute(FUN))
  fname2 <- gsub('^.*::', '', fname)

  FUN2 <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    tryCatch(do.call(FUN, args),
             error = function(e){
               stime <- Sys.time()
               fname <- sprintf('./%s_error_%.f.Rds', fname2,  stime)
               saveRDS(object = list(error   = as.character(e),
                                     time    = stime,
                                     fxn     = FUN,
                                     fxnName = fname,
                                     arglst  = args),
                       file = fname)
               if (stopOnError) {
                 stop(sprintf('Wrote to %s on catching "%s"',
                              fname, as.character(e)))
               } else {
                 warning(sprintf('Wrote to ./%s_error_%.f7.Rds on catching "%s"',
                              Sys.time(), as.character(e)))
                 return(onErrorReturn)
               }
             },
             warning = function(w){
               stime <- Sys.time()
               fname <- sprintf('./%s_warning_%.f.Rds', fname2, stime)
               saveRDS(object = list(warning = as.character(w),
                                     time    = stime,
                                     fxn     = FUN,
                                     fxnName = fname,
                                     arglst  = args),
                       file = fname)
               warning(sprintf('Wrote to %s on catching "%s"',
                               Sys.time(), as.character(w)))
             })
  }
  formals(FUN2) <- formals(FUN)
  FUN2
}
