#' @export
errorize <- function(FUN, fileSuffix=NULL, stopOnError=TRUE, onErrorReturn=NULL, ...){
  fxname <- gsub('^.*::', '', deparse(substitute(FUN)))

  if (length(fileSuffix) > 1) {
    fileSuffix <- fileSuffix[1]
    warning("argument fileSuffix should be a vector of length 1. Only the first element will be used")
  }

  fs <- if (is.null(fileSuffix)) {
    paste0('_', as.character(as.integer(Sys.time())))
  } else if (is.na(fileSuffix)) {
    ''
  } else {
    as.character(fileSuffix[1])
  }


  FUN2 <- function() {
    args <- lapply(as.list(match.call())[-1L], eval, parent.frame())
    tryCatch(do.call(FUN, args),
             error = function(e){
               stime <- Sys.time()
               flname <- sprintf('./%s_error%s.Rds', fxname,  fs)
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
               flname <- sprintf('./%s_warning%s.Rds', fxname, fs)
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
