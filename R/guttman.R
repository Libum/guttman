.onAttach <- function(libname, pkgname) {
        packageStartupMessage("guttman - package for computing reliability of guttman scales")
}


#' @export
setClass(Class = "guttman", contains = "list", slots = c(dataset = "data.frame", optimal_dataset = "data.frame",
         optimal_row_errors = "numeric", optimal_column_errors = "numeric",
         total_errors = "numeric", TN = "numeric", N = "numeric", MMR = "numeric",
         ME = "numeric", CR = "numeric", CS = "numeric"))

#' @export
setMethod(f = "summary", signature = c(object = "guttman"), definition = function(object, ...){
        cat("Sorted table:\n\n")
        print(object@optimal_dataset)
        cat("\n")
        cat("Rows: ", nrow(object@optimal_dataset), "\tCols: ", ncol(object@optimal_dataset), "\tTotal: ", object@N)
        cat("\n\n")
        cat(paste0("Number of Guttman errors (in optimal solution): "), object@total_errors, "\n")
        cat("\n")
        cat("Optimal item ordering - from easiest to hardest: \n\n\t")
        cat(colnames(object@optimal_dataset))
        cat("\n\n")
        cat(paste0("Minimal Marginal Reproducibility: "), object@MMR, "\n")
        cat("\n")
        cat(paste0("Coefficient of Reproducibility: "), object@CR, "\n")
        cat("\n")
        cat(paste0("Coefficient of Scalability: "), object@CS, "\n")
})

#' Return mode (dominant) value from numerical vector
#'
#' @param v numeric vector
getmode <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
}


#' Create guttman object
#'
#' @param dataset a matrix or data frame with binary values
#'
#' @return Object of \emph{guttman} class, a list which contains:
#' \describe{
#'     \item{dataset}{data frame, original dataset provided as imput}
#'     \item{optimal_dataset}{data frame, dataset sorted in order to minimize Guttman errors}
#'     \item{optimal_row_errors}{numeric vector of row errors for optimal_dataset}
#'     \item{optimal_column_errors}{numeric vector of column errors for optimal_dataset}
#'     \item{total_errors}{numeric, total number of Guttman errors in optimal_dataset}
#'     \item{TN}{numeric, total number of mode categories in columns}
#'     \item{N}{numeric, total number of responses}
#'     \item{MMR}{numeric, Minimal Marginal Reproducibility}
#'     \item{ME}{numeric, Marginal Error}
#'     \item{CR}{numeric, Coefficient of Reproducibility}
#'     \item{CS}{numeric, Coefficient of Scalability }
#' }
#' @export
#'
#' @examples
#' data("SFd")
#' guttman_object = guttman(dataset = SFd)
#' summary(guttman_object)
guttman <- function(dataset) {

        if(any(apply(X = dataset, MARGIN = 1, FUN = function(x){sum(!(x %in% c(0,1)))}))){
                stop("Dataset isn't table of binary values")
        }

        if(length(dim(dataset)) != 2) stop("Wrong number of dimensions - provide two-dimensional table")
        dataset = as.data.frame(dataset)
        dataset_optimal = dataset[, order(apply(X = dataset, MARGIN = 2, FUN = sum),
                                          decreasing = TRUE)]
        dataset_optimal = dataset_optimal[order(apply(X = dataset_optimal, MARGIN = 1, FUN = sum),
                                        decreasing = TRUE),]

        row_errors = setNames(apply(X = dataset_optimal, MARGIN = 1, FUN = function(x){
                total = sum(x)
                ideal = c(rep(1,total), rep(0, length(x) - total))
                sum(x != ideal)
        }), rownames(dataset_optimal))

        column_errors = setNames(apply(X = dataset_optimal, MARGIN = 2, FUN = function(x){
                total = sum(x)
                ideal = c(rep(1,total), rep(0, length(x) - total))
                sum(x != ideal)
        }), colnames(dataset_optimal))

        sum_of_modal_categories = sum(apply(X = dataset_optimal, MARGIN = 2, FUN = function(x){
                sum(x == getmode(x))
        }))

        total_errors = sum(row_errors)

        N = prod(dim(dataset_optimal))

        MMR = sum_of_modal_categories / N
        ME = N - sum_of_modal_categories
        CR = 1 - (total_errors / N)
        CS = 1 - (total_errors / ME)

        result = new(Class = "guttman", dataset = dataset, optimal_dataset = dataset_optimal,
                      optimal_row_errors = row_errors, optimal_column_errors = column_errors,
                      total_errors = total_errors, TN = sum_of_modal_categories,
                      N = N, MMR = MMR, ME = ME, CR = CR, CS = CS)
        result
}


