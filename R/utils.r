library(tools, warn.conflicts = FALSE, quietly = T)
library(data.table, warn.conflicts = FALSE, quietly = T)
library(mlr, warn.conflicts = FALSE, quietly = T)

#String Concat
`%++%` <- function(a, b) paste0(a, b)
`%+ %` <- function(a, b) paste(a, b)
`%+/%` <- function(a, b) paste(a, b, sep='/')
`%+_%` <- function(a, b) paste(a, b, sep='_')   

#leitura dados
read_data <- function(file_path, ...){
    ext = file_ext(file_path)
    if(ext == 'csv'){
        return(as.data.frame(data.table::fread(input = file_path, stringsAsFactors = TRUE, showProgress = TRUE, ...)))
    } else if(ext == 'dat'){
        return(as.data.frame(data.table::fread(input = file_path, stringsAsFactors = TRUE, showProgress = TRUE, ...)))
    } else {
        stop(sprintf('Uknow file extension.'))
    }
}

#Cria métrica Gini
gini = makeMeasure(
    id = "gini", name = "Gini",
    properties = c("classif", "req.pred", "req.truth", "re.prob"),
    minimize = FALSE, best = 1, worst = 0,
    fun = function(task, model, pred, feats, extra.args) {
            #if (anyMissing(pred$data$response) || length(unique(pred$data$truth)) == 1L)
            #  return(NA_real_)
            2*measureAUC(getPredictionProbabilities(pred), pred$data$truth, pred$task.desc$negative, pred$task.desc$positive) - 1
          }
)