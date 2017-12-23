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