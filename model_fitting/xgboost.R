load("data/data_final.RData")
require(mlr)
require(mlrMBO)
require(parallelMap)
require(fastDummies)

# small utility function that creates a learner out of tuned params
make_tuned_learner =  function(x){makeLearner("regr.xgboost", par.vals = x$x)}

# exp. MAPE
f_expmape = function(task, model, pred, feats, extra.args){
  mean(abs(exp(pred$data$response) - exp(pred$data$truth))/exp(pred$data$truth))
}
expMAE_rel = makeMeasure("Mean of relative exponential Errors", minimize = TRUE, fun = f_expmape,
                         properties = c("regr", "response"))

# exp. MAE
f_expmae = function(task, model, pred, feats, extra.args){
  mean(abs(exp(pred$data$response) - exp(pred$data$truth)))
}
exp_MAE = makeMeasure("Mean of absolute exponential Errors", minimize = TRUE, fun = f_expmae, 
                     properties = c("regr", "response"))

# search space
ps_xgb = makeParamSet(
  makeIntegerParam("nrounds", lower = 5, upper = 3000, default = 200),
  makeNumericParam("eta", lower = -7, upper = -2, default = -4, trafo = function(x) 2^x),
  makeIntegerParam("max_depth", lower = 2, upper = 8, default = 3),
  makeNumericParam("colsample_bytree", lower = 0.3, upper = 1, default = 0.6),
  makeNumericParam("subsample", lower = 0.1, upper = 1, default = 0.6)
)

# tune config
tune_iters = 40 * length(ps_xgb$pars)
cvdesc = makeResampleDesc("CV", iters = 10)
mbo_ctrl = makeMBOControl()
mbo_ctrl = setMBOControlTermination(mbo_ctrl, iters = tune_iters)
surrogate_lrn =  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget = 10^-6,
                             control = list(trace = FALSE))
ctrl = mlr:::makeTuneControlMBO(learner = surrogate_lrn, mbo.control = mbo_ctrl)
lrn = makeLearner("regr.xgboost")
variables = c("kids_movie", "rating", "genre", "copies", "studio", "year_even", "visitors_premiere_weekend_log",
              "words", "season", 
              paste0("week", 6:1, "_main_title"), paste0("aggregation", 6:1, "_main_title"),
              paste0("week", 6:1, "_complete_title"), paste0("aggregation", 6:1, "_complete_title"),
              paste0("week", 6:1, "_main_title_film"), paste0("aggregation", 6:1, "_main_title_film"),
              paste0("week", 6:1, "_kino"),
              paste0("week", 6:1), paste0("aggregation", 6:1))
data_final = data_final[variables]
dummies = dummy_cols(data_final[, sapply(data_final, is.factor)], remove_most_frequent_dummy = TRUE)
data_final = cbind(data_final[, !(sapply(data_final, is.factor))], 
                    dummies[, !(sapply(dummies, is.factor))])
vars = colnames(data_final)

# training sample
train_size <- floor(0.75 * nrow(data_final))
set.seed(101)
train_idx <- sample(seq_len(nrow(data_final)), size = train_size)

train <- data_final[train_idx, ]
test <- data_final[-train_idx, ]

# Tune models
for (i in 1:6){
  set.seed(i)
  vars = vars[which(colnames(data_final) %in% vars)]
  task = makeRegrTask(data = train[, vars], target = "visitors_premiere_weekend_log")
  parallelStartSocket(parallel::detectCores() / 2 - 1, show.info = TRUE, level = "mlr.resample")
  tune_result = tuneParams(lrn, task = task, par.set = ps_xgb, resampling = cvdesc, control = ctrl, 
                           measures = list(exp_MAE, mse, expMAE_rel), show.info = TRUE)
  parallelStop()
  assign(paste0("vars", i), vars)
  assign(paste0("tune_result", i), tune_result)
  assign(paste0("task", i), task)
  save.image("results/xgboost.RData")
}
