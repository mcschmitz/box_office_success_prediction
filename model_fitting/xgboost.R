load("data/data_final/data_final.RData")
require(mlr)
require(mlrMBO)
require(parallelMap)
require(fastDummies)

# small utility function that creates a learner out of tuned params
make_tuned_learner =  function(x){makeLearner("regr.xgboost", par.vals = x$x)}

# exp. MAPE
f_rel = function(task, model, pred, feats, extra.args){
  mean(abs(exp(pred$data$response) - exp(pred$data$truth))/exp(pred$data$truth))
}
expMAE_rel = makeMeasure("Mean of relative exponential Errors", minimize = TRUE, fun = f_rel,
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
inner = makeResampleDesc("RepCV", folds = 10, reps = 3)
mbo_ctrl = makeMBOControl()
mbo_ctrl = setMBOControlTermination(mbo_ctrl, iters = tune_iters)
surrogate_lrn =  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget = 10^-6,
                             control = list(trace = FALSE))
ctrl = mlr:::makeTuneControlMBO(learner = surrogate_lrn, mbo.control = mbo_ctrl)
lrn_mape = makeTuneWrapper("regr.xgboost", resampling = inner, measures = expMAE_rel, par.set = ps_xgb, control = ctrl, 
                           show.info = TRUE)
lrn_mape = setHyperPars(lrn_mape, verbose = 0, nthread=7)
outer = makeResampleDesc("CV", iters = 3)
variables = c("Kinderfilm", "FSK", "Genre_Prognose", "Kopien", "Verleiherkategorie", "Jahr_gerade", 
              "Besucher_wochenende1_log", "Woerter", "Saison", 
              paste0("Woche", 6:1, "_haupttitel"),
              paste0("Aggregation", 6:1, "_haupttitel"),
              paste0("Woche", 6:1, "_gesamttitel"),
              paste0("Aggregation", 6:1, "_gesamttitel"),
              paste0("Woche", 6:1, "_haupttitel_film"),
              paste0("Aggregation", 6:1, "_haupttitel_film"),
              paste0("Woche", 6:1, "_kino"),
              paste0("Woche", 6:1),
              paste0("Aggregation", 6:1))
data_final = data_final[variables]
dummies = dummy_cols(data_final[, sapply(data_final, is.factor)], remove_most_frequent_dummy = TRUE)
data_final = cbind(data_final[, !(sapply(data_final, is.factor))], 
                    dummies[, !(sapply(dummies, is.factor))])
variables1 = colnames(data_final)

# Week1
variables1 = variables1[which(colnames(data_final) %in% variables1)]
# create task
movies_task1 = makeRegrTask(data = data_final[, variables1], target = "Besucher_wochenende1_log")
set.seed(101)
# Perform parallel 3 fold CV with inner model tuning
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned1_mape = resample(lrn_MAPE, movies_task1, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Week2
# Remove features that are not known 2 weeks before premiere
variables2 = variables1[!(grepl(paste(paste0(c("Woche", "Aggregation"), 1), sep="|"), variables1))]
movies_task2 = makeRegrTask(data = data_final[, variables2], target = "Besucher_wochenende1_log")
set.seed(102)
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned2_mape = resample(lrn_mape, movies_task2, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Week3
variables3 = variables2[!(grepl(paste(paste0(c("Woche", "Aggregation"), 2), sep="|"), variables2))]
movies_task3 = makeRegrTask(data = data_final[, variables3], target = "Besucher_wochenende1_log")
set.seed(103)
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned3_mape = resample(lrn_mape, movies_task3, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Week4
variables4 = variables3[!(grepl(paste(paste0(c("Woche", "Aggregation"), 3), sep="|"), variables3))]
movies_task4 = makeRegrTask(data = data_final[, variables4], target = "Besucher_wochenende1_log")
set.seed(104)
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned4_mape = resample(lrn_mape, movies_task4, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Week5
variables5 = variables4[!(grepl(paste(paste0(c("Woche", "Aggregation"), 4), sep="|"), variables4))]
movies_task5 = makeRegrTask(data = data_final[, variables5], target = "Besucher_wochenende1_log")
set.seed(105)
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned5_mape = resample(lrn_mape, movies_task5, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Week6
variables6 = variables5[!(grepl(paste(paste0(c("Woche", "Aggregation"), 5), sep="|"), variables5))]
movies_task6 = makeRegrTask(data = data_final[, variables6], target = "Besucher_wochenende1_log")
set.seed(106)
parallelStartSocket(3, show.info = TRUE, logging = TRUE, storagedir = "results/xgboost/logs")
xgb_tuned6_mape = resample(lrn_mape, movies_task6, measures = expMAE_rel, resampling = outer, extract = getTuneResult)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# Take the mean of the 3 tuned models for the prediction and do a Leave-one-out CV to estimate the perfromance
# Week1
load("results/xgboost/single_models/xgb_tuned_mape.RData")
# Create ensemble learner that averages the predictions of the 3 tuned learners
lrns1 = lapply(xgb_tuned1_mape$extract, make_tuned_learner)
lrns1[[1]]$id = 1; lrns1[[2]]$id = 2; lrns1[[3]]$id = 3
stacked_lrn1 = makeStackedLearner(base.learners = c(lrns1), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo1 = resample(stacked_lrn1, movies_task1, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# LOO Week2
lrns2 = lapply(xgb_tuned2_mape$extract, make_tuned_learner)
lrns2[[1]]$id = 1; lrns2[[2]]$id = 2; lrns2[[3]]$id = 3
stacked_lrn2 = makeStackedLearner(base.learners = c(lrns2), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo2 = resample(stacked_lrn2, movies_task2, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# LOO Week3
lrns3 = lapply(xgb_tuned3_mape$extract, make_tuned_learner)
lrns3[[1]]$id = 1; lrns3[[2]]$id = 2; lrns3[[3]]$id = 3
stacked_lrn3 = makeStackedLearner(base.learners = c(lrns3), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo3 = resample(stacked_lrn3, movies_task3, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# LOO Week4
lrns4 = lapply(xgb_tuned4_mape$extract, make_tuned_learner)
lrns4[[1]]$id = 1; lrns4[[2]]$id = 2; lrns4[[3]]$id = 3
stacked_lrn4 = makeStackedLearner(base.learners = c(lrns4), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo4 = resample(stacked_lrn4, movies_task4, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")

# LOO Week5
lrns5 = lapply(xgb_tuned5_mape$extract, make_tuned_learner)
lrns5[[1]]$id = 1; lrns5[[2]]$id = 2; lrns5[[3]]$id = 3
stacked_lrn5 = makeStackedLearner(base.learners = c(lrns5), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo5 = resample(stacked_lrn5, movies_task5, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")
parallelStop()

# LOO Week6
lrns6 = lapply(xgb_tuned6_mape$extract, make_tuned_learner)
lrns6[[1]]$id = 1; lrns6[[2]]$id = 2; lrns6[[3]]$id = 3
stacked_lrn6 = makeStackedLearner(base.learners = c(lrns6), method = "average", predict.type = "response")
parallelStartSocket(7, show.info = TRUE)
loo6 = resample(stacked_lrn6, movies_task6, makeResampleDesc("LOO"), measures = expMAE_rel)
parallelStop()
save.image("results/xgboost/single_models/xgb_tuned_mape.RData")
parallelStop()