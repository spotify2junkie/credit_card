nnet_big_form=train(huge_form, data = cred_trn_new, method = 'nnet', preProcess = c('center', 'scale'), trControl = numFolds, tuneGrid=expand.grid(size=c(10), decay=c(0.01)))

# prediction and confusion matrix for xgboost 

# Create our prediction probabilities
pred = predict(xgb_cred, dtrain)

# Set our cutoff threshold
pred.resp <- ifelse(pred >= 0.86, 1, 0)

# Create the confusion matrix
confusionMatrix(pred.resp, test.label, positive="1")

# ==========================================

# for the accuracy plot 

pred = predict(xgb_cred, dtrain)
score_acc = rep(0, length(cutoffs))
score_roc = rep(0, length(cutoffs))
# test cutoffs for accuracy tuned forest
for (c in seq_along(cutoffs)) {
  pred.resp = ifelse(pred > cutoffs[c],1,0)
  score_acc[c] = score(predicted = pred.resp,
                       actual = cred_tst_new$DEFAULT)
}

# ==========================================


# xgboost plot 



# Use ROCR package to plot ROC Curve
xgb.pred <- prediction(pred, test.label)
xgb.perf <- performance(xgb.pred, "tpr", "fpr")

plot(xgb.perf,
     avg="threshold",
     colorize=TRUE,
     lwd=1,
     main="ROC Curve w/ Thresholds",
     print.cutoffs.at=seq(0, 1, by=0.05),
     text.adj=c(-0.5, 0.5),
     text.cex=0.5)
grid(col="lightgray")
axis(1, at=seq(0, 1, by=0.1))
axis(2, at=seq(0, 1, by=0.1))
abline(v=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
abline(h=c(0.1, 0.3, 0.5, 0.7, 0.9), col="lightgray", lty="dotted")
lines(x=c(0, 1), y=c(0, 1), col="black", lty="dotted")

# ================================