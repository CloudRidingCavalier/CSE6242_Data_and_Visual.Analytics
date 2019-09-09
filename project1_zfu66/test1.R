feature.set1.results <- model_evaluation(data=feature.set1, modelName="\nnumeric & converted non-numeric variables")
print(feature.set1.results$figure)
model.summary.1 <- feature.set1.results$model_summary
print(model.summary.1[model.summary.1$type == 'test', ])
trained_models.1 <- feature.set1.results$trained_models
p.value.list <- data.frame(summary(trained_models.1[[10]])$coef[summary(trained_models.1[[10]])$coef[,4] <= 1, 4])
colnames(p.value.list) <- c("p.value")
print(p.value.list)
sig.feature <- rownames(p.value.list)[p.value.list$p.value < 0.05]
#print(sig.feature)
sig.feature <- sig.feature[-1]
print(sig.feature)

feature.set1.1 <- feature.set1[,sig.feature]
feature.set1.1$Gross <- feature.set1$Gross
feature.set1.1.results <- model_evaluation(data=feature.set1.1, modelName="\nnumeric & converted non-numeric variables (reduced model)")
print(feature.set1.1.results$figure)
model.summary.1.1 <- feature.set1.1.results$model_summary
print(model.summary.1.1[model.summary.1.1$type == 'test', ])
trained_models.1.1 <- feature.set1.1.results$trained_models
p.value.list <- data.frame(summary(trained_models.1.1[[10]])$coef[summary(trained_models.1.1[[10]])$coef[,4] <= 0.05, 4])
colnames(p.value.list) <- c("p.value")
print(p.value.list)