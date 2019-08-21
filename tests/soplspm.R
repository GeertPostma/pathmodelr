library(FactoMineR)
library(pathmodelr)


data(wine) #Loads the Val de Loire data into the "wine" variable

wine_data <- wine[,c(-1,-2, -31)]
inner_model_spec <- t(matrix(c(0,0,0,0,0,
                               1,0,0,0,0,
                               1,1,0,0,0,
                               1,1,1,0,0,
                               1,1,1,1,0),
                             nrow=5, ncol=5))

rownames(inner_model_spec) <- c("smell_at_rest", "view", "smell_after_shaking", "tasting", "global_quality")
colnames(inner_model_spec) <- rownames(inner_model_spec)

outer_model_spec <- list(1:5, 6:8, 9:18, 19:27, 28)

process_pls_model <- soplspm(wine_data, inner_model_spec, outer_model_spec)
