#' Breast Cancer EMT Subtype classifier
#'
#'A function to predict the subtypes of breast cancer
#'based on EMT with respect to a ML model.
#'
#' @param expr.dat An object of class matrix with samples as column names, gene symbols as row names, and entries the corresponding expression levels.
#' @param scale logical, whether to scale-normalization for data.
#' @return A dataframe  with the prediction results.
#' @export

predict_bcemtsubtype=function(expr.dat,scale=T){
genes=validation_input$genes
best_model=validation_input$best_model

valid.dat=expr.dat[genes,] %>%
  t() %>% as.data.frame()
if(scale==T){
  valid.dat=valid.dat %>% scale() %>%as.data.frame()
}else(
  valid.dat=valid.dat %>%as.data.frame()
)

# predict the subtype
valid_res=workflows:::predict.workflow(best_model,new_data =valid.dat,type='class') %>% as.data.frame()
colnames(valid_res)=c('cluster')
rownames(valid_res)=rownames(valid.dat)
return(valid_res)
}


