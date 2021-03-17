library(tibble)
Traits_col <- read.csv(file = "traits_columns.csv")

options(scipen = 5, digits = 10)
Traits_col <- column_to_rownames(Traits_col, var = "X")

pvalues <- c()
for (i in 1:105) {
   possibleError <- tryCatch(
    t.test(as.data.frame((Traits_col[!is.na(Traits_col[,i]),]))[,i] ~ (Traits_col[!is.na(Traits_col[,i]),])$status),
    error=function(e)(e)
   )
   
   if(inherits(possibleError, "error")) next
   pvalues <- c(pvalues,(colnames((Traits_col[!is.na(Traits_col[,i]),])[i])),(t.test(as.data.frame((Traits_col[!is.na(Traits_col[,i]),]))[,i] ~ (Traits_col[!is.na(Traits_col[,i]),])$status)$p.value))
    
}

ptable <- data.frame(pvalues[seq(1, length(pvalues), 2)], pvalues[seq(2, length(pvalues), 2)])
colnames(ptable) <- c("Trait", "P-Value")
significance <- c()
for(i in 1:nrow(ptable)) {
  if (as.numeric(as.character(ptable$`P-Value`[i])) > 0.05) {
    significance <- c(significance, "Non-Significant")
  }
  else{
    significance <- c(significance, "Significant")
  }
}
ptable$Significance <- significance

