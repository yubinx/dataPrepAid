#' summaryReport Function
#'
#' This function allows us to create summary report
#' @param 
#' @keywords 
#' @export
#' @examples
#' summaryReport()

summaryReport <- function(df, path){
  
  pdf(paste0(path,"/","summaryReport.pdf",sep=""))
  
  
  t <- textGrob(paste0("The dataframe has ", dim(df)[1], " rows and ", dim(df)[2], " columns."))
  varType<-tableGrob(column[,c(1,2)])
  grid.arrange(varType,nrow=1,ncol=1,
               top=t)
  
  for(i in seq_along(df)){
    # for (i in 1:length(df)){
    var <- names(df)[i]
    if (is.numeric(df[,var])) {
      t <- textGrob(var)
      # p1 <- hist(df[,var],xlab="",main="",plot=TRUE)
      p1 <- qplot(df[,var],xlab="",ylab="",main="")
      p2 <- qplot(factor(0),df[,var],geom=c("boxplot"),xlab="",ylab="",main="")
      # p2 <- boxplot(df[,var],plot=TRUE)
      # p2 <- car::Boxplot(~df[,var], id=list(n=Inf))
      tbl <- tableGrob(t(sumStats[var,]))
      # t <- textGrob(car::Boxplot(~df[,var], id=list(n=Inf)))
      # t <- textGrob(paste0("Outlier(s):",paste(t$label,collapse=",")))
      grid.arrange(p1,p2,tbl,nrow=2,ncol=2,
                   top=t
                   # as.table=TRUE
      )
    }
    if (is.factor(df[,var])) {
      # df[,var]<-rapportools::tocamel(tolower(df[,var]), upper=FALSE)
      # print(var)
      if (length(unique(df[,var]))==length(df[,var])) {df[,var]<-as.character(df[,var]);next}
      t <- textGrob(var)
      freqTable <- as.data.frame(table(df[,var]))
      # rownames(freqTable) <- freqTable$Var1
      freqTable[,paste0("Prop n=",sum(freqTable$Freq),sep="")] <- scales::percent(freqTable$Freq/sum(freqTable$Freq))
      freqTable$Var1 <- sapply(lapply(freqTable$Var1, strwrap, width=60), paste, collapse="\n")
      
      tbl <- tableGrob(freqTable)
      grid.arrange(tbl,nrow=1,ncol=1,
                   top=t
                   # as.table=TRUE
      )
    }
    
  }
  
  # dfnum <- Filter(is.numeric,df)
  # M <- cor(dfnum,na.rm=TRUE)
  # corrplot.mixed(M, lower.col = "black", number.cex = .7)
  
  dev.off()
  
}