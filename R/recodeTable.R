#' recodeTable Function
#'
#' This function allows us to create recode table
#' @param 
#' @keywords 
#' @export
#' @examples
#' recodeTable()

recodeTable <- function(df,path){
  # column name to camel case
  names(df) <- rapportools::tocamel(tolower(names(df)), upper=FALSE)
  
  # summary stats
  sumStats<-psych::describe(df)
  sumStats$varname <- rownames(sumStats)
  names(sumStats)[names(sumStats)=="vars"]<-"varindex"
  sumStats <- sumStats[,c("varname","varindex","n","mean","sd","min","median","max","range")]
  
  # prepare column
  column <- data.frame(
    names(df),sapply(df, class)
  )
  names(column) <- c("variableName","variableTypeOriginal")
  row.names(column) <- seq_along(df)
  column$variableTypeNew <- NA
  
  recode <- data.frame(
    variableIndex = as.numeric(),
    variableName = as.character(),
    variableValueOriginal = as.character(),
    variableValueRecode = as.character()
  )
  recodeMaster <- recode
  
  for (i in seq_along(df)){
    # for (i in c(63,65)){
    varName <- names(df)[i]
    if (is.factor(df[,varName])) {
      n = nlevels(df[,varName])
      
      recode <- data.frame(
        variableIndex = rep(as.character(i),n),
        variableName = rep(varName,n),
        variableValueOriginal = unique(df[,varName]),
        variableValueRecode = as.character(rep("",n))
      )
      
      recodeMaster <- rbind(recodeMaster, recode)
      
    }
  }
  write.csv(recodeMaster, paste0(path,"/","recodeTable.csv",sep=""), row.names=FALSE)  
}


