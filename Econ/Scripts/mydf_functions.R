#Updated 5/7/2022
#Functions to create tables from dataframes to write to word

df.to.wordtable <- function(df){
  
  library(officer)
  library(flextable)
  
  df.to.convert <- df
  
  rows <- which(df.to.convert$p_value < 0.05)
  
  df.to.convert %>% 
    flextable() %>% 
    border_outer(part = "all", border = fp_border(color ="black")) %>%
    border_inner(part = "all", border = fp_border(color ="black")) %>% 
    bold(i = rows) %>% 
    #flextable::autofit()
    flextable::set_table_properties(layout = "autofit")
}

#function to take multiple dataframes, convert to wordtable and write to word document
df.wordtable.loop <- function(explicit.list) {
  
  doc_for_writing <- officer::read_docx()
  for (i in 1:length(explicit.list)) {
    or.df <- explicit.list[[i]]
    
    d <- substitute(explicit.list)
    n <- sapply(d[-1], deparse)
    or.df.name <- n[i]
    
    df <- or.df %>% df.to.wordtable()
    
    doc_for_writing <- doc_for_writing %>% 
      body_add_par(paste(or.df.name), style = "Normal") %>%
      body_add_flextable(df, align = "center")
    
    rm(df)
    
  }
  doc_for_writing
}


my.summary.function <- function(dat, #colname =  c("mean", "median", "lower_ci", "upper_ci"), 
                                #row.name, 
                                margin = 2, frst.ql =0.025 , upper.ql =0.975)
{
  
  output <- data.frame()
  
  my.mean <- round(apply(dat, margin, mean, na.rm = TRUE))
  
  my.median <- round(apply(dat, margin, median, na.rm = TRUE))
  
  my.lowerql <- round(apply(dat,margin, quantile, frst.ql, na.rm = TRUE))
  
  my.upperql <- round(apply(dat,margin, quantile, upper.ql, na.rm = TRUE))
  
  output <- rbind(output, 
                  cbind(
                    mean = paste0(my.mean, " (",
                                  my.lowerql, ", ",
                                  my.upperql, ")"
                    ),
                    median = my.median
                  )
  )
  
  rownames(output) <- colnames(dat)
  
  return(output)
  
}
