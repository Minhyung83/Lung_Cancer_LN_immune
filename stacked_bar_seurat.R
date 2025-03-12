stacked_bar_seurat <- function(object,group_to_present,ident_to_present, opt) {
  library(ggplot2)
  library(dplyr)
  library(Seurat)
  eval(parse(text = (paste0("celltype.prop <- as.data.frame(table(object$", ident_to_present, ", object$", group_to_present,"))"))))
  if (opt == "Cluster") {
    celltype.prop <- celltype.prop %>%
      rename(Cluster = Var1, Condition = Var2) %>%
      group_by(Cluster) %>%
      mutate(Percent = Freq / sum(Freq)*100)
    p <- ggplot(celltype.prop, aes(x = Cluster, y = Percent, fill = Condition))+
      geom_bar(stat = "identity", width = 0.9) +
      geom_text(aes(label = paste(round(Percent,2), "%")), position = position_stack(vjust =  0.5))
    print(p)
    return(p)
  } else if (opt == "Condition") {
    celltype.prop <- celltype.prop %>%
      rename(Cluster = Var1, Condition = Var2) %>%
      group_by(Condition) %>%
      mutate(Percent = Freq / sum(Freq)*100)
    p <- ggplot(celltype.prop, aes(x = Condition, y = Percent, fill = Cluster))+
      geom_bar(stat = "identity", width = 0.9) +
      geom_text(aes(label = paste(round(Percent,2), "%")), position = position_stack(vjust =  0.5))
    print(p)
    return(p)
  }
}

stacked_bar_seurat_nolabel <- function(object,group_to_present,ident_to_present, opt) {
  library(ggplot2)
  library(dplyr)
  library(Seurat)
  eval(parse(text = (paste0("celltype.prop <- as.data.frame(table(object$", ident_to_present, ", object$", group_to_present,"))"))))
  if (opt == "Cluster") {
    celltype.prop <- celltype.prop %>%
      rename(Cluster = Var1, Condition = Var2) %>%
      group_by(Cluster) %>%
      mutate(Percent = Freq / sum(Freq)*100)
    p <- ggplot(celltype.prop, aes(x = Cluster, y = Percent, fill = Condition))+
      geom_bar(stat = "identity", width = 0.9)
      # geom_text(aes(label = paste(round(Percent,2), "%")), position = position_stack(vjust =  0.5))
    print(p)
    return(p)
  } else if (opt == "Condition") {
    celltype.prop <- celltype.prop %>%
      rename(Cluster = Var1, Condition = Var2) %>%
      group_by(Condition) %>%
      mutate(Percent = Freq / sum(Freq)*100)
    p <- ggplot(celltype.prop, aes(x = Condition, y = Percent, fill = Cluster))+
      geom_bar(stat = "identity", width = 0.9)
      # geom_text(aes(label = paste(round(Percent,2), "%")), position = position_stack(vjust =  0.5))
    print(p)
    return(p)
  }
}