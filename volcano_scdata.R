volcano_scdata <- function(data,cell_type_title) {
  # data: result of FindMarkers. cell_tyupe_title: title
  library(ggplot2)
  library(dplyr)
  library(ggrepel)
  aa <- unique(data$p_val)
  data$p_val[data$p_val==0]  <- aa[2]/2
  top_bottom_labels <- data %>%
    mutate(rank = ifelse(avg_log2FC > 0 & p_val<0.05, rank(desc(avg_log2FC)), rank(avg_log2FC))) %>%
    slice_max(n = 10, order_by = avg_log2FC) %>%
    select(avg_log2FC, p_val, rank) %>%
    bind_rows(data %>%
                mutate(rank = ifelse(avg_log2FC < 0 & p_val<0.05, rank(desc(avg_log2FC)), rank(avg_log2FC))) %>%
                slice_min(n = 10, order_by = avg_log2FC) %>%
                select(avg_log2FC, p_val, rank))
  top_bottom_labels$symbol = rownames(top_bottom_labels)
  
  # max_abs_logFC <- ceiling(max(abs(data$avg_log2FC))*10)/10
  max_abs_logFC <- ceiling(max(abs(data$avg_log2FC)))+0.5
  # if (max_abs_logFC-0.5 > max(abs(data$avg_log2FC))) {max_abs_logFC <- max_abs_logFC-0.5}
  
  fig <- ggplot(data, aes(x = avg_log2FC, y = -log10(p_val))) +
    geom_point(aes(color = case_when(avg_log2FC > 0.58 & p_val < 0.05 ~ "up",
                                     avg_log2FC < (-0.58) & p_val < 0.05 ~ "down",
                                     abs(avg_log2FC)<0.58 | p_val >0.05 ~ "non")), alpha = 0.7, show.legend = F) +
    scale_color_manual(values = c("up" = "brown2", "down" = "dodgerblue", "non" = "grey")) +
    geom_hline(yintercept = -log10(0.05), linetype = "dashed", color = "red") +
    geom_vline(xintercept = c(-0.58, 0.58), linetype = "dashed", color = "blue") +
    # scale_x_continuous(breaks = seq(-max_abs_logFC, max_abs_logFC, by = 1), limits = c(-max_abs_logFC, max_abs_logFC)) +
    scale_x_continuous(limits = c(-max_abs_logFC, max_abs_logFC)) +
    geom_text_repel(data = top_bottom_labels, 
                    # aes(label = symbol, color = ifelse(avg_log2FC > 0, "up", "down")),
                    aes(label = symbol),
                    nudge_x = 0.05, nudge_y = 0.05, show.legend = F, force = 10, max.overlaps = 30) +
    labs(x = "log2 Fold Change", y = "-log10(p-value)") +
    ggtitle(cell_type_title) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  print(fig)
  return(fig)
}


