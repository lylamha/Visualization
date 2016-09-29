library(ggplot2)
library(gridExtra)
library(grid)
library(mixtools) # gaussian mixture models

count_files <- list.files(paste0(data_dir, "counts/human_mouse/"),
                          pattern = "*counts.csv")

plot_list <- lapply(count_files, function(file) {
  
  count_dat   <- fread(paste0(data_dir, file))
  barcode_idx <- grep("[ACGT]{14}", colnames(count_dat))
  
  # mouse genes
  gene_idx       <- grep("ENSMUSG", count_dat$V1)
  detected_mouse <- colSums(count_dat[gene_idx, barcode_idx, with=F] >= 1)
  
  
  # human genes
  gene_idx       <- grep("ENSG", count_dat$V1)
  detected_human <- colSums(count_dat[gene_idx, barcode_idx, with=F] >= 1)
  
  
  df <- data.frame(detected_human , detected_mouse)
 
  
  ### gaussian mixture modelling to classify mouse/human/multi cells
  mix_model_mouse <- normalmixEM(log10(detected_mouse + 1))
  mix_model_human <- normalmixEM(log10(detected_human + 1))
  
  
  df$mouse_model_pred <- ifelse(
    mix_model_mouse$posterior[,1] > mix_model_mouse$posterior[,2],
    "human",
    "mouse")
  
  df$human_model_pred <- ifelse(
    mix_model_human$posterior[,1] > mix_model_human$posterior[,2],
    "mouse",
    "human")
  
  df$predicted_cell <- ifelse(
    df$mouse_model_pred == df$human_model_pred,
    df$mouse_model_pred,
    "multi")
  
  df$predicted_cell <- as.factor(df$predicted_cell)
  
  # legend labels with fraction of cells
  cell_percentage <- table(df$predicted_cell) / nrow(df) * 100
  levels(df$predicted_cell) <- paste0(levels(df$predicted_cell),
                                      ": ", cell_percentage, "%")
  
  ### plotting
  
  # placeholder plot - prints nothing at all
  empty <- ggplot() + geom_point(aes(1,1), colour="white") +
    theme(                              
      plot.background = element_blank(), 
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(), 
      panel.border = element_blank(), 
      panel.background = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank()
    )
  
  scatter <- ggplot(
    df, aes(x = detected_human,
            y = detected_mouse,
            colour = predicted_cell)) +
    geom_point() +
    theme_bw() +
    xlab("number of detected genes in human") + 
    ylab("number of detected genes in mouse") +
    ggtitle(gsub(".counts.csv", "", file)) +
    theme(legend.justification=c(1,1), legend.position=c(1,1))
  
  
  # marginal density of x - plot on top
  plot_top <- qplot(
    detected_human,
    y = ..density..,
    geom = "histogram", alpha = 0.2) + 
    geom_density() +
    theme_bw() +
    theme(legend.position = "none")
  
  
  # marginal density of y - plot on the right
  plot_right <- qplot(
    detected_mouse,
    y = ..density..,
    geom = "histogram", alpha = 0.2) + 
    geom_density() +
    coord_flip() +
    theme_bw() +
    theme(legend.position = "none") 
  
  # arrange the plots together
  # with appropriate height and width for each row and column
  grid.arrange(plot_top, empty, scatter, plot_right,
               ncol = 2, nrow = 2, widths=c(4, 1), heights=c(1, 4))
  
})

