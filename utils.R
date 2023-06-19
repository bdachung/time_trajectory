library(scDHA)
library(ggplot2)
library(Rtsne)
library(viridis)

set.seed(1)

smooth_it <- function(x, y, n = 1000, method = "fmm") 
{
    t <- seq_along(x)
    new_t <- seq(min(t), max(t), length.out = n)
    new_x <- spline(t, x, xout = new_t, method = method)$y
    new_y <- spline(t, y, xout = new_t, method = method)$y
    data.frame(x = new_x, y = new_y)
}

timeInferenceFunc <- function(matrix_path, metadata_path=NULL, prefix=NULL) {

    tryCatch(
        exp = {
            df <- read.table(matrix_path, header = TRUE, sep = "\t")
        },
        error = function(e) { 
            return("Can not read file matrix")
        },
        warning = function(e) {
            return("Can not read file matrix")
        }
    )
    
    df <- t(df)

    tryCatch(
        exp = {
            df <- log2(df + 1)
        },
        error = function(e) {
            return("File matrix does not have the required format")
        },
        warning = function(e) {
            return("File matrix does not have the required format")
        }
    )

    cell_id <- rownames(df)

    metadata_df <- NULL
    if (!is.null(metadata_path)){
        tryCatch(
            exp = {
                metadata_df <- read.table(metadata_path, header = TRUE, sep = "\t")
                if (!"cell_id" %in% colnames(metadata_df)) {
                    return('"cell_id" is not in the metadata')
                }
                if (!"cell_type" %in% colnames(metadata_df)) {
                    return('"cell_type" is not in the metadata')
                }
            },
            error = function(e) {
                return("Can not read metadata file")
            },
            warning = function(e) {
                return("Can not read metadata file")
            }
        )
    }

    matrix <- as.matrix(df)

    tryCatch(
        exp = { 
            result <- scDHA(matrix, ncores = 2, seed=1)
        },
        error = function(e) {
            return("Error while training DHA module")
        },
        warning = function(e) {
            return("Error while training DHA module")
        }
    )

    latent <- result$latent

    #Generate pseudo-time for each cell, the input is the output from scDHA function
    tryCatch(
        exp = {
            result <- scDHA.pt(result, start.point = 1, ncores = 2, seed=1)
        },
        error = function(e) {
            return("Error while do time trajectory inference")
        },
        warning = function(e) {
            return("Error while do time trajectory inference")
        }
    )
    

    tsne_result <- Rtsne(latent)
    
    coordinates <- tsne_result$Y

    plot_df <- data.frame(cell_id = cell_id, x = coordinates[, 1], y = coordinates[, 2], values=result$pt)

    if (!is.null(metadata_df)){
        plot_df <- merge(plot_df, metadata_df, by.x = "cell_id", by.y = "cell_id", all.x = TRUE)
    }

    if ("cell_type" %in% colnames(plot_df)){
        unsorted_df <- plot_df[,c("cell_id","values","cell_type")]
        colnames(unsorted_df) <- c("cell_id","pseudo_time","cell_type")
    } else {
        unsorted_df <- plot_df[,c("cell_id","values")]
        colnames(unsorted_df) <- c("cell_id","pseudo_time")
    }
    
    plot_df <- plot_df[order(plot_df$values), ]

    lst <- seq(from=1, to=nrow(plot_df), by=nrow(plot_df) %/% 20)

    sub_df_sorted <- plot_df[lst, ]

    sub_df_sorted <- smooth_it(sub_df_sorted$x, sub_df_sorted$y)

    if (nrow(plot_df) %% 20 != 0){
        r <- tail(plot_df, 1)[,c("x","y")]
        sub_df_sorted <- rbind(sub_df_sorted, r)
    }

    if (is.null(metadata_df)){
        my_plot = ggplot(plot_df, aes(x=x, y=y)) + 
        geom_point(aes(colour=values), size=1) + 
        geom_point(data=head(plot_df, 1), aes(x=x, y=y, colour=values), size=2) + 
        geom_path(data=sub_df_sorted, aes(x=x, y=y),linewidth=0.3,colour="#ff0000") +
        scale_color_gradient(limits=c(0, max(plot_df$values))) + 
        scale_color_viridis(option = "D") + 
        labs(x="t-SNE1",y="t-SNE2",colour="pseudo_time") + 
        theme_bw() +  # Set the plot theme to a white background
        theme(
            axis.line = element_blank(),  # Turn off the axis lines
            panel.grid = element_blank(),  # Turn off the grid lines
            panel.background = element_rect(fill = "white") , # Set the background to white
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.direction = "horizontal",
            legend.key.height = unit(0.5, "cm"),
            legend.key.size = unit(1, "cm"),
            legend.position = "bottom",
            legend.title = element_text(size = 5, angle = 0, vjust = 0.5),
        )
    } else {
        my_plot <- ggplot(plot_df, aes(x=x, y=y)) + 
        geom_point(aes(colour=cell_type), size=1) + 
        geom_point(data=head(plot_df, 1), aes(x=x, y=y, colour=cell_type), size=3) + 
        geom_path(data=sub_df_sorted, aes(x=x, y=y),linewidth=0.3,colour="#ff0000") +
        scale_color_viridis(discrete = TRUE, option = "D") + 
        labs(x="t-SNE1",y="t-SNE2",colour="cell_type") + 
        theme_bw() +  # Set the plot theme to a white background
        theme(
            axis.line = element_blank(),  # Turn off the axis lines
            panel.grid = element_blank(),  # Turn off the grid lines
            panel.background = element_rect(fill = "white") , # Set the background to white
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            legend.direction = "horizontal",
            legend.key.height = unit(0.3, "cm"),
            legend.key.size = unit(0.1, "cm"),
            legend.position = "bottom",
            legend.title = element_text(size = 5, angle = 0, vjust = 0.5),
            legend.text = element_text(size=5)
        )
    }

    x_start <- sub_df_sorted[nrow(sub_df_sorted) - 1, "x"]
    y_start <- sub_df_sorted[nrow(sub_df_sorted) - 1, "y"]
    x_end <- sub_df_sorted[nrow(sub_df_sorted), "x"]
    y_end <- sub_df_sorted[nrow(sub_df_sorted), "y"]

    my_plot <- my_plot + 
        geom_segment(
            x = x_start,
            y = y_start,
            xend = x_end,  # Adjust the length of the arrow
            yend = y_end,  # Adjust the length of the arrow
            arrow=arrow(length=unit(0.02,"npc"),type="closed"),
            color = "#ff0000",
            linewidth=0.3
        ) 

    # Save the plot as an image file
    ggsave(filename = paste("images/", prefix, "_result.png", sep=""), plot = my_plot, width = 6, height = 4, dpi = 300)

    # Save inference result
    write.table(unsorted_df, file = paste("processed_files/", prefix, "_pt.tsv", sep=""), sep = "\t", col.names = TRUE, row.names=FALSE)

    # Create a latent data frame
    latent_data_frame <- data.frame(latent)
    rownames(latent_data_frame) <- cell_id
    write.table(latent_data_frame, file = paste("processed_files/", prefix, "_latent.tsv", sep=""), sep = "\t", col.names = FALSE)

    return(TRUE)
}