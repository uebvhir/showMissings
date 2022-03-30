## showNAs.R - compiled by RoxygenReady, a package by @vertesy
# This function transforms a data.frame or tibble into a data frame
# whose content is 1 if the value is not an NA or 0 in case it is.
# Additionally the rows are sorted according the order defined by "rowGroups" levels, set by "varLabels"
# Also columns are sorted by order defined by  by "varNames"
# "varGroups" and "varLabels" are only used to define variable groups and names to be used in heatmap.
#'
#' @param x A data.frame with named rows
#' @param rowGroups A vector, with length=nrows(x), as the data frame to define groups
#' @param rowLabels A vector to define group names, with as many elements as groups.
#' @param rowGroupsColors A vector to define the colors of groups of individuals
#' @param varNames A vector , with length=ncols(x)-1, of variable names that define the columns of the data.frame
#' @param varGroups A vector of groups of variables with the number of variables in each group
#' @param varLabels A vector with the names of the groups of variables
#' @param varGroupsColors  A vector to define the colors of groups of variables
#' @param aTitle A title for the graphic
#' @param mode A mode to define how many values ​​the data will break down, "bi" (NA = 1 / no_NA = 0), "tri" (NA = -1 / 0 = 0/1 = Other) or "all" (the data originals)
#'
#' @return A heatmap object created with Heatmap
#' @export
#'
#' @examples
#'

showNAs <- function (x, rowGroups, rowLabels, rowGroupsColors,
                     varNames, varGroups, varLabels, varGroupsColors,
                     aTitle, mode=c("bi", "tri", "all")){

require(pheatmap)

  # This function transforms a data.frame or tibble into a data frame
  # whose content is 1 if the value is not an NA or 0 in case it is.
  # Additionally the rows are sorted according the order defined by "rowGroups" levels, set by "varLabels"
  # Also columns are sorted by the order defined by  by "varGroups" levels, set by "varLabels"

# Step 1: Prepare and transform
   dat <- x %>%
    mutate(Group=factor(rowGroups, levels=rowLabels)) %>% # Sort rows by Group value
    arrange (Group) # %>%
  # column_to_rownames(var="id_estudio")
   mat <- dat [, varNames] %>%
     mutate(across(everything(), ~ifelse(is.na(.), 1, 0))) # Transform data into a 1/0 matrix

# Step 2: Prepare a dataset with two columns,
    # - First one for variables group names
    # - Second one for variable names
    # Could be done more easily

    vars_annot <- list();
    i<- 0
    for(l in 1:length(varLabels)){
        vars_annot[[l]] <- varNames[(i+1):(i+varGroups[l])]
        i<- i+varGroups[l]
    }
    names(vars_annot) <- varLabels
    vars_annot1 <- lapply(vars_annot, as.data.frame, stringsAsFactors=FALSE)
    vars_annot_df <- bind_rows(vars_annot1, .id="Dataset")
    colnames(vars_annot_df)[2] <- "Var"
    rownames(vars_annot_df) <- vars_annot_df$Var
# Step 3
    # Prepare information on rows and cols for heatmap
    annots4Rows <- data.frame(Groups = dat[,"Group"])
    rownames(annots4Rows) <- rownames(dat)
    annots4Cols <- data.frame(Vars=vars_annot_df$Dataset)
    rownames(annots4Cols) <- rownames(vars_annot_df)
    annotsColors <- list (Groups=rowGroupsColors, Vars=varGroupsColors)
# Step 4
    # Depict the Heatmap
    p <- pheatmap(mat,
                  scale="none", cluster_rows=FALSE, cluster_cols=FALSE,
                  annotation_col=annots4Cols,
                  annotation_row=annots4Rows,
                  annotation_colors=annotsColors,
                  fontsize_row=3, fontsize_col=5,
                  main= aTitle, cex=1)
  return(p)
}
