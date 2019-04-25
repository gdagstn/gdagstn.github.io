#' Color key
#' @param values vector of numeric values to which colors will be mapped
#' @param pal color palette 
#' @return a vector of colors, mapped to the values and in the same order as the values vector


colorKey <- function(values, pal = viridis(25, option = "B"))
{
    require(pheatmap)
    values_sc <- scale(values)
    bks <- pheatmap:::generate_breaks(values_sc, length(pal), center = F)
    cols <- pheatmap:::scale_colours(values_sc, col=pal, breaks=bks, na_col = "gray")
    cols <- as.character(cols)
    return(cols)
} 

# STILL NEEDS SPEEDUP

#' Text-based heatmap
#' @param dat matrix with numeric values
#' @param cols color palette 
#' @param cluster_cols logical, should columns be clustered by hierarchical clustering? default is TRUE
#' @param cluster_rows logical, should rows be clustered by hierarchical clustering? default is TRUE
#' @param main character, title of the heatmap
#' @param show_col_legend logical, should the column legend be shown at the end of the plot?
#' @return a text-based heatmap directly in the terminal output. Useful when your X11 forwarding is broken or for quick exploratory analysis of small datasets.

textHeatMap <-function(dat, cols = colorspace::sequential_hcl(25, "Viridis"),cluster_cols = T, cluster_rows = T, main = "Heatmap", show_col_legend = T)
{

require(crayon)

#Clustering
if(cluster_rows == T)
{ 
	hr = hclust(                #Hierarchical clustering of the rows
        dist(dat), 
        method="complete"
                )
}
else
{
	hr = list("order" = 1:nrow(dat))
}

if(cluster_cols == T)

{
 hc = hclust(                #Hierarchical clustering of columns (same as rows but transposing the matrix so that rows and columns are exchanged)
        dist(t(dat)), 
        method="complete"
                )
}
else
{
	hc = list("order" = 1:ncol(dat))
}
dat = dat[hr$order, hc$order]


#Colour scale
colormap <-  colorKey(as.vector(dat), pal = cols)
crayonmap <- apply(t(apply(t(col2rgb(colormap)), 1, function(x) x/255)), 1, function(y) crayon::make_style(rgb(y[1],y[2],y[3]), bg = T))

colormat = matrix(1:(nrow(dat)*ncol(dat)), nrow = nrow(dat), byrow = F)

#Plot the heatmap
cat("\n \n")
cat("   ")
cat(paste(bold(main), "\n"))

cat("\n \n")
cat("   ")
for(k in 1:ncol(dat))
{
	if(nchar(k) == 1) cat(paste(" ",k," ", sep = ""))
		else if(nchar(k) == 2) cat(paste(k, " ",sep = ""))
}
cat("\n \n")
for(i in 1:nrow(colormat))
{
	cat("   ")
	for(j in 1:ncol(colormat))
	{	
		cat(crayonmap[[colormat[i,j]]]("   "), sep = "")
	}
	cat("  ")
	cat(rownames(dat)[i])
	cat("\n")
}

cat("\n \n")
cat("   ")

#Color Key
cat(paste("Key: ", round(min(dat)), " ", sep = ""))
for(i in 1:length(cols))
	{
		crayonmap <- apply(t(col2rgb(cols)), 1, function(x) crayon::make_style(rgb(x[1]/255,x[2]/255,x[3]/255), bg = T))
		cat(crayonmap[[i]]("   "))
	}
	cat(paste(" ", round(max(dat)),  sep = ""))
	cat("\n \n")
if(show_col_legend == T)
#Column legend
	{
		cat("   ")
		cat("Column legend:\n")
	for(i in 1:ncol(dat)) cat(paste("   ", i, ":", colnames(dat)[i], "\n", sep = ""))
	}
}


#logo <- read.bitmap("logo2.png")

setClass("crayonmap", representation(crayons = "character", row.num = "numeric", col.num = "numeric"))


#' Create crayonmap object
#' @param file BMP, TIFF, JPG or PNG bitmap file
#' @return a "crayonmap" class object to be used by paintCrayon()

readCrayonMap <- function(file)
{
	require(crayon)
	require(readbitmap)
	pic <- read.bitmap(file)
	crayonstrings <- list()
	crayonstrings <- t(sapply(1:nrow(pic), 
		function(i) {
			sapply(1:ncol(pic), 
				function(j){
					crayon::make_style(rgb(pic[i,j,1],pic[i,j,2],pic[i,j,3]), bg = T)("  ")
							})
					}))
	
	crayonstrings[,ncol(crayonstrings)] <- paste(crayonstrings[,ncol(crayonstrings)], "\n", sep = "")
	crayonstrings[,1] <- paste("   ", crayonstrings[,1], sep = "")
	crayons <- as.vector(t(crayonstrings))
	cmap <- new("crayonmap", crayons = crayons, row.num = nrow(pic), col.num = ncol(pic))
return(cmap)
}


#' Text-based bitmap
#' @param cmap object of class "crayonmap"
#' @return a text-based bitmap directly in the terminal output. 

paintCrayon <- function(cmap)
{	
	cat("\n \n \n")
	cat(cmap@crayons, sep = "")
	cat("\n \n \n")
}



