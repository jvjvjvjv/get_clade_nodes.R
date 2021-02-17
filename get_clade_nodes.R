library(ggtree)
library(ape)
library(ggplot2)

tree <- read.tree("~/Cicadoidea/ERIC_R_TREE/example.tre")
metadata <- read.csv("~/Cicadoidea/ERIC_R_TREE/Cicadaclass.csv")


## Input "phylo" tree object
## Input metadata in a dataframe where first column is tip names and second column is clade names
get_clade_nodes <- function(phy_tree, metadata_df) {
  groups <- levels(as.factor(metadata_df[[2]]))
  
  out <- sapply(groups, function(x){
    names <- as.character(metadata_df[[1]][which(x == metadata_df[2])])
    if (length(names) == 1){
      return(which(names == phy_tree$tip.label))
    } else {
      return(getMRCA(phy_tree, names))
    }
  })
  names(out) <- groups
  return(out)
}

get_clade_nodes(tree, metadata)
z <- get_clade_nodes(tree, metadata)

g <- ggtree(tree)
g + 
  geom_tiplab(size=2) + 
  geom_hilight(node = z[3], extendto = 7) + 
  geom_hilight(node = 69, extendto = 7, aes(fill = "green"), alpha = 0.1) +
  geom_hilight(node = z['Cicadinae'], extendto = 7, aes(fill = "lightsalmon3"), alpha = 0.1) +
  geom_cladelabel(node = z[1], label = 'italic(\'test-test-"test"\')~plain(\'"testy"\')', parse = TRUE, align = T, offset = -0.5)
