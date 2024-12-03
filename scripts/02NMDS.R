#-------- SCRIPT FOR NMDS AND TAXA/ENV-FIT ------------#############

## LOADING PACKAGES #########
library(ggalt)
library(ggplot2)
library(ggrepel)
library(reshape2)


## LOADING DATA ########

source("LOADING_DATA_new.R")


# create a list for looping later

data_list <- list(
  dataset1 = list(abundance_data = abun_dat1, metadata = meta_dat1),
  dataset2 = list(abundance_data = abun_dat2, metadata = meta_dat2),
  dataset3 = list(abundance_data = abun_dat3, metadata = meta_dat3),
  dataset4 = list(abundance_data = abun_dat4, metadata = meta_dat4))
  


#---------------- FUNCTIONS --------------------#######
# Functions for NMDS and the environmental fit and taxonomic fit analysis 

nmds_analysis <- function(abundance_data){
  set.seed(183672807)
  NMDS <- metaMDS(abundance_data, dist="gower", k=6, maxit=1000, trymax=100, autotransform = FALSE)
  return(NMDS)
}

# Environmental fit 

env_fit_analysis <- function(metadata){
  # Fit environmental vectors and factors onto the ordination
  env <- envfit(NMDS, metadata, permutations = 999, na.rm =TRUE)
  return(env)
}

# Taxonomic fit 

taxa_fit_analysis <- function(abundance_data){
  # Fit taxa onto the ordination
  taxa.fit <- envfit(NMDS, abundance_data, permutations = 999, na.rm =TRUE) 
  return(taxa.fit)
}


#---------------- PLOTTING --------------------#######

plot_nmds <- function(NMDS, metadata, dataset_name){
  # Extract eigenvectors and stress value from the Ordination
  Eigenvectors <- data.frame(scores(NMDS, choices=1:2, display="sites"))
  Stress <- NMDS$stress
  
  # Merge eigenvectors and metadata, by=0 mean by rownames
  plot.coords <- merge(Eigenvectors, metadata, by=0)
  
  # Basic plot
  g <- ggplot(data=plot.coords, aes(x=NMDS1, y=NMDS2)) +
    # Add point
    geom_point(aes(color=season), size = 2) +
    # Change legend title by color
    labs(color="season") +
    # Change the colors and the order of the WWTP compartments
    scale_color_manual(values = c( '#D1E5F0','#B2182B', '#F4A582', '#2166AC'), 
                       breaks = c("spring", "summer", "fall", "winter")) +
    # Add stress value
    annotate("text", x=Inf,y=-Inf,hjust=1.1,vjust=-0.5,
             label=paste("Stress value:", round(Stress,3))) +
    # Adjust theme 
    geom_hline(yintercept = 0, colour="black", linetype="dotted") +
    geom_vline(xintercept = 0, colour="black", linetype="dotted") +
    theme_linedraw()+
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    coord_cartesian(xlim = c(-0.23, 0.2), 
                    ylim = c(-0.2, 0.225)) +
    theme(legend.position = "right")
  
  # save the plot 
  ggsave(paste0(dataset_name, "_nmds.pdf"))
  
  return(g)
}


# adding envfit to the plot 

plot_env_fit <- function(g, env_result, dataset_name){
  environmental_vectors <- data.frame(scores(env_result, "vectors")) * ordiArrowMul(env_result) * 0.2
  
  # Set labels
  environmental_vectors$Labels <- rownames(environmental_vectors)
  
  # Assign p-values
  environmental_vectors$p.values <- env_result$vectors$pvals
  
  # Keep only environmental vectors with p value < 0.05
  environmental_vectors <- environmental_vectors[environmental_vectors$p.values < 0.05,]
  
  #--------------------------- Environmental factors
  # Extract centroids for categorical metadata, fit the to the graph
  environmental_factors <- data.frame(scores(env_result, "factors")) #* ordiArrowMul(env_result)
  
  # Set labels
  environmental_factors$Labels <- rownames(environmental_factors)
  
  # Remove all centroids for the one-hot encoded factors
  environmental_factors <- environmental_factors[!grepl("0$|1$", environmental_factors$Labels),]
  
  # We extract the p values for the environmental factors and set lables
  pvalues <-data.frame(p.values =  env_result$factors$pvals)
  pvalues$Labels <- rownames(pvalues)
  
  # Keep only environmental vectors with p value < 0.05
  environmental_factors <- 
    environmental_factors[grepl(paste(pvalues$Labels[pvalues$p.values < 0.5 ], collapse = "|"), 
                                environmental_factors$Labels),]
  # Adding calculations to the plot
  g +
    # Add polygons 
    geom_encircle(aes(fill = season), linetype = "blank", 
                  s_shape = 1, expand = 0, alpha = 0.5, show.legend = FALSE) +
    # Change the fill colors, the opacity, and the order of the WWTP compartments
    scale_fill_manual(values = alpha(c('#D1E5F0','#B2182B', '#F4A582', '#2166AC'), 0.2), 
                      breaks = c( "spring", "summer", "fall", "winter")) +
    # Add arrows and text for environmental vectors
    geom_segment(data=environmental_vectors, aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
                 inherit.aes = F, arrow=arrow(length=unit(0.5, 'cm')), color="black", lwd=1) +
    geom_text_repel(data=environmental_vectors, aes(x=NMDS1, y=NMDS2, label=Labels),
                    inherit.aes = F, size=4, parse = T, force = T, nudge_x = 0.01) +
    geom_point(data=environmental_factors, aes(x=NMDS1, y=NMDS2), 
               shape = "diamond", size = 4, alpha = 0.6, colour = "navy") +
    geom_text_repel(data=environmental_factors, aes(x=NMDS1, y=NMDS2, label=Labels),
                    size = 4, box.padding = unit(1, "lines"), point.padding = unit(1, "lines")) 
  
  # safe the plot 
  ggsave(paste0(dataset_name, "_nmds_envfit.pdf"))
}


# adding taxfit to the plot 

plot_tax_fit <- function(g, taxa_result, dataset_name){
  # Extract points for the taxa 
  taxa <- data.frame(scores(taxa_result, "vectors")) * ordiArrowMul(taxa_result) *0.2
  
  # Set labels
  taxa$Labels <- rownames(taxa)
  
  # Assign the r-square value and p values
  taxa$R <- taxa_result$vectors$r
  taxa$p.values <- taxa_result$vectors$pvals
  
  # Keep only taxa with p value < 0.05
  taxa <- taxa[taxa$p.values < 0.05,]
  
  # Order by the R2 value, only keep top 10 predictors
  taxa <- taxa[order(taxa$R, decreasing = T)[1:10],]
  
  #-------------------------------------------- Plot
  
  g +
    # Add polygons 
    geom_encircle(aes(fill = season), linetype = "blank", 
                  s_shape = 1, expand = 0, alpha = 0.5, show.legend = FALSE) +
    # Change the fill colors, the opacity, and the order of the WWTP compartments
    scale_fill_manual(values = alpha(c('#D1E5F0','#B2182B', '#F4A582', '#2166AC'), 0.2), 
                      breaks = c( "spring", "summer", "fall", "winter")) +
    # Add arrows and text for taxa
    geom_segment(data=taxa, aes(x=0, y=0, xend=NMDS1, yend=NMDS2),
                 inherit.aes = F, arrow=arrow(length=unit(0.5, 'cm')), color="black", lwd=1) +
    geom_text(data=taxa, aes(x=NMDS1, y=NMDS2, label=Labels),
              inherit.aes = F, size=4,  nudge_y = 0.009) 
  
  # save the plot 
  ggsave(paste0(dataset_name, "_nmds_taxfit.pdf"))
}


#---------------- USAGE --------------------#######

# First check if there are any missing values in the entire data frame
if (any(is.na(data_list))) {
  print("There are missing values in the data frame.")
} else {
  print("There are no missing values in the data frame.")
}

# we loop over the created list of datasets and use the functions 
for (dataset_name in names(data_list)) {
  dataset <- data_list[[dataset_name]]
  
  abundance_data <- dataset$abundance_data
  metadata <- dataset$metadata
  
  # functions 
  NMDS <- nmds_analysis(abundance_data)
  env_result <- env_fit_analysis(metadata)
  taxa_result <- taxa_fit_analysis(abundance_data)
  
  # plot
   g <- plot_nmds(NMDS, metadata, dataset_name)
   plot_env_fit(g,env_result, dataset_name)
   plot_tax_fit(g,taxa_result, dataset_name)
  
  print(NMDS)
  print(env_result)
  print(taxa_result)
}
 



