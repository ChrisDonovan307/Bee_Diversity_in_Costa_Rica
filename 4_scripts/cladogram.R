# Cladogram 


# Housekeeping ------------------------------------------------------------

pacman::p_load(
  ggtree,
  dplyr,
  ape,
  data.tree,
  viridisLite
)

# Pull data with APIMEL and filter to Hymenoptera
full <- readRDS('2_clean/with_apimel/full_api.rds') %>% 
  filter(order == 'Hymenoptera')
get_str(full)



# data.tree ---------------------------------------------------------------
## Wrangling ---------------------------------------------------------------


# Take full observation data, filter to relevant variables and arrange nicely.
# Leave in codsp to link later
dat <- full %>% 
  select(order, family, tribe, genus, species, codsp, abundance) %>% 
  arrange(across(c(order, family, tribe, genus, species), desc))
get_str(dat)

# Build a hierarchical tree using data.tree
dat$pathString <- paste(
  dat$order,
  dat$family,
  dat$tribe,
  dat$genus,
  paste(dat$genus, dat$species),
  sep = '/'
)

species_key <- dat %>% 
  group_by(pathString) %>% 
  summarize(abundance = sum(abundance)) %>% 
  arrange(desc(row_number()))
get_str(species_key)

# Convert to data.tree structure
bee_tree <- as.Node(dat)

# Convert the data.tree structure to Newick format
newick <- ToNewick(bee_tree)

# Read the Newick tree into ape
phylo_tree <- read.tree(text = newick)

# Add a space to end of node labels so it isn't cut off
phylo_tree$node.label <- paste0(phylo_tree$node.label, ' ')

get_str(phylo_tree)
phylo_tree$tip.label <- paste0(phylo_tree$tip.label, '_(', species_key$abundance, ')')

# Set color scheme
colors <- viridis(14)



## Plot --------------------------------------------------------------------


png(
  filename = '7_plots/graphs/phylo.png',
  width = 6.5,
  height = 9,
  units = 'in',
  res = 500
)

plot(
  phylo_tree, 
  type = 'c',
  cex = 0.8,
  edge.width = 1,
  show.tip.label = TRUE,
  label.offset = 1,
  no.margin = TRUE,
  edge.color = 'black',
  x.lim = c(-8, 153),
  tip.color = 'black'
)

nodelabels(
  phylo_tree$node.label,
  cex = 0.8,
  bg = 'white'
)

dev.off()


clear_data()
