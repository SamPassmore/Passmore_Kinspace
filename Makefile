
help:
	@echo 1. make data will process the kinbank data into the right format
	@echo 2. make subset will subset the data for each generational subset
	@echo 3. make matrix will create structural matrices for each generational subset
	@echo 4. make cluster will use the structural matrices to estimate clusters
	@echo 5. make umap will project the structural matrices into a two dimensional space. 
	@echo I review the clustering output to determine the structure of the clusters
	@echo 6. make global performs the post-hoc tests of cluster stability 

clean: 
	rm -rf raw

# Clone and Update the data
data:
	git submodule init 

# Create data subsets
subset:
	mkdir -p data/terms
	RScript processing/get_termsubsets.R
	
# Create structural vectors & make matrix
# for each kin subset 
matrix:
	mkdir -p data/matrix
	RScript processing/get_structuralmatrix.R
	
# Cluster languages
cluster:
	mkdir -p results/decision-trees
	mkdir -p results/umap
	mkdir -p results/tsne
	mkdir -p results/hdbscan
	python3 -m venv myvenv
	myvenv/bin/pip3 install -r requirements.txt 
	myvenv/bin/python3 analysis/hdbscan-cluster.py # Cluster languages
	RScript analysis/cluster_tolanguages.R # Build summary file
	RScript analysis/modal_types.R # Identify the most common structure within each cluster
	
# Create kinspace projections
umap: 
	RScript analysis/plot-umap.R siblings
	RScript analysis/plot-umap.R niblings
	RScript analysis/plot-umap.R g0
	RScript analysis/plot-umap.R g1
	RScript analysis/plot-umap.R g2

# Create global level frequency graphs 
global:
	@echo "Create global frequency graphs"
	mkdir -p results/global/data
	mkdir -p results/global/networks
	mkdir -p results/global/networks/vertices
	mkdir -p results/global/silhouette/
	mkdir -p results/global/mantel
	RScript analysis/silhouette_scores.R
	RScript analysis/global-frequency.R siblings
	RScript analysis/global-frequency.R niblings
	RScript analysis/global-frequency.R g0
	RScript analysis/global-frequency.R g1
	RScript analysis/global-frequency.R g2
	
supp_tables: 
	mkdir -p results/supp_tables
	RScript processing/supp_tables.R

network_model: 
	RScript analysis/network_model.R
