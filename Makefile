
# Pass FIGURES=--figures to make cluster to save decision tree PDFs and UMAP scatter plots.
# e.g. make cluster FIGURES=--figures
FIGURES ?=

.PHONY: all install help clean data subset matrix cluster global trees tables_figures supp_tables network_model

all: install data subset matrix cluster global trees tables_figures supp_tables network_model

# Install R and Python dependencies
install:
	Rscript -e "pkgs <- readLines('r_requirements.txt'); pkgs <- pkgs[nzchar(pkgs)]; install.packages(pkgs, repos = 'https://cran.r-project.org')"
	python3 -m venv myvenv
	myvenv/bin/pip3 install -r requirements.txt

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
	mkdir -p results/umap
	mkdir -p results/tsne
	mkdir -p results/hdbscan
	myvenv/bin/python3 analysis/hdbscan-cluster.py $(FIGURES)
	
# Create global level frequency graphs 
global:
	@echo "Create global frequency graphs"
	mkdir -p results/global/data
	RScript analysis/global-frequency.R siblings
	RScript analysis/global-frequency.R niblings
	RScript analysis/global-frequency.R g0
	RScript analysis/global-frequency.R g1
	RScript analysis/global-frequency.R g2

tables_figures:
	RScript analysis/cluster_tolanguages.R # Build summary file
	RScript analysis/modal_types.R # Identify the most common structure within each cluster
	RScript analysis/table_1.R
	RScript analysis/table_2.R
	RScript analysis/plot-umap.R siblings # part of Figure 1 
	RScript analysis/figure_1.R
	RScript analysis/plot-umap.R g0 # Figure 2
	RScript analysis/plot-umap.R g1 # Figure 3
	RScript analysis/plot-umap.R g2 # Figure 4
	RScript analysis/plot-umap.R niblings # Figure 5

supp_tables: 
	mkdir -p results/supp_tables
	RScript processing/supp_tables.R
