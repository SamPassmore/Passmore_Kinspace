KINBANK_REPO=https://github.com/kinbank/kinbank
GLOTTOLOG_REPO=https://github.com/glottolog/glottolog-cldf.git
DPLACE_REPO=https://github.com/D-PLACE/dplace-data.git

KINBANK=raw/kinbank
GLOTTOLOG=raw/glottolog
DPLACE=raw/dplace

help:
	@echo 1. make data
	@echo 2. make subset
	@echo 3. make matrix
	@echo 4. make cluster
	@echo Then manually determine clusters
	@echo 5. make umap will plot the results colored by cluster and EA categorisation
	@echo 6. make global
	@echo 7. make bayestraits
	@echo Then run models on bluecrystal
	

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
	
# manually determine clusters & their DAGs
# DAGs are directed by the addition of a term (although direction is not indiciative of change likelihood)

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
	mkdir -p results/hdbscan/silhouette/
	mkdir -p results/global/mantel
	RScript analysis/silhouette_scores.R
	RScript analysis/global-frequency.R siblings
	RScript analysis/global-frequency.R niblings
	RScript analysis/global-frequency.R g0
	RScript analysis/global-frequency.R g1
	RScript analysis/global-frequency.R g2
	

# Create Bayestraits data
bayestraits: $(DPLACE)
	@echo "Create data for BayesTraits"
		cd $(GLOTTOLOG) && git pull
		mkdir -p data/bayestraits
		mkdir -p data/bayestraits/modelstrings
		mkdir -p data/bayestraits/jobs
		mkdir -p results/bayestraits/1
		mkdir -p results/bayestraits/2
		mkdir -p results/bayestraits/3
		mkdir -p results/bayestraits/logs
		RScript processing/get_btdata.R
		RScript processing/make_modelstrings.R
		python processing/make-btscripts.py
		


	
