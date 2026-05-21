# The global recurrence and variability of kinship terminology structure

[![DOI](https://zenodo.org/badge/530092055.svg)](https://zenodo.org/badge/latestdoi/530092055)

This is the code repository for Passmore (In Prep) "The global recurrence and variability of kinship terminology structure". The manuscript is publicly available at XXXX. 

The analysis and figures can be run from the Makefile, found in the home directory. The Makefile should be run in the following steps (available via `make help`):

1. `make data` initialises the Kinbank git submodule
2. `make subset` subsets the data for each generational domain (siblings, g0, g1, g2, niblings)
3. `make matrix` creates structural vectors and distance matrices for each generational subset
4. `make cluster` applies HDBSCAN clustering to the structural matrices and projects them into two dimensions using UMAP

At this point, manually review the clustering output to determine the qualitative meaning of each cluster, and prepare the data for post-hoc analysis. To view the summarised output download the file `Supplementary Tables.xlsx` in the home of this repository.

5. `make global` calculates silhouette scores and global frequency distributions for each cluster
6. `make trees` fits decision trees to characterise the key distinctions within each cluster
7. `make tables_figures` generates all paper tables and figures
8. `make supp_tables` generates the supplementary tables
9. `make network_model` fits the Bayesian network model

Running the entire analysis should take less than 1 hour.

**For assitance please contact Sam Passmore (samuel.passmore [at] anu.edu.au)**

## Data 
Data for this analysis is drawn from Kinbank (Passmore et al. 2023). 

Kinbank data is available in an interactive format at www.kinbank.org. It has also been archived on github at www.github.com/kinbank and is versioned on Zenodo at https://zenodo.org/account/settings/github/repository/kinbank/kinbank.
Kinbank is the amalgamation of four different projects, which you can see seperately at the github address. Please cite the most appropriate dataset for your project. 

