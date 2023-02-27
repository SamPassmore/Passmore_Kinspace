# The global recurrence and variability of kinship terminology structure

This is the code repository for Passmore (XXXX) "The global recurrence and variability of kinship terminology structure". The manuscript is publicly available at XXXX. 

The analysis and figures can be run from the Makefile, found in the home directory. The Makefile should be run in the following steps (available via `make help`):

1. `make data` will process the kinbank data into the right format
2. `make subset` will subset the data for each generational subset
3. `make matrix` will create structural matrices for each generational subset
4. `make cluster` will use the structural matrices to estimate clusters
5. `make umap` will project the structural matrices into a two dimensional space and create the figures.  

At this point, I manually review the clustering output to determine the the qualitative meaning of each cluster, and prepare the data for post-hoc analysis. 

6. `make global` performs the post-hoc tests of cluster stability 

Running the entire analysis should take less than 1 hour. 

**For assitance please contact Sam Passmore (samuel.passmore [at] anu.edu.au)**

## Data 
Data for this analysis is drawn from Kinbank (Passmore et al. 2023). Kinbank data is available in an interactive format at www.kinbank.org. It has also been archived on github at github.com/kinbank and is versioned on Zenodo at https://zenodo.org/account/settings/github/repository/kinbank/kinbank.
Kinbank is the amalgamation of four different projects, which you can see seperately at the github address. Please cite the most appropriate dataset for your project. 

