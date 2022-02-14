This repository provides a simple interactive Shiny application for exploring 
dynamic primary RNA cleavage models with alternative cleavage sites.

## Requirements
### Active R session

The following packages must be installed in R:

- shiny
- shinydashboard
- tidyverse
- deSolve

```r
install.packages(c('shiny', 'shinydashboard', 'tidyverse', 'deSolve'))
```

### Conda environment
The following YAML can be used to recreate the environment:

```yaml
name: shiny-cleavage-model
channels:
  - conda-forge
dependencies:
  - r-base=4.1
  - r-shiny
  - r-shinydashboard
  - r-tidyverse
  - r-desolve
```

Alternatively, the environment file from the site can be used directly:

```bash
## strongly encourage mamba:
## conda install -n base -c conda-forge mamba
mamba env create \
    -n shiny-cleavage-model \
    -f https://github.com/mfansler/shiny-cleavage-model/raw/main/shiny-cleavage-model.yaml
mamba activate clvmdl
R
```

## Running

From an R session, run

```r
shiny::runGitHub("mfansler/shiny-cleavage-model", ref="main")
```