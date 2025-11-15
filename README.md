
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Result reports of ‘Inferring Competence on a Numerical Task’

------------------------------------------------------------------------

OSF blinded for peer-review:
<https://osf.io/n9erf/overview?view_only=12991dcd2fba4fe2b9a1a77339d6cb2f>

**All this project’s materials are free and open**.

- [Replicate the findings](#replicate)
- [Check out the files glossary for this repository](#glossary)

![Open data](images/data_large_color.png)   ![Open
materials](images/materials_large_color.png)  
![Preregistration](images/preregistered_large_color.png)

------------------------------------------------------------------------

## Abstract

------------------------------------------------------------------------

This repository contains the code and data for our paper.

> ANON. 2025. “Result reports of ‘Inferring Competence on a Numerical
> Task’”

## Replicate

This github repository aims to be computationaly reproducible (in one
click). The manuscript was written using in RMarkdown: all results are
therefore programmatically included when rendering (knitting) the
document which allows us to link reported results with the code and
data.

To reproduce the findings and re-run the analysis, do the following:

1.  Download this repository. You can use GitHub to clone or fork the
    repository (see the green “Clone or download” button at the top of
    the GitHub page).
2.  Open the `manuscript.Rmd` file and run it

Please find the [session info below](#session-info).

## Files glossary

- The `manuscript.Rmd` and the `preprint.Rmd` generate the paper.
  Manuscript is anonymized for submission.
- The `study_[1-X]/` folders contain the data sets and pre-processing
  scripts for each studies.
- `study_[1-X]/data/raw/` include the raw data.
- `study_[1-X]/data/clean/` include preprocessed data from the
  `S[1-3]_preprocess.R` script.
- `study_[1-X]/results/` include fits results (e.g., from
  `S1_nestedness.R`)
- `references.bib` contains all references and `csl/apa.csl` the
  citation formatting.
- `images/` contains images used for this README file.

## Session info

    #> R version 4.4.2 (2024-10-31 ucrt)
    #> Platform: x86_64-w64-mingw32/x64
    #> Running under: Windows 11 x64 (build 26200)
    #> 
    #> Matrix products: default
    #> 
    #> 
    #> locale:
    #> [1] LC_COLLATE=French_France.utf8  LC_CTYPE=French_France.utf8   
    #> [3] LC_MONETARY=French_France.utf8 LC_NUMERIC=C                  
    #> [5] LC_TIME=French_France.utf8    
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.4.2    here_1.0.1        fastmap_1.2.0     rprojroot_2.0.4  
    #>  [5] cli_3.6.3         tools_4.4.2       htmltools_0.5.8.1 rstudioapi_0.17.1
    #>  [9] yaml_2.3.10       rmarkdown_2.29    knitr_1.49        xfun_0.51        
    #> [13] digest_0.6.37     rlang_1.1.4       evaluate_1.0.3
