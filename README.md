
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Inferring Arithmetic Skill from Speed and Accuracy.

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

People routinely infer others’ competence under uncertainty, often
relying on cues such as task difficulty and past accuracy. An emerging
body of research suggests that people approximate Bayesian inference
when doing so. We extend these results by testing whether people can
infer others’ numerical ability in a way that is consistent with a
rational Bayesian model. In Study 1, we find that participants
accurately predict the arithmetic performance of another individual from
information about their past performance. Computational modeling shows
that participants’ inferences are better described by Bayesian processes
than by plausible heuristics. Study 2 introduces a modified paradigm, in
which participants are told about both past performance and time taken
to solve problems. We find that, although participants are quite
accurate in their predictions, they do not seem to take into account
information about speed.

------------------------------------------------------------------------

This repository contains the code and data for our paper.

> Mercier, M., de Lanerolle, R., Morin, O., Quillien, T., Mercier, H..
> (2026). “Inferring Arithmetic Skill from Speed and Accuracy.”

## Replicate

This github repository aims to be computationaly reproducible (in one
click). All inline reported results are accessible in the knitted Rmd
document.

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

    #> R version 4.5.2 (2025-10-31)
    #> Platform: aarch64-apple-darwin25.0.0
    #> Running under: macOS Tahoe 26.3.1
    #> 
    #> Matrix products: default
    #> BLAS:   /opt/homebrew/Cellar/openblas/0.3.30/lib/libopenblasp-r0.3.30.dylib 
    #> LAPACK: /opt/homebrew/Cellar/r/4.5.2_1/lib/R/lib/libRlapack.dylib;  LAPACK version 3.12.1
    #> 
    #> locale:
    #> [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8
    #> 
    #> time zone: Europe/Paris
    #> tzcode source: internal
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] compiler_4.5.2    here_1.0.2        fastmap_1.2.0     rprojroot_2.1.1  
    #>  [5] cli_3.6.5         tools_4.5.2       htmltools_0.5.9   rstudioapi_0.17.1
    #>  [9] yaml_2.3.11       rmarkdown_2.30    knitr_1.50        xfun_0.54        
    #> [13] digest_0.6.39     rlang_1.1.6       evaluate_1.0.5
