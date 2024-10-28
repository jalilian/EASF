# Adaptive spatial sampling

## Concepts
Types of spatial populations
- discrete spatial population
- continuous spatial population

Objectives of spatial sampling
- estimation of parameters or characteristics of the spatial population
- spatial prediction (mapping) of unobserved unites of the spatial population

Types of statistical inferences for spatial populations
- model-based inferences: consider statistical models to describe the underlying processes or relationships within the spatial population
- non-model-based inference: rely on the probability distribution of the sampling process and resampling techniques to generalize the sample results to the population

Types of spatial sampling designs
- random vs non-random spatial sampling designs
  + random sampling designs: selection of population unites is based on a specific well-defined probability distribution
    - simple (completely) random sampling: uniform spatial distribution, ensuring each population unit has an equal chance of being selected
    - inhibitory sampling: spatial distributions with zero probability for selection of close population units
    - stratified sampling: partitioning the population units into strata, and specifying a probability distribution for each stratum 
    - cluster sampling: dividing the population units into clusters, and specifying a probability distribution on these clusters
    - systematic sampling: a regular spatial pattern with a random start
  + non-random sampling designs: selection of population unites does not rely on a probability distribution 
    - preferential sampling: selection of population units is based on values of the study varible, previous information, convenience, or ease of access 
    - target specific sampling: selection of population units is based on a particular criterion, such as spatial coverage 
    - arbitrary sampling: selection of population units is based on subjective judgment
- adaptive and non-adaptive spatial sampling designs
  + adaptive sampling designs: use information from previously selected population units to improve the selection of additional units in each sampling round
    - sequential adaptive sampling: selection of population units is done one at a time, and the decision on the selection of the next unit depends on the results of the previous observations
    - batch adaptive sampling: selection of the population units are done in predefined groups or batches, and selection of the next batch depends on the results of the previous batches
    
  + non-adaptive: selection of population units remains unchanged throughout the sampling process

|     | approach: model-based statistical inference | approach: model-free statistical inference |
| --- | --------------------------------- | -------------------------------- |
objective: parameter estimation        | random sampling designs are more efficient| random sampling designs are more efficient |
objective: spatial predicton (mapping) | non-random sampling designs are more efficient | none are efficient |

## Resources
Books
- Brus, D. J. (2022). [Spatial sampling with R](https://dickbrus.github.io/SpatialSamplingwithR/). CRC Press.
- Diggle, P. J., & Ribeiro, P. J. (2007). Model-based Geostatistics. Springer, [Chapter 8:  Geostatistical design](https://doi.org/10.1007/978-0-387-48536-2_8).
- Gruijter, J. J., Bierkens, M. F., Brus, D. J., & Knotters, M. (2006). [Sampling for natural resource monitoring](https://doi.org/10.1007/3-540-33161-1). Springer-Verlag Berlin Heidelberg.
- Müller, W. G. (2007). [Collecting spatial data: optimum design of experiments for random fields](https://doi.org/10.1007/978-3-540-31175-1). Springer Science & Business Media.
- Mateu, J., & Müller, W. G. (Eds.). (2012). [Spatio-temporal design: Advances in efficient data acquisition](https://www.wiley.com/en-gb/Spatio+temporal+Design%3A+Advances+in+Efficient+Data+Acquisition-p-9781118441886). John Wiley & Sons.
- Diggle, P. J., & Giorgi, E. (2019). [Model-based geostatistics for global public health: methods and applications](https://doi.org/10.1201/9781315188492 ). CRC Press. Chapter 6: Geostatistical design and Chapter 7: Preferential sampling.  
- Khormi, H. M., & Kumar, L. (2015). [Modelling interactions between vector-borne diseases and environment using GIS](https://www.routledge.com/Modelling-Interactions-Between-Vector-Borne-Diseases-and-Environment-Using/Khormi-Kumar/p/book/9781138597235). CRC press.
- Silver, J. B. (2007). [Mosquito ecology: field sampling methods](https://doi.org/10.1007/978-1-4020-6666-5). springer science & business media.
- Le, N. D., & Zidek, J. V. (2006). [Statistical analysis of environmental space-time processes](https://doi.org/10.1007/0-387-35429-8). Springer.

Papers
- Monteiro, G. M., Djogbenou, L. S., Donnelly, M. J., & Sedda, L. (2023). Development and deployment of improved Anopheles gambiae sl field surveillance by adaptive spatial sampling design. [bioRxiv, 2023-06](https://doi.org/10.1101/2023.06.16.545360).
