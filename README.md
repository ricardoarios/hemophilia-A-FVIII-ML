# Hema-Class – v1.0

### Thanks for your interest in using Hema-Class.

<img align="right" src="images/protein.png">

Hemophilia A is a relatively rare hereditary coagulation disorder, caused by the synthesis of defective Factor VIII protein (FVIII). This condition impairs the coagulation cascade, and if left untreated, causes permanent joint damage and poses a risk of fatal intracranial hemorrhage in case of traumatic events. In its severe form, patients who have access to supportive health care systems can benefit from prophylactic treatment, which consists of regular life-long administrations of recombinant forms of the FVIII protein.


We designed a machine learning framework for hemophilia A classification (Hema-Class), and even though our training data was limited, after careful optimization Hema-Class was able to identify properties related to severe and mild forms of the disease. 

We predicted the severity of all residues not yet reported in the medical literature and confirmed its agreement with clinical data, and with in vitro mutagenesis assays.

Here you will find the datasets and the source code used in the manuscript <a href='https://www.nature.com/articles/s41540-021-00183-9'>“Prediction of hemophilia A severity using a small-input machine learning framework”</a>, by Tiago Lopes, Ricardo Rios, Tatiane Nogueira and Rodrigo Mello.

Please note that we cannot make available the data from other databases; to access the complete mutation datasets, please visit the EAHAD and the CHAMPS websites.

The organization of the material is:

> - **/dataset** - contains the datasets to reproduce our findings and create the figures.
> - **/src** - contains the source code for the machine learning framework and for other analyses.
> - **/results** - you can find the pre-trained classification models in this folder.
> - **/workdir** - please execute the code when you are inside this directory.

To reproduce all experiments using individual ML classification models on the training dataset, please run the source codes:

```Prolog
Rscript src/ml/prediction-visualization.R 
Rscript src/ml/prediction-visualization-aug.R
```

You can also open R and run:

```Prolog
source("src/ml/prediction-visualization.R")
```

```Prolog
source("src/ml/prediction-visualization-aug.R")
```

To reproduce our figures and results, we recommend to run all codes from the workdir directory. For example:

```Prolog
cd workdir/

Rscript ../src/ml/make_heatmap_predictions.R
```

You can also open R, from the workdir directory, and run:

```Prolog
source("../src/ml/make_heatmap_predictions.R")
```

If you find any issues with the code, please contact us: tiago-jose@ncchd.go.jp, ricardoar@ufba.br, tatiane.nogueira@ufba.br, mello@icmc.usp.br

On the behalf of all of the authors, we appreciate your interest in Hema-Class and hope it is useful to your research.
