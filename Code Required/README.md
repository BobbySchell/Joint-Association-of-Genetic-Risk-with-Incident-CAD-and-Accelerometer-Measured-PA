REPLACE: Workflow and analysis code for "Joint Association of Genetic Risk and Accelerometer-Measured Physical Activity with Incident Coronary Artery Disease in the UK Biobank Cohort"
------------

__Authors:__ Robert C. Schell,
William H. Dow,
Lia C.H. Fernald,
Patrick T. Bradshaw,
David H. Rehkopf







---

Repository Contents
------------

This repository contains a detailed overview of the steps required to reproduce the analyses in "Joint Association of Genetic Risk and Accelerometer-Measured Physical Activity with Incident Coronary Artery Disease in the UK Biobank Cohort."

This document gives an overview of all data processing, sanity checks, and analyses performed for this manuscript. I created an accelerometer cohort, a phenotype cohort, and a polygenic risk score and merged them as the primary outputs of the document. The accelerometer cohort and phenotype cohort are distinct because I needed to keep the phenotype cohort intact to allow direct comparisons with the PRS score’s performance in Tamlander et al., 2022 before merging them.

Dataset Availability
-----------

You must apply for the UK Biobank data through the Access Management System. This analysis was run using the UK Biobank's cloud-based Research Analysis Platform.

---

Documents in this repository and their role in the study replication
------------

*Code for Data Processing:*
>1. Creating Accelerometer Cohort.ipynb
>2. Genomic Processing and Creation of Polygenic Risk Score.ipynb
>3. Creating New Pheno Dataset and Merging with Accelerometer Cohort.ipynb
>4. Creating the Physical Activity Exposure Variables.ipynb
>5. STEPS FOR MERGING GENETIC AND PHENOTYPIC DATA (and docs)

*Code for Results:*
>6. SANITY CHECKS PERFORMED AND RATIONALE DOCUMENT (and docs)
>7. ANALYSIS CODE FOR RUNNING THE MODELS AND PRODUCING FIGURES AND TABLES (and docs)

*SHOULD ALSO REFERENCE USEFUL RESOURCES USED TO HELP CREATE THIS
