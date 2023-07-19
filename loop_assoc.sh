#!/bin/bash


for i in `ls -d /BiO/enju07/0_md/3_PGS/root_pheno/phecode_list/icd10_code/icd_case/*`
do
        Rscript dis2.R $i SAVE
done
