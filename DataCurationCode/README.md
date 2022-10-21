# Data Curation Code

This is where you store the code that converts your *RawData* to your *DryadData*, or the data that you will archive with your paper submission (regardless of where they are actually publically archived). 

The main steps here are: 

1. Combine the tables and fields you need
2. Filter the data to only the individuals, dates, etc. used in analysis
3. Anonymize the IDs in your data

It's best to provide all Dryad / Supplemental data in basic csv files where possible. If for some reason that isn't possible, make sure the data can be opened with a program that is free and readily accessible.  
The code here should produce data that are **minimum**, **complete**, and **reproducible**, that is, 

- **minimum** only those data necessary to reproduce your analyses 
- **complete** all of the data necessary to reproduce your analyses
- **reproducible** ready in the format expected by your *AnalysisCode*

These files will typically *not* be made public upon submission/publication. 