# Mohonk Winter Limnology

- For a directory of notes, see our <a href="MohonkIceWeather">google drive folder</a>.
- We've already done a number of preliminary/exploratory analyses that are <a href="https://docs.google.com/document/d/1h14IiBCiFjiq_SBtdyfelAApQIOxmKM-x_6lMpSBn6o/edit?usp=sharing">summarized here</a>.

The navigation of this repo is pretty straightforward and mirrors the MTCC project. Use `00_main.R` to call all the necessary scripts (prefix: `01_`) prior to jumping into any analyses. 

<pre>
├── 00_main.R
├── README.md
├── analysis
│   ├── 02_IceTrends.R
│   └── 03_RandomForest_Christianson.R
├── data
├── figures
├── mohonkWinterLimno.Rproj
└── script
│    ├── 01_functions.R
│    ├── 01_munging.R
│    ├── 01_QAQC.R
</pre>