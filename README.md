# Mohonk Winter Limnology
ß
Use `00_main.R` to call all the necessary scripts needed for data compilation. 
Actual analysis starts at `analysis/02_IceTrends.R`

<pre>
├── README.md
├── analysis
│   ├── 00_main.R # data wrangling script
│   └── 01_isotherm.R # calculates all the isotherm metrics
│   ├── 02_IceTrends.R # analysis file for ice phenology trends and GAMs (Figure 1, 3, 4)
│   └── 03_VariabilityRollingWindow.R # original script used for variability analysis
│   ├── 04_VariabilityRollingWindow.R # updated script for variability analysis (Figure 2)
│   └── 05_SEM-analysis.R # structural equation modeling (Figuer 5)
├── data
├── figures
├── mohonkWinterLimno.Rproj
└── script
│    ├── 01_functions.R
│    ├── 01_munging.R
│    ├── 01_QAQC.R
