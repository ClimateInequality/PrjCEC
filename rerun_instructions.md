# Project folder organization

Three data steps before the final output
1. Raw underlying UTCI and Chinese demographic data, in the same format as downloaded, and ID information from shpfile.
2. Input files from ID matching files (across different locations such as geography and counties), core population files (where the structure follows, rows are locations and columns are population in groups) and environmnetal files (in this case, heat files where the structure follows, rows are different locations and columns are dates).
3. Generated data files with statistics including share of time children exposed and so on, used as inputs for visualizations/tabulations.
4. Actual tables and figures from Step 3 that are input into the paper.

This replicable code should allow easy flow from Steps 1 to 3. 

Including additional years of data, no additional changes needed between Steps 2 and 3 beyond basic formatting.

[Identify locations of files for each step, input and output along with code between the two]

- Input for Step 1 is in Kai folder (dropbox)
- Output of Step 1 / Input of Step 2 is in `PrjCEC/data`
- Output of Step 2 / Input of Step 3 is in `PrjCEC/data-res`
- Step 4 results is in `PrjCEC/res`

## Use inputs from folder 3 to make outputs of folder 4 in res folder
### 5 Parts from README in res



### Part 1: res/res_atrisk/

The following codes and alterations result in the replication of our results indicating the percentage and percentage point change of children at risk for varying levels of heat stress between Time 1 and Time 2. These instruction generates output inside the `res/res_atrisk` folder and codes are inside the `R-script/tabfig_2_at_risk` folder. 

#### Note for Fan: Are we assuming that replicators would maintain the naming conventions we have built into where data outputs are being stored? If so some of the setting paths below might be unneccessary.

#### Also for the ls_st_files, how should we approach describing these?

- `R-script/tabfig_2_at_risk/ffs_pirecec_tf_atrisk_csv.R`: Steps 1 and 2. Input data for the below tables and figures
    - Install all packages
    - Set path for spt_path_datares, spt_path_res [, ls_st_files ?]
    - Run all code
    - Results in fig_a_data.csv, tab_a_level_data.csv, tab_b_change_data.csv

- `R-script/tabfig_2_at_risk/ffs_pirecec_tf_atrisk_fig_a.R`: Step 3. Figure 2a, 2b, 2c
    - Install all packages
    - Set WD
    - Set input path for fig_a_data.csv through spt_path_res and spn_path
    - Set output path for ggsave() using variables fig1, fig2, fig3
    - Run all code
    - Results in fig_2_a_atrisk_1990.pdf, fig_2_b_atrisk_2020.pdf, fig_2_c_atrisk_pp.pdf

- `R-script/tabfig_2_at_risk/ffs_pirecec_tf_atrisk_tab_a_level.R`: Step 3. Table B.3
    - Install all packages
    - Set input path for tab_a_level_data.csv through spt_path_res and spn_path
    - Set output path for tab_a_level.tex through spn_tex_out
    - Run all code
    - Results in tab_a_level.tex

- `R-script/tabfig_2_at_risk/ffs_pirecec_tf_atrisk_tab_b_change.R`: Step 3. Table B.4
    - Install all packages 
    - Set input path for tab_b_change_data.csv through spt_path_res and spn_path
    - Set output path for tab_b_change.tex through spn_tex_out
    - Run all code
    - Results in tab_b_change.tex


### Part 2: res/res_decompose/
- `R-script/tabfig_3_decompose/ffs_pirecec_tf_decompose_csv.R`: Steps 1. and 2. Input data for below tables and figures
    - Install all packages
    - Set path for spt_path_datares, spt_path_res [, ls_st_files ?]
    - Run all code
    - Results in tab_b_region_data.csv, tab_a_nationalregional_data.csv, fig_a_data.csv

- `R-script/tabfig_3_decompose/ffs_pirecec_tf_decompose_fig_a.R`: Step 3. Figure 3
    - Install all packages
    - Set WD
    - Set input path for fig_a_data.csv through spt_path_res and spn_path
    - Set output path for ggsave using fig3
    - Run all code
    - Results in fig_3_decompose.pdf

- `R-script/tabfig_3_decompose/ffs_pirecec_tf_decompose_nationalregional_tab_a.R`: Step 3. Table B.5
    - Install all packages
    - Set input path for tab_a_nationalregional_data.csv through spt_path_res and spn_path
    - Set output path for tab_a_nationalregional.tex through spn_tex_out
    - Run all code
    - Results in tab_a_nationalregional.tex

- `R-script/tabfig_3_decompose/ffs_pirecec_tf_decompose_nationalregional_tab_b.R`: Step 3. Table B.6
    - Install all packages
    - Set input path for tab_a_nationalregional_data.csv through spt_path_res and spn_path
    - Set output path for tab_b_regional_centralwestern.tex through spn_tex_out
    - Run all code
    - Results in tab_b_regional_centralwestern.tex

### Part 3: res/res_mean_child/

- `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_csv.R`: Steps 1. and 2. Input data for tables and figures below
    - Install all packages
    - Set path for spt_path_datares, spt_path_res [, ls_st_files ?]
    - Run all code
    - Results in fig_a_data.csv, tab_a_24vsday_data.csv, tab_b_season_data.csv


- `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_24vsday_tab_a.R`: Step 3. Table B.1
    - Install all packages
    - Set input path for tab_a_24vsday_data.csv through spt_path_res and spn_path
    - Set output path for tab_a_24vsday.tex through spn_tex_out
    - Run all code
    - Results in tab_a_24vsday.tex


- `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_fig_useless.R`: Step 3. Unused density curves
    - Install all packages
    - Set input path for fig_a_data.csv through spt_path_res and spn_path
    - Run all code
    - Results in unused variables figure, figure2

- `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_fig.R`: Step 3. Figure 1a and 1b
    - Install all packages
    - Set WD
    - Set input path for tab_a_24vsday_data.csv and tab_b_season_data.csv through variables tab_a_24vsday_data and tab_b_season_data
    - Set output path for ggsave using fig1_b and fig1_a
    - Run all code
    - Results in fig1_b_mean_child_perc.pdf, fig_1_a_mean_child_pp.pdf

#### Seems to be an issue between these two steps. `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_season_tab_b.R` has the WD set to `/Users/mlaghi/Documents/GitHub/PrjCEC/res/res_mean_child/res/res_mean_child/tab_b_season_data.csv` which is incorrect. I am unable to change the WD in `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_season_tab_b.R` but I can change it in `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_fig.R`. Check with Fan, solution might be to change the WD and input path in `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_fig.R`

- `R-script/tabfig_1_mean_child/ffs_pirecec_tf_mean_child_season_tab_b.R`: Table B.2
    - Install all packages
    - Set input path for tab_b_season_data.csv through spt_path_res and spn_path
    - Set output path for tab_b_season.tex through spn_tex_out
    - Run all code
    - Results in tab_b_season.tex


### Part 4: res/res_pop_grid_cnty

# Warning: similar named R files


- `R-script/tabfig_5_loc_pop_grid/ffs_pirecec_tf_loc_grid_by_cnty.R`: Location Unknown


- `R-script/tabfig_5_loc_pop_grid/ffs_pirecec_tf_loc_grid_by_cnty.R` : LaTex for Step 4


- `R-script/tabfig_5_loc_pop_grid/ffs_pirecec_tf_loc_pop_by_cnty.R`: Location unknown


- `R-script/tabfig_5_loc_pop_grid/ffs_pirecec_tf_loc_grid_by_cnty.R` : LaTex for Step 4


### Part 5: res/res_region_prov 

- `R-script/tabfig_4_region_prov/ffs_pirecec_tf_region_prov_csv.R`: Steps 1 and 2
    - Install all packages
    - Set path for spt_path_datares, spt_path_res [, ls_st_files shape/geography?]
    - Run all code
    - Results in fig_a_data.csv, tab_a1_strongheat_data.csv, tab_a2_moderateheat_data.csv

    
- `R-script/tabfig_4_region_prov/ffs_pirecec_tf_region_prov_fig_a.R`: Step 3. Figures 4a, 4b, 4c 
    - Install all packages
    - Set WD
    - Set input path for fig_a_data.csv through spt_path_res and spn_path
    - Set output path for ggsave using fig_pp, fig_1990, fig_2020
    - Run all code
    - Results in fig_4_c_region_pp.pdf, fig_4_a_region_1990.pdf, fig_4_b_region_2020.pdf


- `R-script/tabfig_4_region_prov/ffs_pirecec_tf_region_prov_tab_a1_strongheat.R`: Step 3. Table B.7
    - Install all packages
    - Set input path for tab_a1_strongheat_data.csv through spt_path_res and spn_path
    - Set output path for tab_a1_strongheat.tex through spn_tex_out
    - Run all code
    - Results in tab_a1_strongheat.tex


- `R-script/tabfig_4_region_prov/ffs_pirecec_tf_region_prov_tab_a2_moderateheat.R`: Step 3. Table B.8
    - Install all packages
    - Set input path for tab_a2_moderateheat_data.csv through spt_path_res and spn_path
    - Set output path for tab_a2_moderateheat.tex through spn_tex_out
    - Run all code
    - Results in tab_a2_moderateheat.tex

