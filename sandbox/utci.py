# conda activate wk_ecmwf
# cd "C:\Users\fan\pyfan\vig\getdata\envir\ecmwf_scripts\"
# python ecmef_pressure_utci_historical.py

import os
import cdsapi
import urllib.request
import pandas as pd
import numpy as np
from datetime import datetime, timedelta
# conda install -c conda-forge netCDF4
from netCDF4 import Dataset, date2num, num2date
from zipfile import ZipFile

#######################################################################################################
# A. Folders
spt_root = 'C:/Users/Kaifs/Downloads/_data/'
#snm_data = 'china_total_utci'
#st_era5_prefix = "ECMWF_utci_"

ar_days = ['01', '02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31']
# # Whole of China File Very Large
# ls_coordiantes_area = '53.31, 73, 4.15, 135'
# This is Guangdong only?
# Area
# area below corresponds to North, West, South, East.
# # China
# fl_area_north <- 53.31
# fl_area_west <- 73
# fl_area_south <- 4.15
# fl_area_east <- 135

# Zhejiang
# fl_area_north <- 31
# fl_area_west <- 118
# fl_area_south <- 27
# fl_area_east <- 123
ar_coordiantes_area = [53.31, 73, 4.15, 135]

# Years list
# ar_years = 2001:2019
#ar_years = ['2010','2020']
# ar_years = '2010'
# ar_months_g1 = ['01','02','03','04','05','06']
# ar_months_g1 = ['01', '03']
#ar_months_g2 = ['01','02','03','04','05','06','07','08','09','10','11','12']
# ar_months_g2 = ['07','08','09']
#########################################################################################################


#################################################
# ------------ Process
#################################################

#for it_yr in ar_years:
#    for it_mth_group in [1, 2]:
#        if it_mth_group == 1:
#            ar_months = ar_months_g1
#        if it_mth_group == 2:
#            ar_months = ar_months_g2

        # A. Process Each
        #snm_data_nc_hourly = spt_root + snm_data + '.zip'
        # CSV file
        #st_csv_date_start = str(it_yr) + str(min(ar_months)) + str(min(ar_days))
        #st_csv_date_end = str(it_yr) + str(max(ar_months)) + str(max(ar_days))
        #st_csv_file_name = st_era5_prefix + st_csv_date_start + '_to_' + st_csv_date_end + '.csv'
        

# B. API Request
c = cdsapi.Client()
res = c.retrieve(
            'derived-utci-historical',
            {
                'product_type': 'consolidated_dataset',
                'variable': 'universal_thermal_climate_index',
                'version': '1_1',
                #'year': [it_yr],
                'year': '2020',
                #'month': ar_months,
                'month': '12',
                'day': ar_days,
                'time':'19:00',
                'area': ar_coordiantes_area,
                'grid': [0.25, 0.25],
                'format': 'zip',
            },
            'C:/Users/Kaifs/Downloads/_data/test2020_12_utci.zip')


        # C. show Progress
print('print results')
print(res)
print(type(res))
###############################################################################

