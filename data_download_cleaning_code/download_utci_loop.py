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
spt_root = 'C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/utci_china_1982_2020'
#snm_data = 'china_total_utci'
#st_era5_prefix = "ECMWF_utci_"

year = '1990'
ar_days = ['01', '02','03','04','05','06','07','08','09','10','11','12','13','14','15','16','17','18','19','20','21','22','23','24','25','26','27','28','29','30','31']
ar_coordiantes_area = [53.31, 73, 4.15, 135]


for month in range(1, 13):
    # Format month as two-digit string with leading zeros
    formatted_month = f"{month:02d}"
    
    # Data retrieval for the current month
    c = cdsapi.Client()
    res = c.retrieve(
        'derived-utci-historical',
        {
            'product_type': 'consolidated_dataset',
            'variable': 'universal_thermal_climate_index',
            'version': '1_1',
            'year': year,
            'month': formatted_month,
            'day': ar_days,
            'area': ar_coordiantes_area,
            'grid': [0.25, 0.25],
            'format': 'zip',
        },
        f'C:/Users/Kaifs/OneDrive/Documents/dropbox_penn/Dropbox/utci_china_1982_2020/{year}_{formatted_month}_utci.zip'
    )
    
    # Show progress
    print(f'Downloaded data for {year}-{formatted_month}')

# End of loop
print('Data download complete!')




