---codes:   download_utci_loop.py;
---aims:    to download utci data from the ERA5 by year;
         modify year manually ;
---outputs: hourly utci for a geosquare that includes the whole China region, defined by farest points in the North, South, East, West;
         for each year it produces 12 folders that includes day files;


---codes:   utci_convert_loop_*year*;
---aims:    to convert datafile downloaded from ERA5 to csv file;
---outputs: a spreadsheet contains utci values by coordinates, defined as Long and Lat;
         the columns are in date-hour format, e.g. 01-01-00:00, 01-01-01：00...12-31-23:00;
