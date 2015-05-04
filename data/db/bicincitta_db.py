# we will use the bicincitta data saved as csvs to populate the databse publibike_db

import psycopg2
import pandas as pd
import os.path

connection = psycopg2.connect("dbname=publibike_db user=publibike_user")
cursor = conneciton.cursor()

#where is the data
datapath = "~/work/citiviz/bikeSharing/bikestats/data"

bikedatapath = os.path.join(datapath, "bicincitta")
datestamp = "20150430"
#insert stations

stnfile = os.path.join(bikedatapath, 
                        "stations_" + datestamp + ".csv")
stations = pd.read_csv( stnfile)

