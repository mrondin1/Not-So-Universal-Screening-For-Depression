import pandas as pd

#download data from https://data.medicare.gov/data/physician-compare

#set directory path

screeningData = pd.read_csv(dir_path+'/Physician_Compare_2014_Individual_EP_Public_Reporting_-_Clinical_Quality_Of_Care.csv')

individualWithStates = screeningData.join(providerData.set_index(['NPI','PAC ID','Last Name','First Name']),on=['NPI','PAC ID','Last Name','First Name'],how='left') 

individualWithStates = individualWithStates.drop_duplicates(subset=['NPI'])   

eachStateInfoScreening = individualWithStates.groupby('State').mean()   

eachStateInfoScreening.to_csv(dir_path+'/eachStateScreeningRateTESTING.csv') 