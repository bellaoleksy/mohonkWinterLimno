#Extract winter and summer data for DCR's students poster for NEGLEON
#Get July 20 and Jan 20 from 1985 to 2019

#dayofyear = 20 and 201
write_csv(DailyInterpol%>%filter(dayofyear==20),file="output/MohonkDataFromJan20-1985-2019.csv")
write_csv(DailyInterpol%>%filter(dayofyear==201),file="output/MohonkDataFromJul20-1985-2019.csv")
