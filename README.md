# NikolaosT.bTest
'Landscape resilience analysis'

STA’s Rstudio Model’s Codes
(Please contact author for more information about the access)






library(waveletComp)
library(zoo)

test<-read.table("C:/R/test.txt", header=TRUE)

my.w=analyze.wavelet(test,"Date.TP",loess.span=0,dt=1/12,dj1/250,lowerPeriod=0.250,upperPeriod=64,make.pval=T,n.sim=10)
wt.image(my.w,color.key="quantile",n.levels=250,legend.params=list(lab="Wavelet Power Spectrum",mar=4.7,label.digits=2,n.ticks=10),show.date=T,date.format="%Y-%m-%d",timelab="year",periodlab="period")
my.wx=analyze.wavelet(test,"Date.TP",loess.span=0,dt=1/12,dj1/250,lowerPeriod=0.250,upperPeriod=64,make.pval=T,n.sim=10)
wt.avg(my.wx)
library

c3.in <- read.csv("C:/Users/georg/OneDrive/Desktop/c3.in.csv")
View(c3.in)
c3.in$dt <- as.Date(c3.in$Date)

c3.in$dt <- as.Date(c3.in$Date, "%d/%m/%Y")

min<-min(c3.in$dt)
max <- max(c3.in$dt)
new.df <- data.frame(dt = seq(min, max, by = "1 day"))
View(new.df)
new.df <- merge(new.df, data.frame(dt = c3.in$dt,  TP = c3.in$TP), by = "dt", all.x = T)

new.df$TP <- na.approx(new.df$TP
                       #, max.gap = 5
)

my.w = analyze.wavelet(
  new.df,
  "TP",
  loess.span = 0,
  dt = 1,
  dj = 1 / 50,
  lowerPeriod = 2,
  upperPeriod = 500,
  make.pval = T,
  n.sim = 1000
)

wt.image(my.w)

#monthly data processing####

test <- new.df
test$month <- month(test$dt)
test$year <- year(test$dt)

monthly.data <- aggregate(TP ~ month + year, test, mean )

#New data#

new.data <- R_sta_month0416

cells <- unique(new.data$sta)
sta_1E <- new.data[which(new.data$sta == cells[2]), ]

sta_1E$int.tp.out <- na.approx(sta_1E$out_tp_c)

TP.out.wav = analyze.wavelet(
  sta_1E,
  "int.tp.out",
  loess.span = 0,
  dt = 1,
  dj = 1 / 50,
  lowerPeriod = 2,
  upperPeriod = 160,
  make.pval = F,
  n.sim = 1000
)

wt.avg(TP.out.wav)
wt.image(TP.out.wav)

names(sta_1E)














B.1.3 Python Codes of Remote Sensing-Grassland’s Analysis

# Earth Engine Python API
import ee
ee.Authenticate()
ee.Initialize()

!pip install geemap
# import the eefolium 
import geemap

roi = ee.Geometry.Point([-0.627983, 52.074361]).buffer(1000)
# Load 

L8_SR = (ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    .filterDate('2022-05-01', '2023-05-01')
    .filterBounds(roi))

# get collection metadata
L8_SR
# Filter by cloud cover
L8_SR_low_cloud = (ee.ImageCollection('LANDSAT/LC08/C02/T1_L2')
    .filterDate('2022-05-01', '2023-05-01') 
    .filterBounds(roi)
    .filter(ee.Filter.lt('CLOUD_COVER', 20)))
L8_SR_low_cloud
# Sort by a cloud cover property, get the least cloudy image.
least_cloudy = L8_SR_low_cloud.sort('CLOUD_COVER').first()
least_cloudy
date = ee.Date(least_cloudy.get('system:time_start')).format()
print('Timestamp: ')
date.getInfo() # ee.Date
Map = geemap.Map()
Map.center_object(roi)
Map.add_basemap("HYBRID")
Map
L8_SR_rgbVis = {'bands': ["SR_B4", "SR_B3", "SR_B2"], 'min':0, 'max': 0.3}
# Scaling
least_cloudy_scaled = ee.Image('LANDSAT/LC08/C02/T1_L2/LC08_201024_20220811').multiply(0.0000275).add(-0.2) 
Map.addLayer(least_cloudy_scaled,vis_params=L8_SR_rgbVis,name = 'Least_Cloudy_RGB')
Map
# False colour NIR, Red, Green
L8_SR_false = {'bands': ["SR_B5", "SR_B4", "SR_B3"], 'min':0, 'max': 0.3}
Map.addLayer(least_cloudy_scaled,vis_params=L8_SR_false,name = 'Least_Cloudy_False_Colour')
Map.center_object(roi)
Map
# band selector 
bands = ['SR_B5', 'SR_B4', 'SR_B3', 'SR_B2']# NIR, red, green, blue
        
# output 
image_selected= least_cloudy.select(bands)
image_selected
# NDVI = (NIR-Red)/(NIR+Red)
# ee expression() method
ndvi_image = image_selected.expression(
    '(NIR - RED) / (NIR + RED)', {
        'NIR': image_selected.select('SR_B5'),
        'RED': image_selected.select('SR_B4')})
ndvi_image
# how to change band name to NVDI?
ndvi_viz = {'min': -1, 'max': 1, 'palette': ['blue', 'white', 'green']}

# Add the NDVI layer to the map
Map.addLayer(ndvi_image, ndvi_viz, name = 'Least_Cloudy_NVDI') 
Map.center_object(roi)
Map


Averages_Aberdeen_Case_Study
import pandas as pd
import numpy as np
import os
import glob
import matplotlib.pyplot as plt
from datetime import datetime
path = 'C:\\Users\\georg\\OneDrive\\Desktop\\Corn_temp' # use your path
all_files = glob.glob(path + "/*.csv")
all_files
li = []
for filename in all_files:
    df = pd.read_csv(filename, index_col=0, usecols = [0,8,9] ,skiprows = 90, skipfooter=1)
    li.append(df)
temp = pd.concat(li, axis=0)
temp
temp.index=pd.to_datetime(temp.index)
temp = temp.resample("D").mean()
temp
ave_daily_temp = temp.groupby([temp.index.dayofyear]).mean()
ave_daily_temp
path = 'C:\\Users\\georg\\OneDrive\\Desktop\\Corn_rain' # use your path
all_files = glob.glob(path + "/*.csv")

lis = []
for filename in all_files:
    df = pd.read_csv(filename, index_col=0, usecols = [0,3,8] ,skiprows = 61, skipfooter=1)
    lis.append(df)

rain = pd.concat(lis, axis=0)
rain=rain[rain.ob_hour_count==12]
rain.index = pd.to_datetime(rain.index)
rain=rain.resample("1d").sum()
rain
ave_daily_rain =rain.groupby([rain.index.dayofyear]).mean()
overweathered = ave_daily_temp.join(ave_daily_rain['prcp_amt'])
ave_weather
weather=temp.join(rain["prcp_amt"])
weather.to_csv('./NDVI_data_Corn/weather.csv')
ave_weather.to_csv('./NDVI_data_Corn/ave_weather.csv')
plt.plot(ave_weather)
plt.legend(['Max AirTemp', 'Min AirTemp', 'Prcp Amt'])
plt.xlabel('Time(Days)')
plt.ylabel('Values')
plt.show()
import os
os.getcwd()
ave_weather.head()
plt.scatter(ave_weather.index,ave_weather.prcp_amt)

plt.xlabel('Time(Days)')
plt.ylabel('Values')
plt.show()

Day_Time_Surface_Temperature
import ee
# Trigger the authentication flow.
ee.Authenticate()
# Initialize the library.
ee.Initialize()
# Import the MODIS land cover collection.
lc = ee.ImageCollection('MODIS/006/MCD12Q1')

# Import the MODIS land surface temperature collection.
lst = ee.ImageCollection('MODIS/006/MOD11A1')

# Import the USGS ground elevation image.
elv = ee.Image('USGS/SRTMGL1_003')
# Initial date of interest (inclusive).
i_date = '2000-01-01'

# Final date of interest (exclusive).
f_date = '2023-01-01'

# Selection of appropriate bands and dates for LST.
lst = lst.select('LST_Day_1km', 'QC_Day').filterDate(i_date, f_date)
# Define the urban location of interest as a point near Lyon, France.
u_lon = -0.2887
u_lat = 54.207
u_poi = ee.Geometry.Point(u_lon, u_lat)

# Define the rural location of interest as a point away from the city.
r_lon = -0.2883
r_lat = 54.201
r_poi = ee.Geometry.Point(r_lon, r_lat)
scale = 1000  # scale in meters

# Print the elevation near Lyon, France.
elv_urban_point = elv.sample(u_poi, scale).first().get('elevation').getInfo()
print('Ground elevation at urban point:', elv_urban_point, 'm')

# Calculate and print the mean value of the LST collection at the point.
lst_urban_point = lst.mean().sample(u_poi, scale).first().get('LST_Day_1km').getInfo()
print('Average daytime LST at urban point:', round(lst_urban_point*0.02 -273.15, 2), '°C')

# Print the land cover type at the point.
lc_urban_point = lc.first().sample(u_poi, scale).first().get('LC_Type1').getInfo()
print('Land cover value at urban point is:', lc_urban_point)
# Get the data for the pixel intersecting the point in urban area.
lst_u_poi = lst.getRegion(u_poi, scale).getInfo()

# Get the data for the pixel intersecting the point in rural area.
lst_r_poi = lst.getRegion(r_poi, scale).getInfo()

# Preview the result.
lst_u_poi[:5]
import pandas as pd

def ee_array_to_df(arr, list_of_bands):
    """Transforms client-side ee.Image.getRegion array to pandas.DataFrame."""
    df = pd.DataFrame(arr)

    # Rearrange the header.
    headers = df.iloc[0]
    df = pd.DataFrame(df.values[1:], columns=headers)

    # Remove rows without data inside.
    df = df[['longitude', 'latitude', 'time', *list_of_bands]].dropna()

    # Convert the data to numeric values.
    for band in list_of_bands:
        df[band] = pd.to_numeric(df[band], errors='coerce')

    # Convert the time field into a datetime.
    df['datetime'] = pd.to_datetime(df['time'], unit='ms')

    # Keep the columns of interest.
    df = df[['time','datetime',  *list_of_bands]]

    return df
lst_df_urban = ee_array_to_df(lst_u_poi,['LST_Day_1km'])

def t_modis_to_celsius(t_modis):
    """Converts MODIS LST units to degrees Celsius."""
    t_celsius =  0.02*t_modis - 273.15
    return t_celsius

# Apply the function to get temperature in celsius.
lst_df_urban['LST_Day_1km'] = lst_df_urban['LST_Day_1km'].apply(t_modis_to_celsius)

# Do the same for the rural point.
lst_df_rural = ee_array_to_df(lst_r_poi,['LST_Day_1km'])
lst_df_rural['LST_Day_1km'] = lst_df_rural['LST_Day_1km'].apply(t_modis_to_celsius)

lst_df_urban.head()
import matplotlib.pyplot as plt
import numpy as np
from scipy import optimize
%matplotlib inline

# Fitting curves.
## First, extract x values (times) from the dfs.
x_data_u = np.asanyarray(lst_df_urban['time'].apply(float))  # urban
x_data_r = np.asanyarray(lst_df_rural['time'].apply(float))  # rural

## Secondly, extract y values (LST) from the dfs.
y_data_u = np.asanyarray(lst_df_urban['LST_Day_1km'].apply(float))  # urban
y_data_r = np.asanyarray(lst_df_rural['LST_Day_1km'].apply(float))  # rural

## Then, define the fitting function with parameters.
def fit_func(t, lst0, delta_lst, tau, phi):
    return lst0 + (delta_lst/2)*np.sin(2*np.pi*t/tau + phi)

## Optimize the parameters using a good start p0.
lst0 = 20
delta_lst = 40
tau = 365*24*3600*1000   # milliseconds in a year
phi = 2*np.pi*4*30.5*3600*1000/tau  # offset regarding when we expect LST(t)=LST0

params_u, params_covariance_u = optimize.curve_fit(
    fit_func, x_data_u, y_data_u, p0=[lst0, delta_lst, tau, phi])
params_r, params_covariance_r = optimize.curve_fit(
    fit_func, x_data_r, y_data_r, p0=[lst0, delta_lst, tau, phi])

# Subplots.
fig, ax = plt.subplots(figsize=(14, 6))

# Add scatter plots.
ax.scatter(lst_df_urban['datetime'], lst_df_urban['LST_Day_1km'],
           c='black', alpha=0.2, label='Urban (data)')
ax.scatter(lst_df_rural['datetime'], lst_df_rural['LST_Day_1km'],
           c='green', alpha=0.35, label='Rural (data)')

# Add fitting curves.
ax.plot(lst_df_urban['datetime'],
        fit_func(x_data_u, params_u[0], params_u[1], params_u[2], params_u[3]),
        label='Urban (fitted)', color='black', lw=2.5)
ax.plot(lst_df_rural['datetime'],
        fit_func(x_data_r, params_r[0], params_r[1], params_r[2], params_r[3]),
        label='Rural (fitted)', color='green', lw=2.5)

# Add some parameters.
ax.set_title('Daytime Land Surface Temperature Near Filey', fontsize=16)
ax.set_xlabel('Date', fontsize=14)
ax.set_ylabel('Temperature [C]', fontsize=14)
ax.set_ylim(-0, 40)
ax.grid(lw=0.2)
ax.legend(fontsize=14, loc='lower right')

plt.show()
# Define a region of interest with a buffer zone of 1000 km around Lyon.
roi = u_poi.buffer(1e6)
# Reduce the LST collection by mean.
lst_img = lst.mean()

# Adjust for scale factor.
lst_img = lst_img.select('LST_Day_1km').multiply(0.02)

# Convert Kelvin to Celsius.
lst_img = lst_img.select('LST_Day_1km').add(-273.15)
from IPython.display import Image

# Create a URL to the styled image for a region around France.
url = lst_img.getThumbUrl({
    'min': 10, 'max': 30, 'dimensions': 512, 'region': roi,
    'palette': ['blue', 'yellow', 'orange', 'red']})
print(url)

# Display the thumbnail land surface temperature in France.
print('\nPlease wait while the thumbnail loads, it may take a moment...')
Image(url=url)
# Make pixels with elevation below sea level transparent.
elv_img = elv.updateMask(elv.gt(0))

# Display the thumbnail of styled elevation in France.
Image(url=elv_img.getThumbURL({
    'min': 0, 'max': 2000, 'dimensions': 512, 'region': roi,
    'palette': ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']}))
# Create a buffer zone of 10 km around Lyon.
lyon = u_poi.buffer(1000000000)  # meters

url = elv_img.getThumbUrl({
    'min': 150, 'max': 350, 'region': lyon, 'dimensions': 512,
    'palette': ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']})
Image(url=url)
# Get a feature collection of administrative boundaries.
countries = ee.FeatureCollection('FAO/GAUL/2015/level0').select('ADM0_NAME')

# Filter the feature collection to subset France.
france = countries.filter(ee.Filter.eq('ADM0_NAME', 'france'))

# Clip the image by France.
elv_fr = elv_img.clip(france)

# Create the URL associated with the styled image data.
url = elv_fr.getThumbUrl({
    'min': 0, 'max': 2500, 'region': roi, 'dimensions': 512,
    'palette': ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']})

# Display a thumbnail of elevation in France.
Image(url=url)
countries = ee.FeatureCollection('FAO/GAUL/2015/level0').select('ADM0_NAME')
UK = countries.filter(ee.Filter.eq('Woburn', 'UK'))
elv_UK = elv_img.clip(UK)
url = elv_UK.getThumbUrl({
    'min': 0, 'max': 2500, 'region': roi, 'dimensions': 512,
    'palette': ['006633', 'E5FFCC', '662A00', 'D8D8D8', 'F5F5F5']})
Image(url=url)
task = ee.batch.Export.image.toDrive(image=elv_img,
                                     description='elevation_near_Lyon_France',
                                     scale=30,
                                     region=Lyon,
                                     fileNamePrefix='my_export_Lyon',
                                     crs='EPSG:4326',
                                     fileFormat='GeoTIFF')
task.start()
task.status()
link = lst_img.getDownloadURL({
    'scale': 30,
    'crs': 'EPSG:4326',
    'fileFormat': 'GeoTIFF',
    'region': woburn})
print(link)

Delta_Finding_Aberdeen_Case_Study
import pandas as pd
import os
import glob
import numpy as np
import scipy.linalg
from scipy.stats import t as tdstr
from scipy.stats import norm
from scipy.stats import zscore
from scipy.interpolate import interp1d
from scipy.signal import savgol_filter
from datetime import date, datetime, timedelta
from dateutil.relativedelta import relativedelta
import matplotlib
matplotlib.rc('font', size=14)
import matplotlib.pyplot as plt
import numpy.matlib as npm 
%matplotlib inline
from numpy.fft import *
from matplotlib.lines import Line2D
#import NDVI and Climate data
def pick_field(location,num):
    path = './Sites/'+location+'/NDVI/Daily/'+str(num) 
    all_files = glob.glob(path + "/*.csv")

    li = []

    for filename in all_files:
        df = pd.read_csv(filename, index_col=0, header=0, usecols = [0,5], names=["date","NDVI"])
        li.append(df)

    NDVI = pd.concat(li, axis=0)

    path = './Sites/'+location+'/NDVI/16_day/'+str(num) 
    all_files = glob.glob(path + "/*.csv")
    li = []
    for filename in all_files:
        df = pd.read_csv(filename, index_col=0, header=0, usecols = [0,5], names=["date","NDVI"])
        li.append(df)

    s_NDVI = pd.concat(li, axis=0)

    cloud = pd.read_csv('./Sites/'+location+'/weather/cloud.csv', index_col = 0, header = 0, names = ["date", "cloud"])

    rain = pd.read_csv('./Sites/'+location+'/weather/rain.csv', index_col = 0, header = 0, names = ["date", "precip"])

    temp = pd.read_csv('./Sites/'+location+'/weather/temp.csv', index_col = 0, header = 0, names = ["date", "temp"])

    ave_weather = pd.read_csv('./Sites/'+location+'/weather/ave_weather.csv', index_col = 0, header = 0)

    rain.index = pd.to_datetime(rain.index)
    cloud.index = pd.to_datetime(cloud.index)
    NDVI.index = pd.to_datetime(NDVI.index)
    temp.index = pd.to_datetime(temp.index)
    s_NDVI.index = pd.to_datetime(s_NDVI.index)
    sd = NDVI.index[0] #get our start date
  
#     ave_weather = ave_weather.drop(366)
    
    weather = temp.join(rain)

    NDVI = cloud.join(NDVI,how="outer") # do it this way round in case of missing days in the time series
    #get rid of everything before our start date
    NDVI=NDVI[NDVI.index>=sd]
    weather=weather[weather.index>=sd]
    s_NDVI=s_NDVI[s_NDVI.index>=sd]
    return NDVI, s_NDVI, weather, ave_weather
#resample NDVI to the desired interval, omit values below threshold (NDVI before resampling, cloud after)
def c_min_res(N, w, gap=1, ct=10,nt=-1): #(NDVI, Weather, interval, cloud threshold, NDVI threshold) 
    
    NDVI=N.copy()
    weather=w.copy()
    NDVI.loc[NDVI["NDVI"]<nt,"NDVI"]=np.nan
    
    if gap ==1:
        t = NDVI.rename({'cloud':'wm','NDVI':'ndvi'},axis=1)
        t["vari"]=0
        t['num']=1
        t=t[['wm', 'vari', 'num', 'ndvi']]
    else:
        s = NDVI.index[0]
        
        interval = str(gap)+"d"

        NDVI["wm"]=NDVI.resample(interval,origin=s).cloud.transform(min)

        NDVI["dif"]=NDVI["wm"]-NDVI["cloud"]

        t = NDVI[NDVI.dif==0].copy()

        t['vari'] = t.resample(interval, origin=s).NDVI.transform(np.var,ddof=0)

        t["num"] = t.resample(interval, origin=s).NDVI.transform("count")

        t['ndvi']=np.nan

        t['ndvi']=t.resample(interval,origin=s).NDVI.transform("median")

        t=t.resample(interval, origin=s).max()
        
        t=t.drop(["cloud","NDVI","dif"], axis=1)
        weather = weather.resample(interval, origin=s).mean()
    
    t.loc[t["wm"]>ct,"ndvi"] = np.nan
   
    weather = weather[weather.index>=t.index[0]]
        
    return t, weather
class Prior:
    def __init__(self, m, C, S, nu): 
        self.m = m # mean of t-distribution 
        self.C = C # scale matrix of t-distribution
        self.S = S # precision ~ IG(nu/2,S*nu/2)
        self.nu = nu # degree of freedom

class Model:
    def __init__(self,Y,X,rseas,delta):
        self.Y = Y
        self.X = X
        self.rseas = rseas
        #dd = np.ones(4)*delta
        self.delta = delta
        ntrend = 2;nregn = X.shape[1]; pseas = len(rseas);nseas = pseas*2;
        m = np.zeros([ntrend+nregn+nseas,1])
        C = scipy.linalg.block_diag(1*np.eye(ntrend),1*np.eye(nregn),1*np.eye(nseas))
        S = np.power(0.2,2); nu = ntrend+nregn+pseas;
        pr = Prior(m,C,S,nu)
        self.prior = pr
def forwardFilteringM(Model,gap):
        # All the parameters estimated here correspond Eqs. 13-16 and the related ones in the Supplementary Information of Liu et al. (2019)
        # notation in the code -> notation in Liu et al., 2019: 
        # m -> m_t; C -> C_t^{**}; nu -> n_t; 
        # a -> a_t; R -> R_t^{**}; F -> F_t; e -> e_t; y -> y_t; Q -> q_t^{**}; f -> f_t; S -> s_t = d_t/n_t

        Y = Model.Y
        X = Model.X
        rseas = Model.rseas
        delta = Model.delta
        Prior = Model.prior
        period = 365.25/gap
        deltrend = delta[0];delregn = delta[1];delseas = delta[2];delvar = delta[3]
        Ftrend = np.array([[1],[0]]);ntrend = len(Ftrend); Gtrend = np.array([[1,1],[0,1]]);itrend = np.arange(0,ntrend)
        nregn = X.shape[1];Fregn = np.zeros([nregn,1]);Gregn=np.eye(nregn);iregn = np.arange(ntrend,ntrend+nregn)
        pseas = len(rseas);nseas = pseas*2;iseas = np.arange(ntrend+nregn,ntrend+nregn+nseas)
        Fseas = npm.repmat([[1],[0]],pseas,1);Gseas = np.zeros([nseas,nseas]);
        for j in range(pseas):
            c = np.cos(2*np.pi*rseas[j]/period);
            s = np.sin(2*np.pi*rseas[j]/period);
            i = np.arange(2*j,2*(j+1))
            Gseas[np.reshape(i,[2,1]),i] = [[c,s],[-s,c]]
        F = np.concatenate((Ftrend,Fregn,Fseas),axis=0)
        G = scipy.linalg.block_diag(Gtrend,Gregn,Gseas) 
        m = Prior.m; C = Prior.C; S = Prior.S; nu = Prior.nu

        T = len(Y)
        sm = np.zeros(m.shape)
        sC = np.zeros([C.shape[0],C.shape[1],1])
        sS = np.zeros(1)
        snu = np.zeros(1)
        slik = np.zeros(1)
        eeh=np.zeros(1)
        for t in range(T):
            a = np.dot(G,m)
            R = np.dot(np.dot(G,C),np.transpose(G))
            R[np.reshape(itrend,[-1,1]),itrend] = R[np.reshape(itrend,[-1,1]),itrend]/deltrend
            R[np.reshape(iregn,[-1,1]),iregn] = R[np.reshape(iregn,[-1,1]),iregn]/delregn
            R[np.reshape(iseas,[-1,1]),iseas] = R[np.reshape(iseas,[-1,1]),iseas]/delseas
            nu = delvar*nu
            F[iregn,0] = X[t,]
            
            temp = np.dot(R,F)

            A = np.dot(R,F);Q = np.squeeze(np.dot(np.transpose(F),A)+S); A = A/Q; f = np.squeeze(np.dot(np.transpose(F),a))
            y = Y[t]

            if ~np.isnan(y):
                e = y-f; ac = (nu+np.power(e,2)/Q)/(nu+1)
                rQ = np.sqrt(Q)
                mlik = tdstr.pdf(e/rQ,nu)/rQ
                m = a+A*e; C = ac*(R-np.dot(A,np.transpose(A))*Q); nu = nu+1; S = ac*S; 
                # About "S = ac*S" (using the notations in Liu et al. (2019)): 
                # s_t = d_t/n_t = (d_{t-1}+e_t^2/(q_t^{**}/s_t))/n_t = s_{t-1} * (n_{t-1}+e_t^2/(q_t^{**})/n_t = ac * s_{t-1}
            else:
                m = a; C = R;
                if t<T-1:
                    X[t+1,0] = f
                mlik = np.nan
            
            # Debug
            if np.isnan(m[-1]):
                print(R, F, temp)
                return
            #####
                
            sm = np.concatenate((sm,m),axis=1)
            sC = np.concatenate((sC,np.reshape(C,[C.shape[0],C.shape[1],1])),axis=2)
            snu = np.concatenate((snu,[nu]),axis=0)
            sS = np.concatenate((sS,[S]),axis=0)
            slik = np.concatenate((slik,[mlik]),axis=0)
            
        return {'sm':sm, 'sC':sC ,'snu':snu,'slik':slik} 

def computeAnormaly(CLM,AvgCLM,date0,gap):
    deltaT = timedelta(days=gap)
    anCLM = np.zeros([1,CLM.shape[1]])
    for i in range(CLM.shape[0]):
        st = date0+deltaT*(i); st = st.timetuple().tm_yday
        et = date0+deltaT*(i+1); et = et.timetuple().tm_yday               
        if et<st:
            window = np.concatenate((np.arange(st,365),np.arange(0,et)))
        else:
            window = np.arange(st,et)
        window[window==365] = 0  # leap year
        anCLM = np.concatenate((anCLM,np.reshape(CLM[i,:]- np.mean(AvgCLM[window,:],axis = 0),[1,CLM.shape[1]])),axis=0)
        anCLM= anCLM/np.std(anCLM,axis=0)
    return anCLM[1:,:]

def Index_low(nn,date0,percentile,gap):
    intervel = gap
    date0_num = date0.toordinal()
    dd = np.arange(date0,date0+timedelta(days=intervel)*len(nn),timedelta(days=intervel))
    dd_num = np.arange(date0_num,date0_num+intervel*(len(nn)),intervel)
    idq = [i for i in range(len(nn)) if np.isfinite(nn[i])] 
    tt1_num = np.arange(dd_num[idq[0]],dd_num[idq[-1]],1)
    f_itp = interp1d(dd_num[idq], nn[idq],kind = 'linear')
    nn_itp = f_itp(tt1_num)
    
    yday = np.array([date.fromordinal(tt1_num[i]).timetuple().tm_yday for i in range(len(tt1_num))])
    
    ndvi_mean = np.array([np.mean(nn_itp[yday==i]) for i in range(1,366)])
    ndvi_std = np.array([np.std(nn_itp[yday==i]) for i in range(1,366)])
    if len(ndvi_mean)==365:
        ndvi_mean = np.concatenate((ndvi_mean,[ndvi_mean[-1]]),axis = 0)
        ndvi_std = np.concatenate((ndvi_std,[ndvi_std[-1]]),axis = 0)
    
    tt2 = np.arange(dd[0],dd[-1],timedelta(days=1))
    tt2_num = np.arange(dd_num[0],dd_num[-1],1)
    yday2 = np.array([date.fromordinal(tt2_num[i]).timetuple().tm_yday for i in range(len(tt2_num))])
    nv = norm.ppf(1-(1-percentile)/2)
    lowboundary = np.array([ndvi_mean[yday2[i]-1]-nv*ndvi_std[yday2[i]-1] for i in range(len(tt2))])
    
    index_low = [i for i in range(len(dd)) if (~np.isnan(nn[i])) and (nn[i]<lowboundary[tt2_num==dd_num[i]])] 
    return index_low

def PlotEWS(N,date0,sm,sC,snu,gap):
    # thresholds for identification of abnormally high autocorrelation (EWS)  
    # and abnormally low NDVI(ALN)
    quantile1 = 0.95
    quantile2 = 0.80 
    
    steps = [date0+relativedelta(days=gap*i) for i in range(len(N))]
    lown = Index_low(N,date0,quantile2,gap)
    lown_continuous = []
    for i in range(len(lown)):
        tile = [j for j in lown if (j<=lown[i] and j>=lown[i]-int(90/gap))] 
        if len(tile)>2: 
            #NDVI being abnormally low fro more than half of the time within 3 mon
            lown_continuous = np.concatenate([lown_continuous,[lown[i]]])
            lown_continuous = np.array(lown_continuous).astype(int)
    tmp = np.array([steps[i] for i in lown_continuous])
    #diebackdate = tmp[0]
    steps = np.array(steps)
    xpos = date0
    xtick = [date0+relativedelta(years=2*i) for i in range(0,9)]
    fig=plt.figure(figsize=((6.4,4.6)))
    ax1 = plt.subplot(211)
    xlim = [datetime(date0.year,1,1),datetime(2020,1,1)]
    ylim = [0,1]
    ax1.plot(steps,N,'o-k',label='NDVI',ms=2.5)
    ax1.plot(steps[lown_continuous],N[lown_continuous],'or',label='ALN',ms=3)
    ax1.axvspan(datetime(2010,1,1), datetime(2013,1,1), color='brown', alpha=0.1, lw=0)
    ax1.axvspan(datetime(2018,1,1), datetime(2020,1,1), color='brown', alpha=0.1, lw=0)
    
    #xtick = [date0+relativedelta(years=2*i) for i in range(0,2)]
    #ax1.set_xticks(xtick,('00','02','04','06','08','10','12','14','16'))
    ax1.set_ylim(ylim)
    ax1.set_xlim(xlim)
    ax1.set_yticks([0,0.5,1])
    ax1.set_ylabel('NDVI')
    
#     ax1.set_yticklabels([])
    ax1.legend(markerscale=2,
           scatterpoints=1, fontsize=13, loc='lower right',
           ncol=2, borderaxespad=0.5,columnspacing=0.5,labelspacing=0.1)    
    
    plt.setp(ax1.get_xticklabels(), visible=False)
   
    warmup = int(730/gap)
    bd = list(map(lambda m,C,nu: m+C*tdstr.ppf(quantile1,nu),sm,sC,snu))
    bd2 = list(map(lambda m,C,nu: m+C*tdstr.ppf(quantile2,nu),sm,sC,snu))
    
    mbd = np.median(bd[warmup:])
    ews = np.array([i for i,im in enumerate(sm) if im >mbd])
    ews = ews[ews>warmup]
    ews_continuous = []
    window = int(90/gap) # three months
    
    for i in range(len(ews)):
        tile = [j for j in ews if (j<=ews[i] and j>=ews[i]-window)]
        if len(tile)>window-1:
            ews_continuous = np.concatenate([ews_continuous,[ews[i]]])
    ews_continuous = np.array(ews_continuous).astype(int)
    tmp = steps[ews_continuous]
    #ewsdate = tmp[tmp>datetime(2012,7,15)][0]
    #mortdate = datetime(2015,7,15)
    #arrowprops=dict(facecolor='black', shrink=0.05,width=1,headwidth=10)
    
    ax2 = plt.subplot(212)
    
    ylim = [-0.5,0.5]
    
    ax2.plot(steps[1:], sm[1:], lw=3, label='mean')
    ax2.fill_between(steps, 2*sm-bd, bd, facecolor='0.7',label=str(int(quantile1*100))+'% range')
    ax2.fill_between(steps, 2*sm-bd2, bd2, facecolor='0.5',label=str(int(quantile2*100))+'% range')
    
    ax2.plot([steps[warmup],steps[-1]],[mbd,mbd],'--',color='0.4')
    ax2.plot(steps[ews_continuous],sm[ews_continuous],'^r',markersize=2,label='EWS')
    
    ax2.axvspan(datetime(2010,1,1), datetime(2013,1,1), color='brown', alpha=0.1, lw=0)
    ax2.axvspan(datetime(2018,1,1), datetime(2020,1,1), color='brown', alpha=0.1, lw=0)
    ax2.set_xlim(xlim)
    #ax2.set_xticks(rotation=45)
    #ax2.set_xticklabels(('05','07','09','11','13','15','17','19'))
    ax2.set_ylim(ylim)
    ax2.set_xlabel('Year')    
    ax2.set_ylabel('Autocorrelation')
#     ax2.set_xticklabels(['','08','10','12','14','16','18','20']) 
#     ax2.set_yticklabels([])

    yend = -0.28
    ft = 14
    hshift = relativedelta(months=3)
    plt.subplots_adjust(left=None, bottom=None, right=None, top=None, wspace=None, hspace=0.1)
#     ax2.text(mortdate-hshift, 0.05, 'mortality',rotation='vertical',fontsize=ft)
#     ax2.text(diebackdate-hshift, 0.00, 'dieback',rotation='vertical',fontsize=ft)
    #ax2.text(ewsdate-hshift, -0.12, 'EWS',rotation='vertical',fontsize=ft)
    
#     ax2.annotate('', xy=(mortdate, ylim[0]), 
#                 xytext=(mortdate, yend),arrowprops=arrowprops)
#     ax2.annotate('', xy=(diebackdate, ylim[0]), 
#                 xytext=(diebackdate, yend),arrowprops=arrowprops)
#     ax2.annotate('', xy=(ewsdate, ylim[0]), 
#                 xytext=(ewsdate, yend),arrowprops=arrowprops)
    
#     ax1.annotate("a)", xy=(1.01, 0.95), xycoords="axes fraction")
#     ax2.annotate("b)", xy=(1.1, 0.95), xycoords="axes fraction")
    ax2.legend(markerscale=3,
           scatterpoints=1, fontsize=13, loc='best',
           ncol=2, borderaxespad=0.5,columnspacing=0.5,labelspacing=0.1)
    return fig
font = {'family' : 'Arial',
        'weight' :'medium',
        'size'   : 12}
plt.rc('font', **font)
def fit_delta(mod,gap,start=0.96):
    end=0.9999
    for b in [0.005,0.001]:
        r=np.arange(start,end,b)
        err=[]
        like=[]
        print(r)
        for d in r:
            M=Model(mod.Y,mod.X,mod.rseas,d)
            f = forwardFilteringM(M,gap)
            err.append(f["se"])
            like.append(f["slik"])

        loglike=[]
        for arr in like:
            loglike.append(np.nansum(np.log(arr[int(730/gap):])))
        rmse=[]
        mae=[]
        for arr in err:
            rmse.append(np.sqrt(np.nanmean((arr[int(730/gap):]**2))))
            mae.append(np.nanmean(np.abs(arr[int(730/gap):])))

        fig = plt.figure(figsize=(16, 6))
        ax1 = plt.subplot(131)
        ax1.scatter(r,loglike)
        ax1 = plt.subplot(132)
        ax1.scatter(r,mae)
        ax1 = plt.subplot(133)
        ax1.scatter(r,rmse)
        score=np.zeros(len(r))
        for i in range(len(r)):
            score[np.argsort(mae)[i]] +=i
            score[np.argsort(rmse)[i]] +=i
            score[np.argsort(loglike)[i]] +=len(r)-i
        rank=r[np.argsort(score)]
        start = rank[0]-0.004
        end = rank[0]+0.004
        if end >1:
                end=1
        print(rank)
    return rank[0]
def get_con(NDVI,days=365):
    a = NDVI[:days]  # we want the longest run of NDVI values from the first year (but the option to change is nice)
    m = np.concatenate(( [True], np.isnan(a), [True] ))  # Mask
    ss = np.flatnonzero(m[1:] != m[:-1]).reshape(-1,2)   # Start-stop limits
    start,stop = ss[(ss[:,1] - ss[:,0]).argmax()]
    return a.index[start],stop-start
def run_paper(NDVI,weather,gap=1,delta=-1):
    
    N = NDVI.values
    fill_value = -999

    CLM = weather.values
    AvgCLM = ave_weather.values

    date0 = weather.index[0]
    
    anCLM=weather.values

    # center NDVI time series
    N[N==fill_value] = np.nan
    Y = N[1:]-np.nanmean(N) 

    # use two seasonal harmonic components
    rseas = [1,2] 

    # include lag-1 centerred NDVI and precipitation in the regression module 
    X = np.column_stack((N[:-1]-np.nanmean(N),anCLM[:-1,])) 
    
    # set up model and run forward filtering
    delta = delta
    M = Model(Y,X,rseas,delta)
    
#     if isinstance(delta, list):
#         delta = fit_delta(M,gap,delta[0])
    
    M = Model(Y,X,rseas,delta)
    FF = forwardFilteringM(M,gap)

    # model likelihood
    slik = FF.get('slik')

    # extract estimates on the coefficient corresponding to lag-1 NDVI
    vid = 2 # index of autocorrelation
    sm = FF.get('sm')[vid,:] # mean of autocorrelation
    sC = FF.get('sC')[vid,vid,:] # variance of autocorrelation
    snu = FF.get('snu') # degree of freedom

    # plot Fig. 1 in the manuscript
    fig=PlotEWS(N,date0,sm,sC,snu,gap)
    return FF, fig
# denoise using FFT, if not using daily data, threshold should be multiplied by the sampling interval 
def ff_denoise(signal, threshold=2):
    fourier = rfft(signal)
    
    frequencies = rfftfreq(signal.size,d=1/365.25)
        fourier[frequencies > threshold] = 0
    
    return irfft(fourier,len(signal))
###------THE PLAN-----####

# 1.take an NDVI time series (NDVI) and its sampling frequency (gap)
# 2.interpolate across missing values (otherwise the fft just gives us NaN+NaN i for everything)
# 3.apply fft_denoise() (frequency = 2 cycles per year (frequency*gap))
# 4.drop everything more than "pad" below the curve

# 5. repeat steps 2-4 for a set number of iterations, or until no values are dropped, whichever comes first
# Note: currently we're dropping and reinterpolating all previously dropped values

def ff_iterdenoise(NDVI, gap=1, frequency=2, pad=0.1,iterations=20):
    
    # want to take a series or array, return a DF with filtered results +keep originals
    s_NDVI=pd.DataFrame({"raw":NDVI,"filtered":NDVI})
    
    # c is where we're going to keep track of the filtered NDVI values, as well as on which iteration they were dropped
    s_NDVI["c"]=-1
    s_NDVI.loc[s_NDVI.raw.isna(),"c"]=0 # already missing values
    
    # this is a bit dodgey, but what we're doing is appending the count of '-1's in c, and looping until it doesn't change
    li=[-1,-10]  
    i=0
    while li[i] != li[i+1] and i<iterations:
        
        # interpolating
        Y = s_NDVI['filtered'].values
        x =s_NDVI.index.copy()

        indices = ~np.isnan(Y)

        yinterp = np.interp(x[~indices], x[indices], Y[indices])
        Y[~indices] = yinterp
        
        # applying the ff filter from earlier, calculating "error"
        
        ff=ff_denoise(s_NDVI.filtered,frequency*gap)

        s_NDVI["ff"]=ff

        s_NDVI["delta"]=s_NDVI.ff-s_NDVI.filtered

        s_NDVI.loc[s_NDVI.delta>pad,"c"]=i+1  # these are the values below our threshold

#---can uncomment this and plot each time if we want to ----##         

        fig = plt.figure(figsize=(16, 6))
        ax1 = plt.subplot(111)
        ax1.plot(s_NDVI.index,s_NDVI.ff,"-", c='k')
        s=ax1.scatter(s_NDVI.index,s_NDVI.raw, c=1-s_NDVI.c, cmap="Set1",s=10)
#        ax1.plot(sixteen_d, ls="--", marker="x",c="b")
        fig.colorbar(s)
        ax1.set_ylabel('NDVI')
        ax1.set_ylim([-0.1,1])
#         ax1.set_xlim([datetime(2016,1,1),datetime(2020,1,1)])  # <- to view a smaller time period

        s_NDVI.loc[s_NDVI.c!=-1,"filtered"]=np.nan 

        li.append(s_NDVI.c.value_counts()[-1])
        i+=1
        
    s_NDVI.drop("delta",axis=1, inplace=True)    #don't need this
    return s_NDVI
NDVI =pd.read_csv("./NDVI_data_Aberdeen/NDVI.csv" ,index_col=0,)
weather= pd.read_csv("./NDVI_data_Aberdeen/weather.csv",index_col=0) 
ave_weather=pd.read_csv("./NDVI_data_Aberdeen/ave_weather.csv",index_col=0)
weather.index=pd.to_datetime(weather.index)
NDVI.index=pd.to_datetime(NDVI.index)
weather[weather.max_air_temp.isnull()]
weather.loc[weather.prcp_amt.isna(),"prcp_amt"]=0

NDVI.loc[NDVI.NDVI>=1,'NDVI']=np.nan
NDVI=NDVI.resample('D').mean()


start_date=2010

NDVI=NDVI[NDVI.index.year>=start_date]
weather=weather[weather.index.year>=start_date]

weather[["tmax_anom","tmin_anom","p_anom"]]=weather[['max_air_temp', 'min_air_temp', 'prcp_amt']]-ave_weather.loc[weather.index.dayofyear].values

weather[["tmax_anom","tmin_anom","p_anom"]]=weather[["tmax_anom","tmin_anom","p_anom"]]/np.std(weather[["tmax_anom","tmin_anom","p_anom"]])
filtered=ff_iterdenoise(NDVI.NDVI, gap=1, frequency=2, pad=0.1,iterations=15)  #running the function
s,g=get_con(filtered.filtered,90);s,g

f_filt=filtered.filtered[filtered.index>=s].copy()
f_filt=f_filt[f_filt.index<=weather.index[-1]]
W=weather[weather.index>=s].copy()

prob=datetime(2021,1,1)

f_filt=f_filt[f_filt.index<prob]
W=W[W.index<prob]
W=W.drop(['max_air_temp', 'min_air_temp', 'prcp_amt'],axis=1)
print(weather.values[3223])
FFd,fig_d=run_paper(f_filt,W,gap=1,delta=[0.991, 0.998, 0.998,0.98])
index = 3223
print(FFd['sm'][:, index])
print(FFd['sC'][:, :, index])
print(FFd['snu'][index])
print(FFd['slik'][index])
weather[weather['min_air_temp']==0.]
#plt.plot(FFd.get('sm')[2,:])
plt.plot(weather.index, weather.values[:, 1])
plt.plot(weather.index[3223], weather.values[3223, 1], 'k.')
plt.scatter(f_filt.index[3225:],f_filt[f_filt.index[3225:]])
plt.xticks(rotation = 25)
weather.index[3323]
check[np.isnan(check)]=0.2

plt.scatter(f_filt.index,check,s=1)
plt.xticks(rotation = 25)
fig=plt.figure(figsize=((6.4,4.6)))
ax1 = plt.subplot(211)

xlim = [datetime(2004,1,1),datetime(2020,1,1)]
ylim = [0,1]
ax1.plot(f_filt,'o-k',label='NDVI',ms=2.5)
#xtick = [date0+relativedelta(years=2*i) for i in range(0,2)]
#ax1.set_xticks(xtick,('00','02','04','06','08','10','12','14','16'))
ax1.set_ylim(ylim)
ax1.set_xlim(xlim)
ax1.set_yticks([0,0.5,1])
ax1.set_ylim(ylim)
ax1.set_xlim(xlim)
ax1.set_yticks([0,0.5,1])

ax1.set_ylabel('NDVI')
ax1.set_xlabel('year')
font = {'family' : 'Arial',
        'weight' :'medium',
        'size'   : 13}
plt.rc('font', **font)
fig, axs = plt.subplots(6,figsize=(12, 25))
for i,v in enumerate([1,2,3,4,5,7]):
    vid = v
    sm = FFd.get('sm')[vid,:]
    
    axs[i].plot(f_filt.index,sm)
    axs[i].set_xlim([datetime(2008,1,1),datetime(2020,1,1)])
fig, axs = plt.subplots(7,figsize=(12, 20))
for i,v in enumerate([0,1,2,3,4,5,7]):
    vid = v
    sm = FFd.get('sm')[vid,:]
    
    axs[i].plot(filt_sd.index,sm)
    axs[i].set_xlim([datetime(2010,1,1),datetime(2020,1,1)])
first_iteration=ff_iterdenoise(NDVI.NDVI, gap=1, frequency=2, pad=0.1,iterations=1)

font = {'family' : 'Arial',
        'weight' :'medium',
        'size'   : 13}
plt.rc('font', **font)

ylim=[-0.1,1]
ytick=[0,0.5,1]

fig=plt.figure(figsize=(5.9,6.4))
ax1=plt.subplot(212)

s=ax1.scatter(filtered[filtered.c==-1].index, filtered[filtered.c==-1].raw, c="g",s=4,label="Retained")
s=ax1.scatter(filtered[filtered.c!=-1].index, filtered[filtered.c!=-1].raw,c="0.75",s=4,label="Dropped")
ax1.plot(filtered.ff-0.1,c="k",ls="--",label="Threshold",lw=2)
ax1.plot(filtered.ff,c="r",ls="-",label="Smoothed Waveform",lw=2)

ax1.set_xlim([datetime(2010,1,1),datetime(2020,1,1)])
ax1.set_ylim(ylim)
ax1.set_ylabel('NDVI')
ax2=plt.subplot(211)
s=ax2.scatter(first_iteration[first_iteration.c==-1].index, first_iteration[first_iteration.c==-1].raw, c="g",s=4,label="Retained")
s=ax2.scatter(first_iteration[first_iteration.c!=-1].index, first_iteration[first_iteration.c!=-1].raw,c="0.75",s=4,label="Dropped")
ax2.plot(first_iteration.ff-0.1,c="k",ls="--",label="Threshold",lw=2)
ax2.plot(first_iteration.ff,c="r",ls="-",label="Smoothed Waveform",lw=2)
ax2.set_xticklabels(['','08','10','12','14','16','18','20'])
ax2.set_ylim(ylim)
ax2.set_xlim([datetime(2010,1,1),datetime(2020,1,1)])

h1 = Line2D([0], [0], marker='o', markersize=np.sqrt(30), color='g', linestyle='None')
h2 = Line2D([0], [0], marker='o', markersize=np.sqrt(30), color='0.75', linestyle='None')
h3 = Line2D([0], [0],  color='k', linestyle='--',lw=4)
h4 = Line2D([0], [0],  color='r', linestyle='-',lw=4)
# Plot legend.
ax1.legend([h1, h2, h3, h4], ['Retained', 'Dropped','Threshold','Smoothed Waveform'], markerscale=2,
           scatterpoints=1 , loc='lower center',
       ncol=2, borderaxespad=0.5,columnspacing=0.5,labelspacing=0.1)

ax1.set_yticks(ytick)
ax2.set_yticks(ytick)
ax1.set_ylabel('NDVI')
ax2.set_ylabel('NDVI')
ax1.set_xlabel('Year')
plt.setp(ax2.get_xticklabels(), visible=False)
plt.subplots_adjust(left=None, bottom=None, right=None, top=None, wspace=None, hspace=0.1)
font = {'family' : 'Arial',
        'weight' :'medium',
        'size'   : 15}
plt.rc('font', **font)

ylim=[0,1]
ytick=[0,0.25,0.5,0.75,1]
fig=plt.figure(figsize=(7,4))
ax1=plt.subplot(111)

#ax1.plot(filt_sd.filtered, lw=2, ls="-",marker="o",c="k")

ax1.scatter(filt_sd[filt_sd.c!=-1].index,filt_sd[filt_sd.c!=-1].raw, c="red",label="Filtered Values",s=10)
ax1.scatter(filt_sd[filt_sd.c==-1].index,filt_sd[filt_sd.c==-1].raw, c="green", label="Retained Values",s=10)
ax1.set_ylabel('NDVI')
ax1.set_xlabel('Year')
ax1.set_xlim([datetime(2007,1,1),datetime(2020,1,1)])
ax1.set_ylim(ylim)
ax1.set_ylabel('NDVI')
ax1.set_yticks(ytick)
ax1.legend(markerscale=2,
           scatterpoints=1,  bbox_to_anchor=(0., -.3, 1, 0.), loc='center',
           ncol=2, borderaxespad=0.)
font = {'family' : 'Arial',
        'weight' :'medium',
        'size'   : 15}
plt.rc('font', **font)

ylim=[0,1]
ytick=[0,0.25,0.5,0.75,1]
fig=plt.figure(figsize=(7,4))
ax1=plt.subplot(111)

#ax1.plot(filtered.filtered, lw=2, ls="-",marker="o",c="k")

ax1.scatter(filtered[filtered.c!=-1].index,filtered[filtered.c!=-1].raw, c="0.7", label="Retained Values",s=6)
ax1.scatter(filtered[filtered.c==-1].index,filtered[filtered.c==-1].raw, c="k", label="Removed Values",s=6)
s=ax1.scatter(b_30.index, b_30.bise,s=6,c="g", label="BISE values")
#ax1.plot(filt_sd.filtered, c="red",lw=3,alpha=0.5,ls="-",label="Filtered 16-day composite")

ax1.set_ylabel('NDVI')
ax1.set_xlabel('Year')
ax1.set_xlim([datetime(2007,1,1),datetime(2020,1,1)])
ax1.set_ylim(ylim)
ax1.set_ylabel('NDVI')
ax1.set_yticks(ytick)
ax1.legend(markerscale=2,
           scatterpoints=1,  bbox_to_anchor=(0., -.3, 1, 0.), loc='center',
           ncol=3, borderaxespad=0.,columnspacing=0.5)
b_30=pd.read_csv("./Marko_Cloud_Methods/bise_30.csv", index_col=0)
b_30.index=pd.to_datetime(b_30.index,dayfirst=True)
b_30.bise.isna().value_counts()
f_filt.isna().value_counts()

get_Aberd_ndvi-Copy1
import ee
ee.Authenticate()
ee.Initialize()
import pandas as pd
import os
import datetime as dt
import glob
%matplotlib inline
class Profiler():
  def __init__(self, dataset, scale):
    self.dataset = dataset
    self.scale = scale
    self.collection = ee.ImageCollection(self.dataset)

  def get_profile(self, pnt):
    '''Overide this function'''
    def profile_func(image):
      pass

    return profile_func
  
  def profile(self, point, from_date, to_date):
    collection = (
        self.collection
        .filterDate(from_date, to_date)
        .filterBounds(point)
        # More...
    )
    map_func = self.get_profile(point)
    profile_data = collection.map(map_func)

    return profile_data.getInfo()

  def get_name(self):
    return self.dataset
class Mod09gq_profiler(Profiler):

  def __init__(self, scale=250):
    super().__init__('MODIS/006/MOD09GQ', scale)

  def get_profile(self, pnt):
    def profile_func(image):
      ndvi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01'])
      quality = image.select('SR_CLOUD_QA')
      q_reduced = quality.reduceRegion(ee.Reducer.mean(), pnt, self.scale)
      reduced = ndvi.reduceRegion(ee.Reducer.mean(), pnt, self.scale)
      feat = ee.Feature(pnt, {
          'ndvi': reduced.get('nd'),
          'date': ee.Date(image.get('system:time_start')).format('YYYY-MM-dd'),
          #'qa': q_reduced.get('SR_CLOUD_QA')
          })
      return feat

    return profile_func

class Mod13Q1_profiler(Profiler):

  def __init__(self, scale=250):
    super().__init__('MODIS/006/MOD13Q1', scale)

  def get_profile(self, pnt):
    def profile_func(image):
      ndvi = image.select('NDVI').multiply(0.0001)
      reduced = ndvi.reduceRegion(ee.Reducer.mean(), pnt, self.scale)
      feat = ee.Feature(pnt, {
          'ndvi': reduced.get('NDVI'),
          'date': ee.Date(image.get('system:time_start')).format('YYYY-MM-dd')
          })
      return feat

    return profile_func

# for a new one:
# copy and paste Mod09gq_profiler
# rename it
# change string in init to the required satellite (copy from EE datasets site), and scale = resolution of the satellite you using
# swap out bands for Red and NIR in ndvi calculation
# change qa to the qa band of your sat
# maybe add cloud bands (if you want) for S2 or use S2 clpoud prob layer or something
def expand_to_df(dict_of_feats):
  '''Expand key-value pairs to columns'''
  df = pd.DataFrame(dict_of_feats)
  cols = [i for i in df.columns if isinstance(df[i][0], dict)]
  for col in cols:
    df = pd.concat([df.drop([col], axis=1), df[col].apply(pd.Series)], axis=1)
  return df

def to_df(response):
  data = expand_to_df(response['features'])
  data['date'] = pd.to_datetime(data['date'])
  data.set_index('date', inplace=True)

  return data
path = "./NDVI_data_Liz/GEE_data/" 
if not os.path.exists(path):os.mkdir(path)
profiler_mod09ga = Mod09gq_profiler()

# we running it year by year because EE gets grumpy and throws up an error if you try do too much at once
profiles={}
point=ee.Geometry.Point(-5.204953, 49.959741)
for y in range(2010,2021):
    
    sd = dt.datetime(y,1,1).isoformat()[:10]
    ed = dt.datetime(y+1,1,1).isoformat()[:10]

    p = profiler_mod09ga.profile(point, sd, ed)
    profiles[y] = to_df(p)
for key in profiles:
    profiles[key].to_csv(path  +str(key)+".csv")
  all_files = glob.glob(path + "/*.csv")
li = []

for filename in all_files:
    df = pd.read_csv(filename, index_col=0, header=0, usecols = [0,5], names=["date","NDVI"])
    li.append(df)

NDVI = pd.concat(li, axis=0)
NDVI.to_csv("./ndvi_data_Liz/NDVI.csv")
good bad grazing-Copy1
import ee
ee.Authenticate()
ee.Initialize()
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from scipy import signal
roi_good = ee.Geometry.Polygon([
          [ -0.14279741581390226,
            54.106946784906796
          ],
          [
            -0.14511361345373075,
            54.10687785455647
          ],
          [
            -0.14576026761844219,
            54.10597485637973
          ],
          [
            -0.14710060533963087,
            54.10580941946807
          ],
          [
            -0.14694775980947838,
            54.10542339744043
          ],
          [
            -0.14418478292830628,
            54.1057060210629
          ],
          [
            -0.14307959217609323,
            54.10547854366462
          ],
          [
            -0.14056351961144742,
            54.105657768385754
          ],
          [
            -0.1405047328692035,
            54.106195437902215
          ],
          [
            -0.13966996113015284,
            54.106112719968934
          ],
          [
            -0.13948184355587046,
            54.1063884457717
          ],
          [
            -0.14022255650698412,
            54.10644359071205
          ],
          [
            -0.14046946082353884,
            54.1068364962907
          ],
          [
            -0.14106908559358544,
            54.106939891876976
          ],
          [
            -0.1413042325485776,
            54.10710532429343
          ],
          [
            -0.14202176179929893,
            54.10705943556209
          ],
          [
            -0.14193946036041893,
            54.10647352552087
          ],
          [
            -0.14208054854151442,
            54.10639770126343
          ],
          [
            -0.14239799694934163,
            54.10649420483995
          ],
          [
            -0.142750717401384,
            54.10654934963972
          ],
          [
            -0.14282126152374985,
            54.106956040320455
          ]
])

roi_bad = ee.Geometry.Polygon([
          [
            -0.14316222462818473,
            54.1054533330323
          ],
          [
            -0.14049330653341485,
            54.10563255786241
          ],
          [
            -0.14031694630816105,
            54.10459855779979
          ],
          [
            -0.14235096758579857,
            54.104364180867805
          ],
          [
            -0.1423392102382195,
            54.104226311465254
          ],
          [
            -0.14260962925138188,
            54.104171163576154
          ],
          [
            -0.14290356296260143,
            54.10419873753011
          ],
          [
            -0.1431857393247924,
            54.10546711958506
          ]
])
  
features = ee.FeatureCollection([
    #ee.Feature(roi_bad, {'name': 'roi_bad'})
    ee.Feature(roi_good, {'name': 'roi_good'})
])
features = ee.FeatureCollection([
    ee.Feature(roi_bad, {'name': 'roi_bad'})
])
point_bad = roi_bad.centroid()
point_good = roi_good.centroid()

filt_time = ee.Filter.date('2000', '2023')
filt_roi = ee.Filter.bounds(features)

lake_district = ee.ImageCollection('MODIS/061/MOD09Q1')

lake_district_filt = (
    lake_district
    .filter(filt_time)
    .filter(filt_roi)
    )
def ndvi(image):
  value = image.normalizedDifference(
      ['sur_refl_b02', 'sur_refl_b01']).rename('ndvi')
  
  result = image.addBands(value).select('ndvi')

  return result
lakes_ndvi = lake_district_filt.map(ndvi)
lake_district_filt.getInfo()
lakes_bad_ndvi = lakes_ndvi.getRegion(point_bad, scale=250).getInfo()
lakes_good_ndvi = lakes_ndvi.getRegion(point_good, scale=250).getInfo()
lakes_good = lakes_ndvi.getRegion(point_good, scale=250).getInfo()
lakes_ndvi[:4] # First 4 rows
len(mod09gq_ndvi)
lakes_bad_df = pd.DataFrame(lakes_bad_ndvi[1:], columns=lakes_bad_ndvi[0])
lakes_good_df = pd.DataFrame(lakes_good[1:], columns=lakes_good[0])
lakes_bad_df.head()
lakes_bad_df.sort_values('time', inplace=True)
lakes_good_df.sort_values('time', inplace=True)
fig, ax = plt.subplots(figsize=(8, 3))

ax.plot(lakes_bad_df['time'], lakes_bad_df['ndvi'], lw=.1, c='gray')
ax.plot(lakes_good_df['time'], lakes_good_df['ndvi'], 'k-');
filter_bad = signal.savgol_filter(lakes_bad_df['ndvi'], 9, 2)
filter_good = signal.savgol_filter(lakes_good_df['ndvi'], 9, 2)

fig, ax = plt.subplots(figsize=(8, 3))

#ax.plot(mod09gq_df['time'], mod09gq_df['ndvi'], lw=.1, c='gray')
#x.plot(mod09q1_df['time'], mod09q1_df['ndvi'], 'k-', lw=.5)

ax.plot(lakes_bad_df['time'], filter_bad, 'r-');
# using pandas inbuilt functions to covert time

fig, ax = plt.subplots(figsize=(8, 3))

time_x = lakes_bad_df['time'].astype('datetime64[ms]') # everysingle value in the column

p1, = ax.plot(time_x, filter_bad, 'm-', lw=.5, label = "upper woodland")
p2, = ax.plot(time_x, filter_good, 'c-', lw=.5, label = "lower woodland")


# Labels and limits
ax.set_ylabel('Flamborough/Bridlington NDVI')
ax.set_xlabel('Time(Years)')
ax.set_ylim((.4, 1)) # min and max value
ax.legend()

loc = mdates.YearLocator() # formatting in years
ax.xaxis.set_major_locator(loc)

fig.autofmt_xdate()
fig.tight_layout() # squish everything so it fits in the window
# using pandas inbuilt functions to covert time

fig, ax = plt.subplots(figsize=(8, 3))

time_x = lakes_bad_df['time'].astype('datetime64[ms]') # everysingle value in the column

p1, = ax.plot(time_x, filter_bad, 'm-', lw=.5, label = "grazing land")
p2, = ax.plot(time_x, filter_good, 'c-', lw=.5, label = "non-grazing land")


# Labels and limits
ax.set_ylabel('Lakedistrict NDVI')
ax.set_ylim((.4, 1)) # min and max value
ax.legend()

loc = mdates.YearLocator() # formatting in years
ax.xaxis.set_major_locator(loc)

fig.autofmt_xdate()
fig.tight_layout() # squish everything so it fits in the window
fig, ax = plt.subplots(figsize=(8, 3))

time_x = mod09q1_df['time'].astype('datetime64[ms]')

ax.plot(time_x, filter_mod09q1, 'k-', lw=.5)

# Labels and limits
ax.set_ylabel('MOD09Q1 NDVI')
ax.set_ylim((.4, 1))

loc = mdates.YearLocator()
ax.xaxis.set_major_locator(loc)

fig.autofmt_xdate()
fig.tight_layout()
collection = ee.ImageCollection('MODIS/061/MOD09GQ')

filtered = (
    collection
    .filter(ee.Filter.date('2010-01-01', '2021-01-01'))
    .filter(ee.Filter.bounds(features))
    #.filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
    )

def get_values(image, feature_collection):
  '''Function to map to each image in a GEE collection'''
  ndvi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01'])
  regions_fc = ndvi.reduceRegions(
        collection=feature_collection,
        reducer=ee.Reducer.mean(),
        scale=250,
      ).filter(ee.Filter.notNull(['mean'])) # drop nulls
  
  values =  regions_fc.reduceColumns(ee.Reducer.toList(), ['mean'])
  feat = ee.Feature(None, {# Must return a feature
      'ndvi': values.get('list'),
      })
  return feat

data = filtered.map(lambda image: get_values(image, features)).getInfo()
--------read shp---------
!pip install geemap
!pip install pyshp
!pip install pycrs
!pip install geopandas
import ee
ee.Authenticate()
ee.Initialize()
import geopandas as gpd
import geopandas as gpd
import geemap
import shapefile
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from scipy import signal
shp = ee.FeatureCollection('/content/lake_district_coods.shp')
shp = ee.FeatureCollection('/content/yorkshire_dales_coords.shp')

LD = shp.filter(ee.Filter.eq('NAME', 'LAKE DISTRICT'))
YD = shp.filter(ee.Filter.eq('NAME', 'YORKSHIRE DALES'))

def get_values(image, feature_collection):
  '''Function to map to each image in a GEE collection'''
  ndvi = image.normalizedDifference(['sur_refl_b02', 'sur_refl_b01'])
  regions_fc = ndvi.reduceRegions(
        collection=feature_collection,
        reducer=ee.Reducer.mean(),
        scale=250,
      ).filter(ee.Filter.notNull(['mean'])) # drop nulls
  
  values =  regions_fc.reduceColumns(ee.Reducer.toList(), ['mean'])
  feat = ee.Feature(None, {# Must return a feature
      'ndvi': values.get('list'),
      })
  return feat
fig, axes = plt.subplots(2, 1, figsize=(8, 6))
fig.subplots_adjust(hspace=0.3)

colors = ['green', 'red']
labels = ['Lake District', 'Yorkshire Dales']
shps = [LD, YD]
axes = axes.flatten()
dates, data = [], []

for idx, shp in enumerate(shps):
  ax = axes[idx]
  filt_time = ee.Filter.date('2000-12-01', '2001-02-15')
  filt_roi = ee.Filter.bounds(shp)

  mod09q1 = ee.ImageCollection('MODIS/061/MOD09Q1')

  mod09q1_filt = (
      mod09q1
      .filter(filt_time)
      .filter(filt_roi)
      )
  
  data = mod09q1_filt.map(lambda image: get_values(image, shp)).getInfo()
  print(data)

  break
  mod09q1_ndvi = mod09q1_ndvi.getRegion(point, scale=250).getInfo()

  mod09q1_df = pd.DataFrame(mod09q1_ndvi[1:], columns=mod09q1_ndvi[0])

  mod09q1_df.sort_values('time', inplace=True)  
  
  filter_mod09q1 = signal.savgol_filter(mod09q1_df['ndvi'], 9, 2)

  time_x = mod09q1_df['time'].astype('datetime64[ms]')

  ax.plot(time_x, filter_mod09q1, color=colors[idx], lw=.75, label=labels[idx])
  data.append(filter_mod09q1)
  # Labels and limits
  ax.set_ylabel('MOD09Q1 NDVI')
  ax.set_ylim((.4, 1))
  ax.set_title(labels[idx])

loc = mdates.YearLocator()
ax.xaxis.set_major_locator(loc)
fig.autofmt_xdate()
fig.tight_layout()














B.1.4 Rstudio Code of Louisiana Wavelet Analysis 

salinity_data <- read.csv("C:/Users/georg/OneDrive/Desktop/DeltaCSV2/0276.csv", stringsAsFactors=TRUE)
View(salinity_data)

head("C:/Users/georg/OneDrive/Desktop/salinity_data.csv")
#salinity_data$dt <- seq(as.POSIXct("01/01/2013 00:00" , format = "%m/%d/%Y %H:%M"), length.out = nrow(salinity_data), by = "1 hour")
salinity_data$dt_chr <- paste0(salinity_data$Date, " ", salinity_data$Time..hh.mm.ss.)
salinity_data$dt <- as.POSIXct(salinity_data$dt_chr,  format = "%m/%d/%Y %H:%M:%S", tz = "UTC") 

min<-min(salinity_data$dt)
max <- max(salinity_data$dt)
which.max(salinity_data$dt)

new.df <- data.frame(date = seq(min, max, by = "1 month"))

new.df <- merge(new.df, data.frame(date = salinity_data$dt,  Sal = salinity_data$Adjusted.Salinity..ppt.), by = "date", all.x = T)

new.df <-new.df %>% replace(is.na(.), 0)
new.df <- new.df[apply(new.df!=0, 1, all),] 
new.df$Sal <- na.approx(new.df$Sal
                       #, max.gap = 5
)
View(new.df)

library(dplyr)
my.w = analyze.wavelet(
  new.df,
  "Sal",
  
  loess.span = 0,
  
  dt = 1,
  
  dj = 1 / 50,
  
  lowerPeriod = 16,
  
  upperPeriod = 160,
  
  make.pval = F,
  
  n.sim = 1000
  
)
wt.image(my.w, main = "Wavelet Power Spectrum of Salinity_Station CRMS0276", legend.params = list(lab = "wavelet power levels"),
                       
                       periodlab = "Time Period (Months)", show.date = TRUE,
                       date.format = "%Y", timelab = "Date")

#wt.image(my.w, main = "Wavelet Power Spectrum", legend.params = list(lab = "wavelet power levels"), 
         periodlab = "Period (Months)", timelab = "Time (Years)", date.format = "%Y", spec.time.axis = 
           list(at = seq(2013, 2022, by = 1), labels = seq(2013, 2022, by = 1)), ylab = "Date")
library(dplyr)

monthly_mean <- new.df %>%
  mutate(month = floor_date(datetime, "month")) %>%
  group_by(month) %>%
  summarize(monthly_mean = mean(value, na.rm = TRUE))
dplyr::summarise (Mean = mean(new.df$Sal, na.rm = TRUE))
aggregate(x, nfrequency = 1, FUN = sum, ndeltat = 1,
          ts.eps = getOption("ts.eps"), ...)

ggplot(aes(x = new.df$date, y=new.df$Sal)) + geom_col() + labs(title="Mean Salinity per month", y= "Mean Salinity values")
plot(salinity_data$dt, salinity_data$Salinity, xlab = "Time (Years)", ylab = "Salinity (ppt)", type = "l")

acf(new.df$Sal, main = "Salinity")
pacf(new.df$Sal, main = "Salinity", lag.max = 12, plot = TRUE)
lag.plot(new.df$Sal, main = "Salinity CRMS0655-H01", lags = 12, do.lines = FALSE)
#convert this to a date format

new.df$dt <- as.POSIXct(new.df$dt, format = "%Y-%m")
view(new.df$dt)

#dataframe passed to the wavelet transform, date column needs to be called 'date'

df <- data.frame(date = new.df$dt, Sal = new.df$Sal)
df$salinity_interpolate <- na.approx(new.df$Sal
                               #, max.gap = 5
)

min(df$salinity_interpolate)
max(df$salinity_interpolate)

df$salinity_data$dt <- runif(121, min = 0, max = 121) # fake data

#data <- #your data

window <- 16
max.lag <- window -1 

data.v <- salinity_data$Salinity

pcf.results <- matrix(121, nrow = length(data.v) - window, ncol = max.lag)

start.inds <- 1:(length(data.v) - window)

for (i in start.inds) {
  
  start <- i
  end <- start + (window - 1)
  
  subset <- data.v[start:end]
  
  pcf.obj <- pacf(subset, lag.max= max.lag, plot = F)
  pcf.v <- as.vector(pcf.obj$acf)
  
  pcf.results[i, ] <- pcf.v
  
  print(i)
  
}

image(pcf.results)

image(pcf.results, main = "Rolling Autocorrelation of Salinity", xlab = "Dates", ylab = "Lag(Months)") + 
  legend(x= 0, y = 0.75, abbreviate(levels(pcf.results), minlength=1), pch= as.numeric(pcf.results), 
         col= "yellow", yjust=0) 
library(reshape)

df <- melt(pcf.results)
view(df)
library(ggplot2)
figa<-ggplot(df, aes(x= Var1, y= Var2, fill = value)) + 
  geom_tile()+
  ggtitle("Rolling Autocorrelation of Salinity ") +
  xlab("Date (Months)")+ 
  ylab("Lag (Months)")+
  scale_fill_gradient(low="yellow", high="red")+
  theme_bw()+
  font("title", size = 20, face = "bold.italic", color = "black")+
  font("xylab", size = 15, face = "bold", color = "black")+
  font("xy.text", size = 10, face = "bold", color = "black")+
  font("legend.text", size = 10)+
  font("legend.title", face = "bold", size = 10)
ggsave("figa.png",width=30, height=20)
plot(figa)

library(readxl)
library(dplyr)
library(plyr)
library(readr)
library(data.table)
filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/E", pattern = "*.csv", full.names = TRUE)
print(filenames)

fullpath = file.path("C:/Users/georg/OneDrive/Desktop/E", filenames)
print(filenames)

tp_interpolate <- lapply(filenames, fread, sep = ",")
data <- rbindlist(tp_interpolate)

write.csv(data, file = "combined1", rownames = FALSE)
dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
lapply(read.csv)
Combined <- read.csv("Combined.csv", header = T)
lag.plot(mynewmatrix, main = "Correlations Inflow-Outflow of STA_1E", lags = 12, do.lines = FALSE)
library(astsa)
astsa::lag.plot(combined, 12)
library(Hmisc)

cor.test(Combined$tp_interpolate.inflow, Combined$tp_interpolate.outflow, method = "kendall", exact = FALSE)

set1 <- c("tp_interpolate.inflow", "tp_interpolate.outflow")
mynewmatrix<-Combined[set1]
matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)

library(PerformanceAnalytics)
chart.Correlation(mynewmatrix, main = "Correlations Combined")
library(zoo)
library(WaveletComp)
R_sta_34 <- read.csv("R_sta_34.csv", header = T)
R_sta_34$month <- sprintf("%02d", R_sta_34$month)
R_sta_34$Date_txt <- paste0("01-", R_sta_34$month, "-", R_sta_34$year)
R_sta_34$Date_dt <- as.POSIXct(R_sta_34$Date_txt, format = "%d-%m-%Y")
df <- data.frame(date = R_sta_34$Date_dt, in_tp_l = R_sta_34$in_tp_l)
df$tp_interpolate <- na.approx(df$in_tp_l
                               #, max.gap = 5
)
write.table(df, file = "sta6new.csv", sep = ",", row.names = FALSE)
filenames <- list.files(path = "C:/Users/georg/OneDrive/Desktop/D", pattern = "*.csv", full.names = TRUE)
print(filenames)

fullpath = file.path("C:/Users/georg/OneDrive/Desktop/D", filenames)
print(filenames)

tp_interpolate <- lapply(filenames, fread, sep = ",")
data <- rbindlist(tp_interpolate)


dataset <- do.call("rbind", lapply(filenames, FUN = function(files){read.csv(files)}))
lapply(read.csv)
Combined <- read.csv("combinedsta34.csv", header = T)
Combined$x1<- lag(Combined$tp_interpolate.outflow.Mg.L., k =12, na.rm= TRUE)
na.omit(Combined)
Combined$x1[is.na(Combined$x1)] <- 0

library(Hmisc)
library(ggpubr)

cor.test(Combined$tp_interpolate.inflow.Mg.L., Combined$tp_interpolate.outflow.Mg.L., method = "kendall", exact = FALSE)
formula1 <- (y ~ x)
Combined$tp_interpolate.inflow.Mg.L.<- as.numeric(Combined$tp_interpolate.inflow.Mg.L.)
x1<-as.numeric(x1)

A<- ggplot(data= Combined, aes(x = x1, y = tp_interpolate.inflow.Mg.L.))+ 
  xlab("Lag Outflow [TP]-Mg/L")+ 
  ylab("Inflow [TP]-Mg/L")+ 
  ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
  geom_point(size=2, color="black") +
  geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
  stat_poly_eq(formula = formula1,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
               label.x = 0.045, 
               label.y = 0.125,
               output.type = "expression",
               geom = "text",
               color = "black",
               fontface="bold",
               size = 6,
               position = "identity",
               na.rm = FALSE,
               show.legend = NA,
               inherit.aes = TRUE,
               parse = TRUE)+
  stat_cor(method = "pearson",
           aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
           label.x = 0.03, 
           label.y = 0.135,
           output.type = "expression",
           geom = "text",
           color = "black",
           fontface="bold",
           size = 6,
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           parse = TRUE) +
  font("title", size = 15, face = "bold.italic", color = "black") +
  font("xylab", size = 15, face = "bold", color = "black") +
  font("xy.text", size = 10, face = "bold", color = "black") +
  font("legend.text", size = 10) +
  font("legend.title", face = "bold", size = 15)
plot(A)

B<- ggplot(data= Combined, aes(x = tp_interpolate.outflow.Mg.L., y = tp_interpolate.inflow.Mg.L.))+ 
  xlab("Outflow [TP]-Mg/L")+ 
  ylab("Inflow [TP]-Mg/L")+ 
  ggtitle("[TP]-Mg/L Inflow-Outflow of STA34") +
  geom_point(size=2, color="black") +
  geom_smooth(method = "lm", se=TRUE, formula = formula1, color="black")+
  stat_poly_eq(formula = formula1,
               aes(label = paste(..eq.label.., ..rr.label..,sep = "*\", \"*")),
               label.x = 0.045, 
               label.y = 0.125,
               output.type = "expression",
               geom = "text",
               color = "black",
               fontface="bold",
               size = 6,
               position = "identity",
               na.rm = FALSE,
               show.legend = NA,
               inherit.aes = TRUE,
               parse = TRUE)+
  stat_cor(method = "kendall",
           aes(label = paste(..r.label.., ..rr.label.., ..p.label.., sep = "*\", \"*")),
           label.x = 0.03, 
           label.y = 0.135,
           output.type = "expression",
           geom = "text",
           color = "black",
           fontface="bold",
           size = 6,
           position = "identity",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE,
           parse = TRUE) +
  font("title", size = 15, face = "bold.italic", color = "black") +
  font("xylab", size = 15, face = "bold", color = "black") +
  font("xy.text", size = 10, face = "bold", color = "black") +
  font("legend.text", size = 10) +
  font("legend.title", face = "bold", size = 15)
plot(B)
write.table(Combined, file = "sta1enew.csv", sep = ",", row.names = FALSE)
set1 <- c("tp_interpolate.inflow.Mg.L.", "x")

mynewmatrix<-Combined[set1]

matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)
lag.plot(mynewmatrix, main = "TP[Mg/L] Inflow-Outflow of STA_1E", lags = 6, do.lines = FALSE)

library(sjPlot)
library(GGally)
GGally::ggpairs(Combined, columns = 2:3, ggplot2::aes(colour = "tp_interpolate.outflow"))
library(Hmisc)
cor.test(sta_1Einflow$tp_interpolate, sta_1Eoutflow$tp_interpolate, method = "kendall", exact = FALSE)
set1 <- c("", "", "rainfall_sum")
mynewmatrix<-R_sta_6[set1]
matrix2<-rcorr(as.matrix(mynewmatrix))
print(matrix2)
par(mfrow = c(2,1))
plot(in_tp_c, rainfall_sum)
abline(lm(in_tp_c~rainfall_sum))
plot(out_tp_c, rainfall_sum)
abline(lm(out_tp_c ~ rainfall_sum))
library(car)
scatterplot(in_tp_c, rainfall_sum, main = "Inflow STA_6")
scatterplot(out_tp_c, rainfall_sum, main = "Outflow STA_6")
library(PerformanceAnalytics)
chart.Correlation(mynewmatrix, main = "Correlations STA_6")      

