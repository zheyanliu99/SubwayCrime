# %% packages
import pandas as pd
import googlemaps
import datetime

# %% Function to read result from googlemaps results
def find_index(alist, name):
    for adict in alist[0]['address_components']:
        if name in adict['types']:
            return adict['long_name']
    return 'None'

# configure address
class configure_googlemap():
    def __init__(self):
        # initialize
        self.address_list = {'formatted_address':[],
                             'google_adress':[],
                             'street_number':[],
                             'route':[],
                             'postal_code':[],
                             'sublocality':[],
                             'administrative_area_level_2':[],
                             'administrative_area_level_1':[],
                             'country':[],
                             'lat':[],
                             'long':[],
                             'type':[]
                             }

    def find_index(alist, name):
        for adict in alist[0]['address_components']:
            if name in adict['types']:
                return adict['long_name']
        return 'None'

    def configure_address(self, alist):
        self.address_list['google_adress'].append(find_index(alist, 'establishment'))
        self.address_list['street_number'].append(find_index(alist, 'street_number'))
        self.address_list['route'].append(find_index(alist, 'route'))
        self.address_list['sublocality'].append(find_index(alist, 'sublocality'))
        self.address_list['postal_code'].append(find_index(alist, 'postal_code'))
        self.address_list['administrative_area_level_1'].append(find_index(alist, 'administrative_area_level_1'))
        self.address_list['administrative_area_level_2'].append(find_index(alist, 'administrative_area_level_2'))
        self.address_list['country'].append(find_index(alist, 'country'))


        self.address_list['formatted_address'].append(geocode_result[0]['formatted_address'])
        self.address_list['lat'].append(alist[0]['geometry']['location']['lat'])
        self.address_list['long'].append(alist[0]['geometry']['location']['lng'])
        self.address_list['type'].append(alist[0]['types'])

        return self.address_list

    # def configure_address(self, alist):
    #     self.address_list['google_adress'].append(alist[0]['address_components'][0]['long_name'])
    #     self.address_list['street_number'].append(alist[0]['address_components'][1]['long_name'])
    #     self.address_list['route'].append(alist[0]['address_components'][2]['long_name'])
    #     self.address_list['sublocality'].append(alist[0]['address_components'][3]['long_name'])
    #     self.address_list['administrative_area_level_1'].append(alist[0]['address_components'][5]['long_name'])
    #     self.address_list['administrative_area_level_2'].append(alist[0]['address_components'][6]['long_name'])
    #     self.address_list['country'].append(alist[0]['address_components'][7]['long_name'])

    #     self.address_list['formatted_address'].append(geocode_result[0]['formatted_address'])
    #     self.address_list['lat'].append(alist[0]['geometry']['location']['lat'])
    #     self.address_list['long'].append(alist[0]['geometry']['location']['lng'])
    #     self.address_list['type'].append(alist[0]['types'])

    #     return self.address_list

# %% Use Google map api


# Read subway station df
df = pd.read_csv('passenger_imputed.csv').drop_duplicates(subset=['station', 'linename'])[['station', 'linename']]
df['place_of_interest'] = df['station'] + ' subway station' + ' Line ' + df['linename'] + ', New York'


key  = 'AIzaSyAGrKCa5wYrYmkhiFQcKQ27oz0_jOivtkE' # https://developers.google.com/maps/documentation/javascript/get-api-key
gmaps = googlemaps.Client(key=key)
# Geocoding an address
geocode_result = gmaps.geocode('14th Subway station Line ACE, New York')

amap = configure_googlemap()
for place in df['place_of_interest']:
    print(place)
    geocode_result = gmaps.geocode(place)
    amap.configure_address(geocode_result)


df2 = pd.DataFrame.from_dict(amap.address_list)


df = df.reset_index(drop=True)
df2 = df2.reset_index(drop=True)
pd.concat([df, df2], axis=1).to_csv('subway_info_final3.csv', index = False)