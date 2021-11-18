'''
Author: Zheyan Liu
Date:11/18/2021
'''

# %% packages
import pandas as pd
import googlemaps
import datetime

# %%
def extract_info_from_direction(legs):

    step_dict = {'time':0,
                 'distance':0,
                 'walking_distance':0,
                 'subway_route':{}
    }

    # get general info: time & distance
    step_dict['time'] = legs['duration']['value'] # seconds
    step_dict['distance'] = legs['distance']['value'] # miles

    # subway setp cnt
    transit_num = 1

    # each step
    for step in legs['steps']:

        # process walking
        if step['travel_mode'] == 'WALKING':
            # add up walking distance
            # print(step_dict['walking_distance'])
            # print(step['distance']['value'])
            step_dict['walking_distance'] += step['distance']['value'] # meters

        # process subway
        if step['travel_mode'] == 'TRANSIT':
            # print(step['transit_details'].keys())
            if step['transit_details']['line']['vehicle']['type'] == 'SUBWAY':
                subway_route_dict = {}
                # icon url: '//maps.gstatic.com/mapfiles/transit/iw2/6/us-ny-mta/A.png'
                subway_route_dict['line'] = step['transit_details']['line']['short_name']
                subway_route_dict['departure_stop'] = step['transit_details']['departure_stop']['name']
                subway_route_dict['arrival_stop'] = step['transit_details']['arrival_stop']['name']
                subway_route_dict['num_stops'] = step['transit_details']['num_stops']
                # get departure and arrival time, can calculate time on the train
                subway_route_dict['departure_time'] = step['transit_details']['departure_time']['value']
                subway_route_dict['arrival_time'] = step['transit_details']['arrival_time']['value']

                # append that in step_dict key 'subway_route'
                step_dict['subway_route'][transit_num] = subway_route_dict
                transit_num += 1

    return step_dict

# %%
key  = 'AIzaSyAGrKCa5wYrYmkhiFQcKQ27oz0_jOivtkE' # https://developers.google.com/maps/documentation/javascript/get-api-key
gmaps = googlemaps.Client(key=key)
address = '168 St, New York, NY 10032'
destination = '21 Street-Van Alst Station, Queens, NY 11101'

# Set alternatives = True to get more routes
now = datetime.datetime.now()
directions = gmaps.directions(address, destination, mode = 'transit', 
                              transit_mode = 'subway', alternatives = True,  departure_time=now)

directions[0]['legs']



legs = directions[3]['legs'][0]
adict = extract_info_from_direction(legs)
adict
# %%
