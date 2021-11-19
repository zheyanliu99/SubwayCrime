'''
Author: Zheyan Liu
Date:11/18/2021
'''

# %% packages
import pandas as pd
import googlemaps
import datetime

# %%

def extract_info_from_direction(legs, route_num):

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
            

        # generate dataframe    
        subway_route = step_dict['subway_route']
        subway_route_df = pd.DataFrame.from_dict(subway_route).T.reset_index(drop = True)
        

        step_dict.pop('subway_route')
        other_info_df = pd.DataFrame.from_dict([step_dict]).reset_index(drop = True)
        # step_df = other_info_df.merge(subway_route_df, how='outer', left_on = 'time', right_on = 'num_stops')
        step_df = pd.concat([other_info_df, subway_route_df], axis = 1)
        step_df = step_df.fillna(step_df.mean())
        step_df['route_num'] = route_num

        return step_df


class google_routes():

    def __init__(self):
        self.key = 'AIzaSyAGrKCa5wYrYmkhiFQcKQ27oz0_jOivtkE'
        self.start_location = '168 St, New York, NY 10032'
        self.destination = '21 Street-Van Alst Station, Queens, NY 11101'
        self.departure_time = datetime.datetime.now()
        self.direction_df_list = []


    def get_directions(self):
        gmaps = googlemaps.Client(key = self.key)
        directions = gmaps.directions(self.start_location, self.destination, mode = 'transit', 
                              transit_mode = 'subway', alternatives = True,  departure_time = self.departure_time)

        for route_num, direction in enumerate(directions):
            direction_df = extract_info_from_direction(direction['legs'][0], route_num + 1)
            self.direction_df_list.append(direction_df)
        
        directions_df = pd.concat(self.direction_df_list, ignore_index=True)
        directions_df.to_csv('directions.csv', index = False)

        return directions_df
        

# %% try this class
mygoogle_routes = google_routes()
a = mygoogle_routes.get_directions()
a