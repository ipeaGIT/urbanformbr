# -*- coding: utf-8 -*-
"""
Created on Wed Jul  7 15:54:40 2021

@author: Diego
"""

import geopandas as gpd
import pandas as pd
import osmnx as ox
import networkx as nx
import geobr as gbr
import matplotlib.pyplot as plt
import csv
from csv import writer
import concurrent.futures
from concurrent.futures import ProcessPoolExecutor
import time
import multiprocessing
from multiprocessing import Pool
import fiona

temp = pd.read_csv("C:/Users/b35143921880/Downloads/urban_extent_cutoff_20_shape/urban_areas.csv")

def run_osmnx(i):
    try:
        start = time.perf_counter()
        print(i)
        ag = gpd.read_file("C:/Users/b35143921880/Downloads/urban_extent_cutoff_20_shape/" + temp["codigo"][i])
        ag["codigo"] = ag["name_uca_case"]
        ag = ag.dissolve(by='name_uca_case')
        ag_reprojc = ag.to_crs({'init':'epsg:5070'})
        area = ag_reprojc.area
            
        ag = ag.to_crs({'init':'epsg:4326'})
        ag_sub = ag
        area1 = area
        polygon = ag_sub['geometry'].iloc[0]
    
        G = ox.graph_from_polygon(polygon, network_type='drive')
        basic_stats = ox.basic_stats(G, area=area1)
        row = [ag_sub['codigo'][0], basic_stats['intersection_density_km']]
        csv_writer = writer(open('C:/Users/b35143921880/Downloads/saida_OSMnx_v6.csv', "a+", newline=''))
        csv_writer.writerow(row)
        finish = time.perf_counter()
        print(f'Terminou em {round(finish-start,2)} segundos')

    except Exception as e:
        raise Exception(str(e))

def test(l):
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        return executor.map(run_osmnx, l)

if __name__ == '__main__':
   n = range(0,184)
   results = test(n)



