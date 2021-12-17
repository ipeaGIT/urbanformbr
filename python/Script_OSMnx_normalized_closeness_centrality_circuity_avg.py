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
import time
import multiprocessing
from multiprocessing import Pool
from multiprocessing import Process
import statistics


ag = gbr.read_urban_concentrations(year=2015)
ag["codigo"] = ag["name_urban_concentration"]
ag = ag.dissolve(by='name_urban_concentration')

def run_osmnx(i):
    start = time.perf_counter()
    print(i)
    ag_sub = ag[i:i+1]
    polygon = ag_sub['geometry'].iloc[0]

    G = ox.graph_from_polygon(polygon, network_type='drive')
    basic_stats = ox.basic_stats(G)
    node_closeness_centrality = nx.closeness_centrality(G)
    a = max(node_closeness_centrality.values())
    b = min(node_closeness_centrality.values())
    
    factor = a-b
    for k in node_closeness_centrality:
        node_closeness_centrality[k] = (node_closeness_centrality[k] - b)/factor
    res = statistics.mean(node_closeness_centrality.values())
    
    row = [ag_sub['codigo'][0],res, basic_stats['circuity_avg']]
    csv_writer = writer(open('C:/Users/Diego/OneDrive/IPEA/OSMnx/saida_OSMnx_CC_normalized.csv', "a+", newline=''))
    csv_writer.writerow(row)
    finish = time.perf_counter()
    print(f'Terminou em {round(finish-start,2)} segundos')

        
def test(l): 
    with concurrent.futures.ThreadPoolExecutor(max_workers=10) as executor:
        return executor.map(run_osmnx, l)
 
        
if __name__ == "__main__":
    n = range(0,186)
    results = test(n)