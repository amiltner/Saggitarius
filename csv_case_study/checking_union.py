# -*- coding: utf-8 -*-
"""
Created on Thu Nov 19 09:51:46 2020

@author: arnol
"""

base = "C:/Users/arnol/Documents/Summer2020/GrammarToLP/"
need_to_compute_file = base + "csvBenchmarks/our_results/need_extra_strict_5.txt"
with open(need_to_compute_file, 'r+') as fneeded:
    csvs_needed = set([x.strip() for x in fneeded.readlines()])
need_to_compute_file = base + "csvBenchmarks/our_results/need_extra_strict_5_weird.txt"
with open(need_to_compute_file, 'r+') as fneeded:
    csvs_needed_2 = set([x.strip() for x in fneeded.readlines()])
print("Found {} CSV1s needed".format(len(csvs_needed)))
print("Found {} CSV2s needed".format(len(csvs_needed_2)))
csvs_needed = csvs_needed.union(csvs_needed_2)
print("Found {} Total CSVs needed".format(len(csvs_needed)))