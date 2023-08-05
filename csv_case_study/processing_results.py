# -*- coding: utf-8 -*-
"""
Created on Sun Nov 15 20:00:38 2020

@author: arnol
"""

import json
import os
import glob

base_path = "C:/Users/arnol/Documents/Summer2020/CSV_Wrangling-master/results/test/detection"

def my_join(base, extension):
    if base[-1] == "/":
        return base + extension
    else:
        return base + "/" + extension

def get_predictions(extension):
    results_dict = {}
    with open(my_join(base_path, extension), 'r') as f:
        for jsonObj in f:
            js = json.loads(jsonObj)
            if 'dialect' in js:
                results_dict[js['filename']] = js['dialect']
            else:
                results_dict[js['filename']] = None
    return results_dict

human_data_dict = get_predictions("out_human_github.json")
#full_score_dict = get_predictions("out_sniffer_github.json")
full_score_dict = get_predictions("out_our_score_full_github.json")

our_base = "C:/Users/arnol/Documents/Summer2020/GrammarToLP/"
#with open(our_base + "csvBenchmarks/our_results/dialect_slim_ascii_lenient_5.txt") as f:
#with open(our_base + "csvBenchmarks/our_results/dialect_constant_ascii_strict_5.txt") as f:
#with open(our_base + "csvBenchmarks/our_results/dialect_constant_ascii_strict_20.txt") as f:
#with open(our_base + "csvBenchmarks/our_results/rfc_ascii_20.txt") as f:
with open(our_base + "csvBenchmarks/our_results/rfc_ascii_5.txt") as f:
    results = f.readlines()
our_res_dict = {}
cnothing = 0
ctimeout = 0
correct = 0
wrong = 0
dwrong = 0
qwrong = 0
ewrong = 0

wrangler_correct= 0 
wrangler_wrong= 0 
wrangler_dwrong = 0
wrangler_qwrong = 0
wrangler_ewrong = 0
wrangler_missing = 0

#failures = open(our_base + "csvBenchmarks/our_results/need_further_rfc.txt", "w+")

unambiguous = 0
ambiguous = 0
for line in results:
    csvname = line[:line.index(" ")]
    gh_name = "./data/github/"+csvname
    their_answer = human_data_dict[gh_name]
    our_answer_str = line[line.index(" ")+1:]
    
    if "NOTHING" in our_answer_str:
        cnothing += 1
#        failures.write(csvname + "\n")
    elif "TIMEOUT" in our_answer_str:
        ctimeout += 1
#        failures.write(csvname + "\n")
    else:
        our_answer = eval(our_answer_str) 
        if 'delimiter' in our_answer:
            if "\\t" in our_answer['delimiter']:
                our_answer['delimiter'] = "\t" # removes erroneous backslash               
        if 'quotechar' in our_answer:
            if "'" in our_answer['quotechar']:
                our_answer['quotechar'] = "'" # removes erroneous backslash
        if 'escapechar' in our_answer:
            if "\\\\" in our_answer['escapechar']:
                our_answer['escapechar'] = "\\" # removes erroneous backslash
        our_res_dict[csvname] = our_answer
        
        # comparing to our answer
        if their_answer == our_answer:
            correct += 1
            old_correct = True
        else:
            old_correct = False
            wrong += 1
            if our_answer['delimiter'] != their_answer['delimiter']:
                print(csvname)
                print("Our delim answer: {} --- Their answer: {}".format(our_answer['delimiter'],their_answer['delimiter']))
                dwrong += 1
            if our_answer['quotechar'] != their_answer['quotechar']:
                print(csvname)
                print("Our quote answer: {} --- Their answer: {}".format(our_answer['quotechar'],their_answer['quotechar']))
                qwrong += 1
            if our_answer['escapechar'] != their_answer['escapechar']:
                print(csvname)
                print("Our escape answer: {} --- Their answer: {}".format(our_answer['escapechar'],their_answer['escapechar']))
                ewrong += 1
                           
    #        comparing to wrangler answer
        wrangler_answer = full_score_dict[gh_name]
        if not wrangler_answer:
            wrangler_missing += 1
        else:
    #            if check_at_end:
    #                print("Wrangler answer: {} --- Their answer: {}".format(wrangler_answer['quotechar'],their_answer['quotechar']))
            if wrangler_answer == their_answer:
                wrangler_correct += 1
            else:
                wrangler_wrong += 1
                if wrangler_answer['delimiter'] != their_answer['delimiter']:
                    wrangler_dwrong += 1
                if wrangler_answer['quotechar'] != their_answer['quotechar']:
                    wrangler_qwrong += 1
                if wrangler_answer['escapechar'] != their_answer['escapechar']:
                    wrangler_ewrong += 1
        
#print("Unambiguous: {}".format(unambiguous))
#print("Ambiguous: {}".format(ambiguous))
print("\n Our results")
print("Timeout: {}".format(ctimeout))
print("Nothing found: {}".format(cnothing))
print("Correct: {}".format(correct))
print("Wrong: {}".format(wrong))
print("DWrong: {}".format(dwrong))
print("QWrong: {}".format(qwrong))
print("EWrong: {}".format(ewrong))

print("\n Wrangler Results")
print("Correct: {}".format(wrangler_correct))
print("Wrong: {}".format(wrangler_wrong))
print("Missing: {}".format(wrangler_missing))
print("DWrong: {}".format(wrangler_dwrong))
print("QWrong: {}".format(wrangler_qwrong))
print("EWrong: {}".format(wrangler_ewrong))

#failures.close()


#i = 0
#def is_ascii(s):
#    return all(ord(c) < 128 for c in s)
#skipped = 0
#for fname, answer in human_data_dict.items():
#    base = "./data/github/"
#    if answer:
#        csv_name = fname[len(base):]
#        try:
#            with open("./thousand_github_files_20/" + csv_name,'r') as f:
#                lines = f.readlines()
#                if is_ascii("".join(lines)):
#                    with open("./hundred_ascii_files_20/" + csv_name, 'w+') as g:
#                        g.writelines(lines[:20])
#                        i += 1
#                        if i == 100:
#                            break
#                else:
#                    skipped += 1
#        except:
#            continue
#print("Number skipped: {}".format(skipped))
#        
#for filename in glob.glob(our_base + "csvBenchmarks/hundred_ascii_files_20/*"):
#    start = len(our_base) + len("csvBenchmarks/hundred_ascii_files_20/")
#    csvname = filename[start:]
#    with open("./hundred_ascii_files_20/" + csvname,'r') as f:
#       with open("./hundred_ascii_files_5/" + csvname, 'w+') as g:
#           g.writelines(f.readlines()[:5])