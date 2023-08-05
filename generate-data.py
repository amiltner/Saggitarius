#!/usr/local/bin/python

from __future__ import print_function
from easyprocess import EasyProcess

import os
import csv
import pretty_csv
from os.path import splitext, join
import subprocess
import sys
import time

from math import sqrt

def can_be_float(s):
    try:
        float(s)
        return True
    except ValueError:
        return False

def can_be_int(s):
    try:
        int(s)
        return True
    except ValueError:
        return False

def clean(s):
    s = str(s)
    if can_be_int(s):
        return int(s)
    elif can_be_float(s):
        f = float(s)
        if f.is_integer():
            return int(f)
        else:
            return "{:.2f}".format(float(s))
    elif s == "timeout":
        return "t/o"
    elif s == "error":
        return "err"
    else:
        return s

def stddev(lst):
    mean = float(sum(lst)) / len(lst)
    return sqrt(float(reduce(lambda x, y: x + y, list(map(lambda x: (x - mean) ** 2, lst)))) / len(lst))

def average(lst):
    return sum(lst)/len(lst)


TEST_EXT = '.ex'
POS_EXT = '.possingle'
NEG_EXT = '.negsingle'
POSNDF_EXT = '.pos'
NEGNDF_EXT = '.neg'
REF_EXT = '.out'
BASELINE_EXT = '.out'
BASE_FLAGS = ["-run-experiments"]
TIMEOUT_TIME = 60
STILL_WORK_TIMEOUT_TIME = 300
GENERATE_EXAMPLES_TIMEOUT_TIME = 600000

REPETITION_COUNT = 10

def ensure_dir(f):
    d = os.path.dirname(f)
    if not os.path.exists(d):
        os.makedirs(d)

def transpose(matrix):
    return list(zip(*matrix))

def check_equal(path,base,data):
    with open(join(path,base + REF_EXT), "r") as exfile:
        return exfile.read() == data

def find_tests(root):
    tests = []
    for path, dirs, files in os.walk(root):
        files = [(f[0], f[1]) for f in [splitext(f) for f in files]]
        tests.extend([(path, f[0]) for f in files if f[1] == TEST_EXT])
    return tests

def find_subs(root):
    dirs = next(os.walk(root))[1]
    groupings=[]
    for direct in dirs:
        files = next(os.walk(join(root,direct)))[2]
        positives = [join(root,direct,f) for f in files if splitext(f)[1] == POS_EXT]
        negatives = [join(root,direct,f) for f in files if splitext(f)[1] == NEG_EXT]
        posndfs = [join(root,direct,f) for f in files if splitext(f)[1] == POSNDF_EXT]
        negndfs = [join(root,direct,f) for f in files if splitext(f)[1] == NEGNDF_EXT]
        groupings.append((direct,positives,posndfs,negatives,negndfs))
    return groupings

def gather_datum(prog, path, base, additional_flags, timeout):
    start = time.time()
    flags = additional_flags
    #flags = map(lambda t: t(path,base),additional_flags)
    print([prog] + BASE_FLAGS + flags + [join(path, base + TEST_EXT)])
    process_output = EasyProcess([prog] + BASE_FLAGS + flags + [join(path, base + TEST_EXT)]).call(timeout=timeout+5)
    end = time.time()
    return ((end - start), process_output.stdout,process_output.stderr)

def gather_data(rootlength, prog, path, base, positives, posndfs, negatives, negndfs,name):
    current_data = {"Test":name}
    pos_flags = [v for p in positives for v in ["-pos",p]]
    posndf_flags = [v for p in posndfs for v in ["-pos-ndf",p]]
    neg_flags = [v for n in negatives for v in ["-neg",n]]
    negndf_flags = [v for n in negndfs for v in ["-neg-ndf",n]]
    example_flags = pos_flags+posndf_flags+neg_flags+negndf_flags

    def gather_col(flags, run_combiner, col_names, timeout_time, repetition_count, compare):
        run_data = []
        timeout = False
        error = False
        incorrect = False
        memout = False
        for iteration in range(repetition_count):
            (time,datum,err) = gather_datum(prog, path, base,flags,timeout_time)
            if err != "":
                print(err)
                error = True
                break
            if time >= TIMEOUT_TIME:
                timeout = True
                break
            if datum == "":
                memout = True
                break
            this_run_data = list(map(lambda d: d.strip(),datum.split(";"))) + [time]
            if not compare and check_equal(path,base,this_run_data[1]):
                incorrect = True
            run_data.append(this_run_data)
        if error:
            print("err")
            for col_name in col_names:
                current_data[col_name]="err"
        elif timeout:
            print("t/o")
            for col_name in col_names:
                current_data[col_name]="t/o"
        elif memout:
            print("m/o")
            for col_name in col_names:
                current_data[col_name]="m/o"
        elif incorrect:
            print("incorrect")
            for col_name in col_names:
                current_data[col_name]="incorrect"
        else:
            run_data_transpose = transpose(run_data)
            combined_data = run_combiner(run_data_transpose)
            for (col_name,data) in list(zip(col_names,combined_data)):
                current_data[col_name] = data

    def ctime_combiner(run_data_transpose):
        data_indices = range(1,len(run_data_transpose))
        cols = [[float(x) for x in run_data_transpose[i]] for i in data_indices]
        averages = [average(col) for col in cols]
        return averages

    def cdata_combiner(run_data_transpose):
        data_indices = range(0,len(run_data_transpose)-1)
        cols = [[float(x) for x in run_data_transpose[i]] for i in data_indices]
        averages = [average(col) for col in cols]
        return averages

    gather_col(example_flags,ctime_combiner,["SatCalls","SolveTime","ParseTime","SubgrammarTime","DeltaDebugTime","TrimTime","ParseCalls","PredTime","ScanTime","CompleteTime","LookupTime","TargetsetTime","InsertionTime","ProvUnionTime","NumRecsCount","ComputationTime"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-satlite"],ctime_combiner,["SatCallsLite","SolveTimeLite","ParseTimeLite","SubgrammarTimeLite","DeltaDebugTimeLite","TrimTimeLite","ParseCallsLite","PredTimeLite","ScanTimeLite","CompleteTimeLite","LookupTimeLite","TargetsetTimeLite","InsertionTimeLite","ProvUnionTimeLite","NumRecsCountLite","ComputationTimeLite"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-prosynth"],ctime_combiner,["SatCallsProSynth","SolveTimeProSynth","ParseTimeProSynth","SubgrammarTimeProSynth","DeltaDebugTimeProSynth","TrimTimeProSynth","ParseCallsProSynth","PredTimeProSynth","ScanTimeProSynth","CompleteTimeProSynth","LookupTimeProSynth","TargetsetTimeProSynth","InsertionTimeProSynth","ProvUnionTimeProSynth","NumRecsCountProSynth","ComputationTimeProSynth"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-size"],cdata_combiner,["SpecSize"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-prod-size"],cdata_combiner,["ProdSize"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-nt-size"],cdata_combiner,["NonterminalSize"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-example-count"],cdata_combiner,["ExampleCount"],TIMEOUT_TIME,REPETITION_COUNT,True)
    gather_col(example_flags+["-random-min-examples"],cdata_combiner,["RandomMinExampleCount"],STILL_WORK_TIMEOUT_TIME,REPETITION_COUNT,True)

    return current_data

def extract_test(x):
    return x["Test"]

def specsize_compare(x,y):
    return int(x["SpecSize"])-int(y["SpecSize"])

def test_compare(x,y):
    return int(x["Test"])-int(y["Test"])

def sort_data(data):
    data.sort(key=extract_test)#sorted(data,cmp=test_compare)

def clean_full_data(data):
    for row in data:
        for key in row.keys():
            row[key] = clean(row[key])

def print_data(data):
    clean_full_data(data)
    ensure_dir("generated_data/")
    with open("generated_data/generated_data.csv", "w") as csvfile:
        datawriter = csv.DictWriter(csvfile,fieldnames=data[0].keys())
        datawriter.writeheader()
        datawriter.writerows(data)

def print_usage(args):
    print("Usage: {0} <prog> <testdir> <foldfile1> <foldfile2> <bstfile>".format(args[0]))

def load_data():
    try:
        with open("generated_data/generated_data.csv", "r") as csvfile:
            datareader = csv.DictReader(csvfile)
            return [row for row in datareader]
    except:
        return []

def main(args):
    if len(args) == 3:
        prog = args[1]
        path = args[2]
        rootlength = len(path)
        data = load_data()
        print(data)
        if not os.path.exists(prog):
            print_usage(args)
        elif os.path.exists(path) and os.path.isdir(path):
            for path, base in find_tests(path):
                print(path)
                for (gnum,positives,posndfs,negatives,negndfs) in find_subs(path):
                    test_name = join(path, base).replace("_","-")[rootlength+1:]+gnum
                    print(test_name)
                    print(gnum)
                    if (not (any(row["Test"] == test_name for row in data))):
                        current_data = gather_data(rootlength,prog, path, base,positives,posndfs,negatives,negndfs,test_name)
                        data.append(current_data)
                        print_data(data)
                    else:
                        print("data already retrieved")
            sort_data(data)
            print_data(data)
        else:
            path, filename = os.path.split(path)
            base, ext = splitext(filename)
            if ext != TEST_EXT:
                print_usage(args)
            else:
                data = gather_data(prog, path, base)
                sort_data(data)
    else:
        print_usage(args)

if __name__ == '__main__':
    main(sys.argv)
