import glob
import signal
import subprocess


class TimeoutException(Exception):   # Custom exception class
    pass


def timeout_handler(signum, frame):   # Custom signal handler
    raise TimeoutException


def parse_result(x):
    res_dict = {'delimiter': '', 'escapechar': '', 'quotechar': ''}
    for line in x.split("\n"):
        if "Found nothing" in line:
            return "NOTHINGFOUND"
        if "FieldSep -> " in line:
            lbound = line.index('[') + 1
            rbound = line.index(']')
            res_dict['delimiter'] = line[lbound:rbound]
        if "Escape -> " in line:
            lbound = line.index('[') + 1
            rbound = line.index(']')
            res_dict['escapechar'] = line[lbound:rbound]
        if "Quote -> " in line and "Not" not in line:
            lbound = line.index('[') + 1
            rbound = line.index(']')
            res_dict['quotechar'] = line[lbound:rbound]
    return res_dict
if __name__ == "__main__":
    TIMEOUT_SEC = 60

    # Get old CSV keys.
    base = "/mnt/c/Users/arnol/Documents/Summer2020/GrammarToLP/"
    need_to_compute_file = base + "csvBenchmarks/our_results/need_further_rfc.txt"
    with open(need_to_compute_file, 'r+') as fneeded:
        csvs_needed = set([x.strip() for x in fneeded.readlines()])
    print("Found {} Total CSVs needed".format(len(csvs_needed)))
    print(list(csvs_needed)[0])
    induction_tool = base + "App.exe" 
    metagrammar_names = ["rfc"]
    for metagrammar_name in metagrammar_names:               
        csvnames = set()
        metagrammar_spec = base + "csvBenchmarks/{}.ex".format(metagrammar_name)
        try:
            outpath = base + "csvBenchmarks/our_results/{}_{}.txt".format(metagrammar_name, "ascii_5")
            outfile = open(outpath, "r")
            lines = outfile.readlines()
            for line in lines:
                csvnames.add(line[:line.index(" ")])
            outfile.close()
            print("Found {} cached results: ".format(len(csvnames)))
        except:
            print("No cached data found.")
        
        writefile = open(outpath, "a+")
        trimmed = True
        base_dir = "/mnt/c/Users/arnol/Documents/Summer2020/GrammarToLP/csvBenchmarks/hundred_ascii_files_5/"
        n = len(glob.glob(base_dir + "*")) # total number of files
        print("Found {} files: ".format(n))
        for i, name in list(enumerate(glob.glob(base_dir + "*"))):
            csvname = name[len(base_dir):]
            if csvname not in csvnames and csvname in csvs_needed:
#            if csvname not in csvnames:
                try:
                    result = subprocess.run([induction_tool, metagrammar_spec, "-pos", name], 
                                            stdout = subprocess.PIPE, timeout=TIMEOUT_SEC).stdout.decode('utf-8')
                    writefile.write(csvname + " {}\n".format(parse_result(result).__repr__()))
                except subprocess.TimeoutExpired:
                    print("Hit timeout for {}".format(csvname))
                    writefile.write(csvname + " TIMEOUT{}\n".format(TIMEOUT_SEC))
                # Reset the alarm to 0
                signal.alarm(0)
    
            if (i+1) % 10 == 0:
                print("Finished {} files".format(i))
            writefile.flush()
        writefile.flush()
        writefile.close()
