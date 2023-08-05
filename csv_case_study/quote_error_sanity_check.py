import glob

if __name__ == "__main__":
    TIMEOUT_SEC = 60

    # Get old CSV keys.
    base = "C:/Users/arnol/Documents/Summer2020/GrammarToLP/"
    need_to_compute_file = base + "csvBenchmarks/our_results/quote_failure_case.txt"
#    need_to_compute_file = base + "csvBenchmarks/our_results/need_extra_strict_5.txt"
    with open(need_to_compute_file, 'r+') as fneeded:
        csvs_needed = set([x.strip() for x in fneeded.readlines()])
    print("Found {} Total CSVs needed".format(len(csvs_needed)))

    base_dir = "C:/Users/arnol/Documents/Summer2020/GrammarToLP/csvBenchmarks/data/github/"
    found = 0
    missing = 0
    for csvname in csvs_needed:
        full_csv_name = base_dir + csvname
        with open(full_csv_name, 'r') as csvfile:
            x = csvfile.readlines()
            print("Reading csv: ", csvname)
            print("Number of lines: " , len(x))
            contents = "".join(x)
            if "\"" in contents:
                found += 1
                print("Found quote")
            else:
                missing += 1
                print("Missing quote")
            print("-----------------------------")
            
print("Total found: ", found)
print("Total missing: ", missing)