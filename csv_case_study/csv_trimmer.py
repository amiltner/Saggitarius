import glob

if __name__ == "__main__":
    base = "./"
    dataset = "github"
    old_base = base + "data/{}/".format(dataset)
    new_base_5 = base + "trimmed_5/".format(dataset)
    new_base_20 = base + "trimmed_20/".format(dataset)
    
    print(old_base)
    n = len(glob.glob(old_base + "*")) # total number of files
    print("Found {} files: ".format(n))
    for i, name in list(enumerate(glob.glob(old_base + "*"))):
        csvname = name[len(old_base):]
        try:
            with open(name, 'r') as f:
                text = f.readlines()
            with open(new_base_20 + csvname, "w+") as fout:
                fout.writelines(text[:20])
            with open(new_base_5 + csvname, "w+") as fout:
                fout.writelines(text[:5])
        except:
            pass

        if i % 100 == 0 and i != 0:
            print("Finished {} files".format(i))
