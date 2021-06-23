#!/usr/bin/env python
import os, sys, inspect

sys.path.append(os.path.dirname(os.path.abspath(inspect.getfile(inspect.currentframe()))) + "/..")


import mnrl

import argparse

if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("infile")
    parser.add_argument("outfile")
    args = parser.parse_args()
    m = mnrl.loadMNRL(args.infile)
    m.exportToFile(args.outfile)