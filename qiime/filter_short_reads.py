#!/usr/bin/env python
 
"""Usage:
python filter_short_reads.py X Y > Z
where
X is the input fasta file
Y is the minimum length
Z is the output fasta file
 
"""
from sys import argv
 
from cogent.parse.fasta import MinimalFastaParser
 
fasta_f = open(argv[1], "U")
min_len = int(argv[2])

for label, seq in MinimalFastaParser(fasta_f):
    if len(seq) >= min_len:
        print ">%s\n%s" % (label, seq)