# StringMatch
Parallel Exact String Matching in Haskell

# Usage
We used a 3.1 GB human reference genome DNA sequence for our experiments. The file can be downloaded by running the `data/download_human_g1k_v37_fasta.sh` script. Due to the large file size, we have also prepared smaller texts, like `data/hamlet.txt`, a text file of Shakespeare's Hamlet. 

To run the program, there are a couple options:

1. To run sequential matching for a pattern that is given a string, do
```
stack run [pattern] [path_to_search_text]
```

2. To run parallel matching for a pattern that is given as a string, do (for example)
```
stack run p [pattern] [path_to_search_text] [num_partitions] -- +RTS -N8 -ls
```

3. To run sequential matching for a pattern that is given as a text file, do
```
stack run f [path_to_pattern] [path_to_search_text]
```

4. To run sequential matching for a pattern that is given as a text file, do
```
stack run pf [path_to_pattern] [path_to_search_text] [num_partitions] -- +RTS -N8 -ls
```

Example:
```
stack run p "To be, or not to be" "data/hamlet.txt" 8 -- +RTS -N8 -ls
```

