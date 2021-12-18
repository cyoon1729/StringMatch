# wget ftp://ftp-trace.ncbi.nih.gov/1000genomes/ftp/technical/reference/human_g1k_v37.fasta.gz
gzip -d human_g1k_v37.fasta
tr -d "\n\r" < ./human_g1k_v37.fasta > ./human_g1k_v37.txt
