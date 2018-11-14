WEVOTE_DB=~/databases/wevote
OUTPUT_PREFIX=$(dirname $1)/wevote
OUTPUT_FILE=$(dirname $1)/wevote_WEVOTE_Details.txt
CLASSIFICATAIONS_FILE=$(dirname $1)/wevote_classifications.tsv
ABUNDANCES_FILE=$(dirname $1)/wevote_Abundance.csv
PROFILE_FILE=$(dirname $1)/wevote_profile.tsv
#~/WEVOTE/bin/WEVOTE -i $1 -d $WEVOTE_DB -p $OUTPUT_PREFIX -n 4 -a 0 -s 0 -k 2
#mv $OUTPUT_FILE $CLASSIFICATAIONS_FILE
~/WEVOTE/run_ABUNDANCE.sh -i $CLASSIFICATAIONS_FILE -p $OUTPUT_PREFIX --db $WEVOTE_DB
mv $ABUNDANCES_FILE $PROFILE_FILE
