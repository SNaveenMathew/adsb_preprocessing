awk -F',' '{ print > "../../" $1 ".csv" }' ../../samples_with_jfk_landing_flag.csv
rm ../../date.csv
