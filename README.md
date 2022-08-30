wget https://raw.githubusercontent.com/rdeits/cryptics/master/raw_data/UKACD.txt
tail -n +26 UKACD.txt | tr A-Z a-z | sed 's/\s//g' | sed "s/'//g" | sed "s/-//g" | sed "s/\///g" | sed "s/!//g" | sed "s/?//g" |sed "s/,//g" |sed "s/;//g" |sed "s/\.//g" | grep -P '^[[:ascii:]]*$' | sed -nr '/^.{0,30}$/p'  > UKACD.processed.txt
