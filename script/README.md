Run run_all.sh from project root (../).
Unfortunately, it's hard to handle all the imports when running the script from its folder.
Ex: ./scripts/run_all.sh


IF YOU RUN run_all.sh FROM ITS FOLDER, IT WON'T WORK!
Ex: ./run_all.sh #--> It doenst work


If you want to run a set of specific exercises, you may specify it on line 5 of run_all.sh script:

for i in {12..15};
for i in {12..14};
for i in {13..15}; and so on...
