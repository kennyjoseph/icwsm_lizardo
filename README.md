# icwsm_lizardo

Code and replication data for the following article:


```
@inproceedings{joseph_culture_2015,
	title = {Culture, Networks, Twitter and foursquare: Testing a Model of Cultural Conversion with Social Media Data},
	eventtitle = {Proceedings of {ICWSM}},
	author = {Joseph, Kenneth and Carley, Kathleen M},
	date = {2015}
}
```

- Everything really happens off of ```R/result_generation_script.r``` - it contains details in there on how all data is obtained aside from the check-in data. Open up the R.rproj file to get started with replication

- The ```python``` directory contains the code I used to both collect data from twitter and to do certain portions of the analysis.  These scripts are not needed to replicate results, provided you believe that I was able to compute the relatively straightforward values they produce correctly, but are included here for posterity. Additionally, the network data extraction tool uses a library called '''casostwitter''' that is not currently public. I hope to make it so soon, and will provide a link here when that happens

- Note that this directory does not include the check-in data itself, as it is not mine to share. 

- Also, note that the file ```full_paper.pdf``` contains the draft of a full version of the poster paper. **Please** do not cite this full version, instead reference the article in the proceedings given in bibtex above!

- As always, email with questions!