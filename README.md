# Tracing inclusivity at UNFCCC conferences through side events and interest group dynamics 
This GitHub repository contains the source code for tracing UNFCCC COP side event topics and network relationships of their organisers. This work uses open-source secondary data and the data sources can be found in the Methods section of the manuscript. 

Any correspondence can be made to Judy Xie: j.xie20@imperial.ac.uk 

## Software Requirements
The scripts are written in Python (Version 3.10.13) using Jupyter Notebooks, R (Version 4.3.1), and Gephi version 0.10. 
 
## Overview
This repository includes an analysis folder and two special output folders.
- <code>analysis</code> includes three subfolders <code>delegates</code>, <code>orgs</code>, and <code>side_events</code> which analyses the delegate nomination mechanisms, extracts the organisation databases, and analyses the topics and organisational networks in side events. Each of the subfolders include the data files or code used to extract the data. 
    - In the <code>delegates</code> subfolder, the main data sources are the [UNFCCC List of participants: on-site participation](https://unfccc.int/documents/636674) (accessed 31 Jan 2024) and the anonymised list of fossil fuel lobbyist developed by the [Kick Big Polluters Out coalition](https://docs.google.com/spreadsheets/d/1irVXQGiYuiDjHm4tQjrMl0AFauuoyo1KW6jYiHW8i0I/edit?pli=1&gid=783251733#gid=783251733) (accessed 30 Jul 2024). They are presented as the participant nomination sankey diagram nodes and links data located in <code>delegates/sankey</code>. 
    - In the <code>orgs</code> subfolder, the main data sources are the UNFCCC [admitted IGO database](https://unfccc.int/process/parties-non-party-stakeholders/non-party-stakeholders/admitted-igos/list-of-admitted-igos) (accessed 13 Feb 2024) and [admitted NGO database](https://unfccc.int/process/parties-non-party-stakeholders/non-party-stakeholders/admitted-ngos/list-of-admitted-ngos) (accessed 31 Jan 2024).
    - In the <code>side_events</code> subfolder, data are extracted from the [UNFCCC SEORS database](https://seors.unfccc.int/applications/seors/reports/archive.html) (accessed 14 Mar 2024).
- <code>graph-files</code> includes the four networks visualised in the manuscript and can be opened using the publicly available Gephi software.
- <code>network-results</code> includes the network statistics of nodes and edges from the analysis.  