# R code
A collection of R files (mostly modelling and scraping) 

- [scraping](https://github.com/PavChristian/R_code/tree/main/scraping)
This folder contains multiple files with code used for scraping and sentiment analysis (without using LLMs) of news articles from the New York Times, Wall Street Journal, and Proquest (a popular database with a plethora of scientific articles and news reports from various outlets)
  - NYT_Metadata_scraping -- code for getting metadata using the NYT API and testing the small scraping algorithm
  - NYT_Scraping -- code that iterates over links in the dataset and scrapes article texts from the website
  - Proquest -- scraping proquest articles (all the fields)
  - sentiment_analysis -- code for transforming data, calculating sentiment scores, and applying models to test the existence of a relationship between sanctions and the sentiment in media coverage.
- [china_trade](https://github.com/PavChristian/R_code/tree/main/china_trade)
Contains code I used to build gravity models and graphs for an academic paper about the relationship between democracy in the former Soviet space and trade with China.

- [MNIST](https://github.com/PavChristian/R_code/blob/main/mnist.Rmd)
Contains code for building models used for predicting numbers from the famous MNIST dataset. This particular file leverages QDA and LDA to distinguish between 7s and 8s. 
