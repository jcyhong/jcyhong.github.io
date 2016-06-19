---
layout: page
title: Research/Projects
---

* [Machine learning techniques for price change forecast using the limit order book data]({{site.url}}/assets/machine-learning-price-movements.pdf) (with J. Han, N. Sutardja, S.F. Wong, 2015)

<b>Abstract</b>: In the setting of high-frequency trading, stock price movements are fairly predictable. 
We use support vector machines (SVMs) with various kernels and random forests to predict the midprice movements of SPY (SPDR S&P 500 Trust ETF). 
We find that the machine learning approach provides good prediction accuracy. 
This work is based on the paper "Modeling high-frequency limit order book dynamics with support vector machines" by Kercheval and Zhang.

* [An introduction to the use of hidden Markov models for stock return analysis]({{site.url}}/assets/intro-hmm-stock.pdf) (with Y. Pitcan, 2015)

<b>Abstract</b>: We construct two HMMs to model the stock returns for every 10-day period. 
Our first model uses the Baum-Welch algorithm for inference about volatility, which regards volatility as hidden states and uses a mean zero Gaussian distribution as the emission probability for the stock returns. 
Our second model uses a spectral algorithm to perform stock returns forecasting. 
We analyze the tradeoffs of these two implementations as well. [Code](https://github.com/jcyhong/stat241A)

* <a href = "https://github.com/mkoeppe/infinite-group-relaxation-code">Sage program for computation and experimentation with the 1-row Gomory–Johnson infinite group problem</a> (with M. K&ouml;ppe, Y. Zhou, 2014-)
