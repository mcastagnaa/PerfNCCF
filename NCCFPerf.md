# Performances and funds NCCF: a data based analysis
_Matteo Castagna (Nov/2013) - OMGI Investment Risk & Performance / v.1.0_
> This paper has been created entirely with *R* language and environment (a GNU project - available as Free Software); raw data are stored and retrieved from a SQLServer dBase. 
The added *R* packages [1] used are listed in the _References page_

### Introduction and summary

A large portion of our time is focused on analysing and making sure funds perform "well". But are fund performances a key factor determining their success?

A successful product is a product that sells. A fund that performs well (is absolute terms, vs. a reference index or vs. peer group) but doesn't sell is not a success. The objective of an asset management company is to sell as many of their fund as they can using scalable operations (i.e. without incurring in excess operational costs).
Sure there are management fees that needs to be accounted for as well; but the focus of this analysis are retail products, not mandates or Hedge Funds.
Retail products only rarely command performance fees [2]. 

This papers demonstate that there is a very weak statistical relationship between a fund performance and the net client cashflows (NCCF - the difference between subscriptions and redemptions); that is: there is no clear relationship between the fund manager success (as measured by its product performance) and the asset management company success (as measured by the increase of AuMs).

### Method

The _IR&P_ team has developed a substantial dataset that enable proper data analysis: this paper is based on two data panels:
- Panel A - covering the entire set of OMGI funds/mandates over the last 10 months (Jan/2013 to Oct/2013) 

```
## [1] "Raw number of products: 122"
```

```
## [1] "Observations: 1079"
```


- Panel B - covering typically the bigger OMGI retail funds alongside up to 7 competitors (as defined by the _product_ unit) and "two bestsellers" (over the last 1m and 3m , as sourced from Morningstar) observed between Apr/2013 and Oct/2013. 

```
## [1] "Raw number of products: 15"
```

```
## [1] "Observations: 653"
```


Panel A is used to analyse the impact of different measures of performance on the monthly NCCF. 
The performance measures used are absolute, relative and sector percentile ranking over 1m, 3m, 6m, 1y, 2y, 3y. The analysis considered as well the 1m and 3m lagged performance variables by 1, 2, and 3 periods (that is the effect on NCCF from the performance 1, 2 and three months before).
When more data will be available further lags will be considered.

Panel B is used to assess how important is the 1y and 3y relative performance (relative to the best sellers) to explain the 1 or 3 months NCCF ratio vs. the best seller NCCF in the peer group.
#### PanelA analysis
The raw dataset for Panel A consists in official performance data (as collected on a monthly basis from Morningstar) and NCCF estimates computed by _IR&P_ based on the following: 
$$
\frac{NCCF_i(t-1, t)}{AuM_i(t-1)} = (\frac{AuM_i(t)}{AuM_i(t-1)} - 1) - {Perf_i(t-1, t)}
$$ 
that is the percent change of AuMs for the product _i_ between _t-1_ and _t_ due to only to subscriptions and redemptions is equal to the percentage change of the fund AuMs less the fund performance over the same period.
What we are doing with that measure is to get rid of the increase of AuM in the fund due to the performance of the fund itself; the latter is typically liked to the market movements and not the activity of the fund manager.

and for analytical purpose (i.e. once outliers have been removed) the set is reduced to 


An added set of panel





What I'm trying to assess with this paper is if there is any sensible statistical relationship between fund performances and 

Yes I know the value of pi is 3.1416, and 2 times pi is 6.2832.
OK, here we go: $\alpha+\beta=\gamma$. 





You can also embed plots, for example:




### Results

### Conclusions

### References
[1] The following packages have been used: RODBC, ggplot2, scales, car, knitr.

[2] in the OMGI case we only have _UKDEFOS_, _GEAR_, _UKOPP_ and _SKMFUT_



![knitr logo](http://yihui.name/knitr/images/knit-logo.png)

| First Header  | Second Header | Third Header         |
| :------------ | :-----------: | -------------------: |
| First row     | Data          | Very long data entry |
| Second row    | **Cell**      | *Cell*               |
| Third row     | Cell that spans across two columns  ||
[Table caption, works as a reference][section-mmd-tables-table1] 

