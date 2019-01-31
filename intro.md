## Retail Sales Data Analysis

### Introduction:

Federal reserve of St Louis in coordination with of Cencus.gov publishes Retail Industry sales data every month. This data is used by Economists and Wall Street (along with other datasets) for macro trend analysis of economy and retail sector. The data provides valueble information about growth rates of retail, retail ecommerce and various subsectors. Through this project I wish to 
* Explore the data available from these sources
* Combine and Organize the dataset for analysis 
* Develop a Shiny dashboard to view and analyze the data in a flexible manner while also providing some basic trend insights


### Data

**Retail Sales and Inventory Data :** 
Same data available from [FRED Economic Data from St. Louis Fed](https://fred.stlouisfed.org/categories/6) and [U.S. Bureau of the Census,](https://www.census.gov/econ/retail.html).  For this project, I have excluded Auto data to focus on non-Auto Retail for this project.  

**eCommerce Sales Data :** 
This Data is also available from same sources mentioned above. Note that this data includes Auto data as well and so the gross numbers will be higher than Retail numbers used above.

Type                      | Publish Frequency | Data Granularity
--------------------------|-------------------|------------------ 
Retail Sales and Inventory| Monthly           | Monthly
Retail eCommerce Data     | Quarterly         | Quarterly




### How to use this webApp:

**1. Retail Overall Page:** This page provides insights at overall Retail Industry level. Data is available since *Jan 1992*. The dropdown and radio selectors at the top of the enable selecting and narrowing data.

**2. Sub Sector Analysis Page :** This page helps explore data specific to a sub-sector within Retail.

**3. eCommerce Page: ** This page provides some basic statistics on Retail eCommerce (note that eCommerce is broader than Retail eCommerce). 


### Future Work:

- FRED publishes eCommerce data for some Retail sub-sectors. Plan to include that data into eCommerce page
- Include employment data also published by FRED.
- More statistical insights

