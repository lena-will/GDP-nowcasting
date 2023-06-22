# GDP nowcasting using Google Trends Data

## Getting started
Start by cloning the Git repsitory: 
```sh
git clone https://github.com/lena-will/GDP-nowcasting.git
```

## Introduction
This repository holds the code for my seminar project on GDP nowcasting using Google Trends Data. It mainly follows Ferrara and Simoni (2022) with an additional comparison to the elastic net estimator (Zou and Hastie (2005)).

## Data sources
Data and scripts to get data can be found in the ```Data prep``` folder.
+ Google Trends Data scraped via the ```gtrendsR``` function (find code in ```getGTD.R``` and data in ```gtd_categories.csv```)
+ For convenience, macro data for Germany is summarised in ```macro_data.xlsx```. The data sources are respectively:
  + [Quarterly GDP](https://www-genesis.destatis.de/genesis//online?operation=table&code=81000-0002&bypass=true&levelindex=1&levelid=1685634675885#abreadcrumb)
  + [IP Index](https://www-genesis.destatis.de/genesis//online?operation=table&code=42153-0001&bypass=true&levelindex=0&levelid=1685634299865#abreadcrumb) (specifically: Produktion im produzierenden Gewerbe ohne Bau)
  + [ESI Index](https://economy-finance.ec.europa.eu/economic-forecast-and-surveys/business-and-consumer-surveys/download-business-and-consumer-survey-data/time-series_en)

## Code structure

The main script that runs the ridge after model selection estimations is ```Ridge_after_model_selection.R```. All functions that the main file calls are in the folder ```functions```. Scripts to any plots are in ```plots```.

## References
(1) Ferrara, Laurent and Simoni, Anna (2022). "When are Google Data Useful to Nowcast GDP? An Approach via Preselection and Shrinkage". In: Journal of Business & Economic Statistics, Vol. 00, No. 0, pp. 1–15.

(2) Zou, Hui and Hastie, Trevor (2005). "Regularization and Variable Selection via the Elastic Net". In: Journal of the Royal Statistical Society. Series B (Statistical Methodology), Vol. 67, No. 2, pp. 301-320.

