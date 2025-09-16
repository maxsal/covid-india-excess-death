# Age-stratified population data

The national age-stratified population data comes from the World Bank, which itself relies on the [UN World Population Prospects](https://population.un.org/wpp/).

Three separate files were manually downloaded, one for each age group:
- [Age 0 to less than 15](https://data.worldbank.org/indicator/SP.POP.0014.TO?locations=IN): `API_SP.POP0014.TO_DS2_en_csv_v2_675545.csv`
- [Age 15 to less than 65](https://data.worldbank.org/indicator/SP.POP.1564.TO?locations=IN): `API_SP.POP1564.TO_DS2_en_csv_v2_735505.csv`
- [Age 65+](https://data.worldbank.org/indicator/SP.POP.65UP.TO?locations=IN) `API_SP.POP65UP.TO_DS2_en_csv_v2_717723.csv`

In the `old/` subdirectory, we also have the files that were used in preliminary versions of the analysis:
- `population-by-age-group-with-projections.csv` from [Our World in Data](https://ourworldindata.org/grapher/population-by-age-group-with-projections?country=~IND), which itself relies on the UN World Population Prospects 2024.