#

# PREDICT-THE-NEXT-BEST-PRODUCT-S-FOR-THE-EXISTING-CUSTOMERS
Bank customers receive many recommendations while many others rarely see any, resulting in an uneven customer experience.
Banks needs to transform their approach from a product-centric focus to a more customer-centric view.
A customer-centric cross-sell approach is built on three principles:
Better customer insight
Analytics to translate data into intelligent product offers
Optimizing business processes

The “Next Best Offer” strategy starts with knowledge about the individual customer so that you know which product is the right thing to offer to that customer. If customer level data is analyzed well and correct approach to offer products is taken, benefits customers to get the financial products they want, and the bank to develop a deeper relationship with the customer. This means that product offerings are tailored for each customer, rather than single products being marketed through large scale and costly outbound campaigns.
As a Next Best Action solution adds value to every customer decision, its bottom line contribution goes up with both the number of interactions and their complexity.
The NBA model provides a decision engine that guides each inbound and outbound customer action and communication for every channel and line of business.

We have conducted our analysis in R Environment. Data visualization is done with tableau and R. Following steps were      executed.
Data Integration & Treatment in R
Dependent Variable Analysis
Missing values & Outliers Treatment
Redundant Variables
Bi-Variate Analysis in R and tableau
Dummy Variable Creation for categorical variables
Chi square test for each categorical variable against the target variables.
Sampling- Test and Train Data
Random Forest Modelling in R
Statistical Analysis
AUC, KS, Gini index and Lift Charts
Validation

#variable transformation

fecha_dato and fecha_alta variable formatted with date format. fecha_dato is the
    row-identifier date and table is partitioned for this column. fecha_alta is the date
    on which the customer joined.
Nomprov, canal_entrada, tiprel_1mes and indrel_1mes, pais_residencia are
    categorical variables. Combining groups with less than 2% of values and assign
    the group as Other.
Converted all categorical variables to factor.
Normalize the renta column and converted to log renta.

# data Imputation
Designing the logic to impute missing values in Renta: We studied the data and decided to impute the missing values of renta by customer nomprov and segmento, as this combination will result in logical estimation of customer income.
Group by on nomprov & segmento: The data was subset where the renta was missing which was then further reduced to keep only the required columns(nomprov, segmento and renta). We took the median instead of mean as this wil also take care of outliers present in the renta
Flagging the missing values and Imputation: Next step was to flag the missing rows (renta) in the train dataset, so that we can later identify what all records have imputedrenta values. Post flagging, we performed left join on the subset data (where renta was missing.
How to treat remaining missing records after the above operation: Check the % number of records which are still NAs. Now looks for renta (Gross income of the household). There are lots of variations between the Provinces. So instead of replacing with average value, value will be assigned based on Province.) to impute median values – this was done using primary key (nomprov concat segmento).
 Outlier treatment: For outlier treatment replace any value which has less than first quantile with 1st quantile and any value which has more than 99 quantile with 99th quantile.
cod_prov (Province Code) is assigned a new code (100) for missing records.
sexo (sex) is assigned a new code (U: Unknown) for blank records.
indrel_1mes (Customer type at the beginning of the month) is assigned code   which is most frequent code for blank code.
tiprel_1mes (Customer relation type at the beginning of the month is assigned   code(I) which is most frequent code for blank code.
canal_entrada(channel used by the customer to join) is assigned a new code (UNK: Unknown) for blank records.
nomprov (Province name) is assigned a new code (UNKNOWN) for blank records.
segmento (segmentation) is assigned the code (04-UNKNOWN) which Is most frequent for blank records.
In addition to NA, there are people with very small age (2) and very high age (164). It’s also interesting that the distribution is not normal where mean is 40.18 and median is 39. Box plot shows outlier at upper and lower both side.  Since there is significant difference between max value and 99th percentile and min value and 1st percentile so to transform its values we have replaced age value with 99th percentile value if value is greater than 99th percentile and replaced its value with 1st percentile if age value is less than 1st percentile.

