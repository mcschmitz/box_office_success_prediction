# Forecasting Box Office success based on Google Trends data

**Predicting  the amount of people watching a movie on the opening weekend in german cinemas by using meta information
of the movies linked with Google Trends data of search terms like the title of the movie or general terms like "Kino" 
(german for cinema). Models used for predition are additive regression models as well as boosting models.**

### Background
Inspired by a [whitepaper by Google](http://dl.icdst.org/pdfs/files1/350427db54ce5dcf1e46ad7f00d2e2cf.pdf) the models
fitted in this reposititory try to predict the amount of moviegoers for specific movies on their opening weekend.
In this paper Google is presenting several linear regression that make use of search volume of several movie related 
search terms to predict the amount of people going to the movies. These models reach a R2 of 58% one week before
premiere and 70% one day before premiere which shall be the baseline for this repository.

### Data gathering, -preprocessing and descriptive analysis
The provided dataset contains key data for about 900 movies that premiered in Germany between 01/03/2013 and 07/07/2016. It contains 100 features of the movies like age rating, genre, studio as well as the number of ordered copies which is referring to the number of cinemas in which a movie is presented and the number of visitors of the movies on the first weekend. The number of visitors is the target which will be forecasted.

After preprocessing of the raw movie data is done in [this script](preprocessing/preprocess_movies_data.R) the Google Trends data will be collected and preprocessed. I recommend reading more on how we preprocessed the data in [this article](https://towardsdatascience.com/using-google-trends-data-to-leverage-your-predictive-model-a56635355e3d).

The order how to run the scripts to gather and preprocess the Google Trends data is

1. Collecting the Google Trends data for the anchor terms running [this script](preprocessing/collect_gt_data_anchors.R)
2. Collecting and scaling the Google Trends data for the defined search terms running [this script](preprocessing/collect_gt_data_search_terms.R)
3. Substracting the median of the time series running [this script](preprocessing/preprocess_gt_data.R)


For every movie we defined 2 to 3 search terms. The main title, the main title + the suffix film and - if the movie has a secondary title, the complete title. We built a Google Value as linear combination of the 3 search terms.
After preprocessing the Google Trends data for every search term by running the scripts in the mentioned order we need to melt the 3 search terms for each movie to a single KPI (Google Value *gv*) by applying a linear transformation. To do so we'll melt the search volumina of the main title (*mt*) and main title + "film" (*mtf*) to a single value. If the movie has a subtitle we'll take the search volume of the complete title (*ct*) into consideration in a second step. The resulting linear combination should maximize the correlation between the Google Value and the amount of visitors:

<p align="center">
<img src="https://latex.codecogs.com/svg.latex?\inline&space;\underset{a,&space;b}{maximize}&space;f(gv_{j})&space;=&space;\rho(gv_{j},&space;visitors)&space;\\&space;with&space;\hspace{0.5cm}&space;gv_{j}&space;=&space;\begin{cases}&space;a_j&space;\cdot&space;mt_{ij}&space;&plus;&space;(1&space;-&space;a_j)&space;\cdot&space;mtf_{ij}&space;&&space;\text{for&space;}&space;mt&space;=&space;ct.&space;\\&space;b_j&space;\cdot&space;\Big{(}a_j&space;\cdot&space;mt_{ij}&space;&plus;&space;(1&space;-&space;a_j)&space;\cdot&space;mtf_{ij}\Big{)}&space;&plus;&space;(1&space;-&space;b_j)&space;\cdot&space;ct_{ij}&space;&&space;\text{for&space;}&space;ct&space;\neq&space;kt.&space;\end{cases}&space;\\&space;\\&space;w.r.t&space;\hspace{0.5&space;cm}&space;0&space;<&space;a_j&space;<&space;1&space;\hspace{0.5&space;cm}&space;and&space;\hspace{0.5&space;cm}&space;0&space;<&space;b_j&space;<&space;1" title="\underset{a, b}{maximize} f(gv_{j}) = \rho(gv_{j}, visitors) \\ with \hspace{0.5cm} gv_{j} = \begin{cases} a_j \cdot mt_{ij} + (1 - a_j) \cdot mtf_{ij} & \text{for } mt = ct. \\ b_j \cdot \Big{(}a_j \cdot mt_{ij} + (1 - a_j) \cdot mtf_{ij}\Big{)} + (1 - b_j) \cdot ct_{ij} & \text{for } ct \neq kt. \end{cases} \\ \\ w.r.t \hspace{0.5 cm} 0 < a_j < 1 \hspace{0.5 cm} and \hspace{0.5 cm} 0 < b_j < 1" />
</p>

We'll do this for each week <img src="https://latex.codecogs.com/svg.latex?\inline&space;j" title="j" />. To prevent overfitting of the weights a nested resampling approach is applied and the resulting weights are averaged. To prevent values of 0, the Google Value will be [Box-Cox transformed](https://en.wikipedia.org/wiki/Power_transform#Box%E2%80%93Cox_transformation) afterwards. Optimization of the weights for the Google Value as well as their application to calculate the final KPI is done in [this script](preprocessing/calculate_google_values.R). This script also splits the data in a train and test set to prevent overfitting on the optimization results.

After the data is gathered and preprocessed, descriptive analysis is mostly done by the notebooks in [the data analysis directory](data_analysis)
