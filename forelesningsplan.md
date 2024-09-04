# Course Plan SOK-3020 Econometrics Fall 2024

| Session  | Date | To do                                            |
|------------------|---------|------------------------------------------------------------------|
| *Preparation*    |         | We use the open-source programming software R, along with its graphical user interface, RStudio, for all the estimations. We highly recommend you download these onto your personal computer and spend some time acquainting yourself with the RStudio interface. Download R from [here](https://cloud.r-project.org/){:target="blank"}, and RStudio from [here](https://posit.co/download/rstudio-desktop/){:target="blank"}. Alternatively, you can access R through the Jupyter notebook interface using your web-browser [here](https://jupyter.uit.no){:target="blank"}. Use your UiT username and password.   |
|                  |          | If you need to sharpen your R skills, you can search Youtube for "learning r". Google has some introductory videos [here](https://www.youtube.com/playlist?list=PLOU2XLYxmsIK9qQfztXeybpHvru-TrqAP){:target='blank'}. Follow this link for a bunch of [Cheatsheets](https://posit.co/resources/cheatsheets/){:target='blank'}. Note the [RStudio IDE :: Cheatsheet](https://rstudio.github.io/cheatsheets/html/rstudio-ide.html){:target='blank'} and the [Data Transformation with dplyr](https://rstudio.github.io/cheatsheets/html/data-transformation.html) cheatsheet. Our textbook is: [Principles of Econometrics](http://principlesofeconometrics.com/poe5/poe5.html){:target='blank'}, 5th Edition (POE5), Wiley - 2018. Available at the campus bookstore Akademika. Buy the book! You will not manage the course without it. |
| **Resources** |            | The 4th edition of our textbook has an [online version with R code](https://bookdown.org/ccolonescu/RPoE4/){:target='_blank_'}. There is an online book called [Introduction to econometrics with R](https://www.econometrics-with-r.org/index.html){:target='_blank_'}. Another online book is [Using R for Introductory Econometrics](http://urfie.net){:target='_blank_'}. Also available for Python and Julia. When you are on the UiT network, you have access to [Applied Econometrics with R](https://link.springer.com/book/10.1007%2F978-0-387-77318-6){:target='_blank_'}. They have a [web-page](https://eeecon.uibk.ac.at/~zeileis/teaching/AER/){:target='_blank_'}. Springer has a whole series of [books in the Use R! section](https://link.springer.com/search?facet-series=%226991%22&facet-content-type=%22Book%22){:target='_blank_'}. |
|                  |         | The ultimate resource for online books on R is [The big book of R](https://www.bigbookofr.com/){:target='_blank_'}. |
| **Self Study** |     |  Probability Primer, POE5 p. 15-39 |
|   |   |  Example P.1 Using a cdf  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p1%20using%20a%20cdf.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=20d5f48a-e081-4ce0-b024-b0610078f235){:target='_blank_'}  |
|   |   | Example P.2 Calculating a conditional probability [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p2%20calculating%20a%20conditional%20probability.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ae78fd28-9635-41fe-a3a9-b0610078f247){:target='_blank_'}  [Intuition behind (video)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=79a7b549-d8d7-4230-87fd-b0610078f23f){:target='_blank_'}  |  
|   |   | Example P.3 Calculating an expected value  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p3%20calculating%20an%20expected%20value.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=32f60538-9af0-4ea4-89f3-b0610078f0d9){:target='_blank_'}   |  
|   |   | Example P.4 Calculating a Conditional Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p4%20calculating%20a%20conditional%20expectation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=7f00a56e-92b0-4c21-8073-b0610078f577){:target='_blank_'}   |  
|   |   | Example P.5 Calculating a Variance  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p5%20calculating%20a%20variance.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=d75b1ca0-b337-4438-b4b5-b0610078f64c){:target='_blank_'}   |  
|   |   | Example P.6 Calculating a Correlation [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p6%20calculating%20a%20correlation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=64831e5e-8ca8-41db-b18b-b0610078f747){:target='_blank_'} [Correlation is a standardized covariance (video)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=068b0fb1-4f62-49c6-86f7-b0610078f56e){:target='_blank_'}   |  
|   |   | Example P.7 Conditional Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p7%20conditional%20expectation.R){:target='_blank_'}     |  
|   |   | Example P.8 Conditional Variance  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p8%20conditional%20variance.R){:target='_blank_'}     |  
|   |   | Example P.9 Iterated Expectation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p9%20iterated%20expectation.R){:target='_blank_'}     |  
|   |   | Example P.10 Covariance Decomposition  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p10%20covariance%20decomposition.R){:target='_blank_'}   |  
|   |   | Example P.11 Normal Distribution Probability Calculation  [Rcode](https://raw.githubusercontent.com/uit-sok-3020-h23/uit-sok-3020-h23.github.io/main/example_p11-normal-distribution-probability-calculation.R){:target='_blank_'}  [video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=ed10162c-003a-441f-bc7f-b06200d77a64){:target='_blank_'}   |  
| 1  |   15/8 | Introduction to R|
|    |       |   Download and [install R (first) then RStudio](https://posit.co/download/rstudio-desktop/){:target='_blank_'} |
|    |       |  [RStudio IDE :: Cheatsheet](https://rstudio.github.io/cheatsheets/html/rstudio-ide.html){:target='_blank_'}   |
|    |       |  [Data files from Principles of Econometrics](https://www.principlesofeconometrics.com/poe5/poe5data.html){:target='blank'}       |
|    |       |  [my first R file](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/my%20first%20r%20file.R){:target='_blank_'} |
| 2  | 16/8      | [my first R exercise](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/my_first_exercise.R){:target='_blank_'}  |
| 3  | 16/8    |  [Intro to R for Econometrics](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/intro%20to%20R%20for%20econometrics.qmd){:target='_blank_'} |
|    | | Browse the paper "Econometric Computing with 'R'" (in Files in Canvas) |
|    |  | Relating to our wages example in class, there is a [report out from SSB](https://www.ssb.no/inntekt-og-forbruk/inntekt-og-formue/artikler/forskjeller-i-livslopsinntekt-mellom-utdanningsgrupper/_/attachment/inline/fbc61677-a679-4edd-93d8-c6095b866d0d:138a258371f5d983314e8175b97bb5ed5adc04e3/RAPP2023-26.pdf){:target='_blank_'} that looks at lifetime income in different professions (in Norwegian) |
| 4  | 19/8  | Appendix A [Mathematical Tools in R](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/Appendix_A_annotated.R){:target='_blank_'} ["old" video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e39c6666f51844efa636aee500c51b7a){:target='_blank_'}    |
| 5  | 21/8 | Chapter 1. An introduction to econometrics  [pdf in Canvas/Files] [Quarto](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/chap1_why_study_econometrics.qmd){:target='_blank_'} |
| 6  | 26/8 |  Chapter 2 - The Simple Linear Regression Model |
|    |     |   [R code Chapter 2](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/R_code_Chapter_2.R){:target='_blank_'}  |
|    |     |   ["old" video (Part 1)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=8cafa645-c30b-465a-a992-aee500c4723a){:target='_blank_'}     |
|    |     |   ["old" video (Part 2)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=fe2607de-2c56-492f-87bd-aee500c57671){:target='_blank_'}  |
| 7 | 28/8 | Continue on Chapter 2 |
| 8 | 28/8 | Chapter 3 - Interval Estimation and Hypothesis Testing   |
|    |      |  [R code for Chapter 3](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/Chapter_3.R){:target='_blank_'}    |
|    |      |   ["old" video (Part 1)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=e319d74e-c548-4de3-b6f5-aee500c4d176){:target='_blank_'}     |
|    |      |   ["old" video (Part 2)](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=4d290fdc-36ae-4d2d-8495-aee500c57827){:target='_blank_'}     |
| 9 | 2/9  | Continue on Chapter 3 |
| 10 | 4/9 | Chapter 2 and 3 using R code |
|11  |9/9  | Chapter 4 - Prediction, Goodness-of-Fit, and Modeling Issues   |
|    |      | ["old" video](https://uit.cloud.panopto.eu/Panopto/Pages/Viewer.aspx?id=befb571c-77a5-4919-b3e5-aee500c4b463){:target='_blank_'} |
|    |      |  [Quarto R file for Chapter 4](https://raw.githubusercontent.com/uit-sok-3020-h24/uit-sok-3020-h24.github.io/main/Chapter_4.qmd){:target='_blank_'} | 
| 12 | 10/9  | Continue on Chapter 4 |
| 13 | 10/9  |  |
| 14 | 12/9  |  |






