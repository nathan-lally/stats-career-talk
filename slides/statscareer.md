<style>
.centertext {
    color: black;
    font-size: 200%;
    background: #E8E8E8;
    position: fixed;
    top: 50%;
    text-align: center;
    width:100%;
}
.header {
    color: black;
    background: #E8E8E8;
    position: static;
    top: 10%;
    text-align: left;
    width:100%;
}
.header2 {
    color: black;
    background: #E8E8E8;
    position: static;
    text-align: left;
    width:100%;
}
.footer{
    color: red;
    background: #E8E8E8;
    position: fixed;
    top: 90%;
    text-align: center;
    width:100%;
}
</style>

What Do Statisticians Actually Do In The Real World?
========================================================
author: Nathan Lally: Senior Machine Learning Modeler @ HSB - Munich Re
width: 1366
height: 768
date: 2018-10-29
autosize: true


So What Exactly Is Statistics?
========================================================
<div class="centertext">You're in a statistics class you tell me first!</div>


So What Exactly Is Statistics?
========================================================
<div class="header">Start with the definition. Boring dude...</div>

__Statistics:__ Is a branch of applied mathematics concerned with the collection, organization, analysis, interpretation and presentation of data.

$$ \pi(\theta|X) = \frac{\pi(X|\theta)\pi(\theta)}{\int \pi(X|\theta)\pi(\theta) d\theta} $$

<div class="footer">This is a pretty broad definition. Why is this interesting or relevant???</div>

So What Exactly Is Statistics?
========================================================
<div class="header">The language of scientific experimentation</div>

Statistics is the tool we use to validate the results of all applied sciences including but not limited to,

* Physics
* Chemistry
* Biology
* Engineering
* Computer Science
* Medicine, Public Health & Epidemiology
* Education & Testing

When you have collected data about a phenomenon you can use statistics to understand it.


***

<div class="header">Statistics in experimental physics</div>

<center>
![The LHC](lhc.jpg)
<center>

[Statistics was used to validate the discovery of the Higgs boson.](http://iopscience.iop.org/article/10.1088/1742-6596/510/1/012001/pdf)

So What Exactly Is Statistics?
========================================================
<div class="header">At the forefront of technological innovation</div>

In addition, probability and statistics makeup the backbone of machine learning (ML) and artificial intelligence (AI) algorithms.

Statistics is the core of the new discipline of data science. We will talk about data science later and see a demo or ML in action.

***

<div class="header">Machine Learning & Artificial Intelligence</div>

<center>
![A humble T-800](arnold.jpg)
<center>


So What Exactly Is Statistics?
========================================================
<div class="header">According to Wikipedia...</div>

"Two main statistical methods are used in data analysis:"

* __Descriptive Statistics__
  * "Summarize data from a sample using indexes such as the mean or standard deviation"
  * Includes numerical summaries and statistical graphics
  
* __Inferential Statistics__
  * "Draw conclusions from data that are subject to random variation"
  * Includes hypothesis testing and other probabilistic assessments

***

<div class="header">Descriptive Statistics</div>

<img src="statscareer-figure/descstats-1.png" title="plot of chunk descstats" alt="plot of chunk descstats" style="display: block; margin: auto;" />


<div class="header2">Inferential Statistics</div>

$$
\small
\begin{align}
  &H_0: \mu_{B1} = \mu_{B10}\\
  &H_a: \mu_{B1} < \mu_{B10}\\ \hline
  &\bar{X}_{B1}= 45.38, \bar{X}_{B10}= 47.39, s_p = 14.07\\
  &n_{B1}=163125, n_{B10}=17740\\
  &t = \frac{\bar{X}_{B1} - \bar{X}_{B10}}{s_p\sqrt{\frac{1}{n_{B1}}+ \frac{1}{n_{B10}}}} = -18.11\\
  &P(T \leq t | H_0) \approx 2.2 \cdot 10^{-16}
\end{align}
$$



So What Exactly Is Statistics?
========================================================
<div class="header">My perspective</div>

Descriptive statistics, while important in their own right, are almost always a precursor to a more thorough statistical analysis. Typically, statisticians are concerned with two things,

* Make Inference
  * Hypothesis testing and other probabilistic assessments
  * Discover relationships in the data and quantify them


* Make Predictions
  * Construct a model that can accurately predict an unobserved or future event
  * Prediction is the primary goal of machine learning and AI and a major part of the work of a data scientist

Together, inference and prediction help enable decision making in science, government, business and more.

Statistics In Action
========================================================
<div class="header">Statistical Inference: An Example</div>

Show contigency table for college admissions and hypothesis test

Statistics In Action
========================================================
<div class="header">Predictive Modeling: An Example</div>

Either a

Now That We Understand Statistics, What Is Data Science?
========================================================
<div class="header">Data Science: A Multidisciplinary Science With Statistics At Its Core</div>



Statistics & Data Science In Action
========================================================
<div class="header">Machine learning, AI and you</div>

<div class="centertext">Do you think you have ever used a product that uses ML or AI?</div>


Statistics & Data Science In Action
========================================================
<div class="header">Machine learning, AI and you</div>

You have. You use them all the time. Here are some examples,

* Google Search: [Pagerank](https://en.wikipedia.org/wiki/PageRank)
* Text Auto-Correct & Suggestions
* Siri, Alexa, & friends
* Amazon Product Recommendations
* Netflix Scores
* Facebook Facial Recognition
* Uber Driver Routing


***

<div class="header">Google search uses math not magic</div>

<center>
![Google Page Rank Algorithm](pagerank.jpg)
<center>


What Do I Do As A Statistician/Data Scientist?
========================================================
<div class="header">Where I have worked</div>

1. The Hartford Insurance Group
2. Pratt & Whitney
3. The Hartford Steam Boiler (Munich Re)

<center>
![F-35 JSF](f35.jpg)
<center>


***

<div class="header">Just some of my projects</div>

1. Predict electric utility damage in hurricanes
2. Auto insurance customer retention & pricing
3. Insurance reserve estimation
4. Jet engine combustor section wear and fatigue modeling
5. Jet engine service and part replacement forecasting
6. F-35 Joint Strike Fighter engine vibration analysis
7. Commercial equipment breakdown pricing
8. Location risk for commercial equipment insurance




What Do I Have To Do To Be A Statistician/Data Scientist???
========================================================
<div class="header">In case you thought you were done studying soon...</div>

1. Complete an undergraduate degree with a strong quantitative focus
  * Statistics, Mathematics, Computer Science, Physics, Electrical Engineering, Economics (focus on econometrics)
  * These degrees can help get you an entry level role in the private or public sector
2. Complete a graduate degree
  * PhD or MS in Statisics or a related field with relevant coursework and research in statistics
  * This is essential if your goal is to work in academics or in a research position. It also will help you in  industry jobs if you want a to be a technical lead or decision maker.
3. Learn to code
  * R , Python, Julia, Scala, SQL
4. Study more math
5. Did I say study more math? Do that some more...

Why Become A Statistican or Data Scientist?
========================================================
<div class="header">Save the world!</div>

add median for everyone in the USA

***

<div class="header">Get paid well while you're at it!</div>

* Statistician Salaries ([Median](http://www.amstat.org/ASA/Your-Career/Salary-Information.aspx))
  * Academic
      1. Assistant Professor: $92,004
      2. Associate Professor: $102,142
      3. Professor: $148,368
  * Industry
      1. Similar to data scientist salaries but with more variability due to diverse career options

* Data Science Salaries ([Median](https://datasciencedegree.wisconsin.edu/data-science/data-scientist-salary/))
  1. Entry-Level: $95,000
  2. Mid-Level: $126,000
  3. Experienced: $157,000


