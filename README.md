# NaiveBayesClassifier

The goal of this task was the implementation of the Naive Bayes algorithm.<br>
A classification algorithm, which is supposed to make a statement about the most probable value of a given attribute based on the input data. <br>
<br>
This implementation focusses on the "Titanic" dataset, while at the same time trying to maintain some level of abstraction to keep the code reusable.<br>
<br>
In order to minimise noise in our calculations and save calculation time, our first step was to identify all those attributes for which we could safely assume that they would contain no or no relevant information regarding the survival of a person.
In our opinion, the values of the attributes {PassengerID, Embarked, Fare} are not relevant.
One could argue that the price paid for the ticket would allow us to draw conclusions about the social status of a person and that this could certainly have a significant effect on, for example, access to lifeboats.<br>
However, since, in addition to the ticket price, the cabin number and the class of travel are available as possible attributes, and since these show less variance, we decided not to include the price of the ticket, since in our opinion the combination of these two attributes is more meaningful.<br> <br>
During the preparation of the training data we were able to get an overview of how certain attributes were distributed in relation to others in terms of their frequency of occurrence. <br>

We then tried to put these differences in the frequency distribution into a meaningful context.<br>
When selecting the attributes to be considered (and thus deciding whether it could be a mere random correlation or a causal relationship), we included subsequent aspects in our considerations:<br><br>
Could <br>
* Social constructs such as masculinity/heroticity/family, the<br>
* Topos Ethics/Morals ("Women and children first" vs. "Every man for himself" ) and<br>
* the division of society into classes<br>

have had a demonstrably measurable influence in terms of correlation on whether a person survived the accident?<br><br>

If such a correlation could be plausibly constructed for us, we included it in the list of attributes to be considered.<br>
Our first approach - ignoring data sets with missing attribute values - resulted in the loss of about three-quarters of the training data and was therefore immediately discarded.<br>
We then replaced missing numerical attribute values with average values calculated from the available data.<br>
However, during the categorisation process it turned out that a strictly binary classification (e.g. that of the attribute "age" in <= avg_age and "age" > avg_age) was less accurate than the classification listed below:
* child→ age [0; 12],
* adolescent→ age(12, 18],
* adult→ age(18, 67],
* senior→ age(67, ∞ ]

This classification seemed to make more sense, as it seemed to reflect the fact that children survived more often than all other age groups. In the course of these considerations, we also decided to include data on the number of family members and relatives on board, as it seemed plausible to us that families would try to get into lifeboats together. 

We divided both attributes (parch, sibSp) into the following categories:<br>
* none→ parch or sibSp [0; 0],
* oneToTwo→ parch or sibSp [1; 2],
* moreThanTwo→ parch or sibSp [3; ∞]

The third category seemed to make sense to us because we thought it likely that smaller groups would get lost less often than larger ones and that smaller groups would have an advantage over larger ones in arguing who else should be allowed on a lifeboat (this is of course a heuristic).
In fact, the various tests showed that a combination of the attributes {sex, pclass} provided the most reliable predictions resulting in a KaggleScore of 0.775%.

Translated with www.DeepL.com/Translator (free version)
