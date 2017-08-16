# adjustPhenotypes

This is a small package that offers different ways to adjust phenotypes.  There a three "mean adjusting" functions:
   *allcorrect (subtract the mean of all plants in chamber from each obs)
   *phytcorrect (subtract the mean of all phytometers in chamber from each obs)
   *colcorrect (subtract the mean of all COL replicates in chamber from each obs)

There is also a "scaling" function:
   *scalePhenos (this one adjusts observations by dividing by the within chamber stddev)


If the data are piped through allcorrect() and then scalePhenos(),
this is equivalent to producing z-scores based on chamber means and
sd.  If the data are piped through colcorrect() and then
scalePhenos(), a *z-score-like* statistic is produced where the
population means are replaced by COL means.  Similarly when
phytcorrect() and scalePhenos() is used, the population means are
replaced by phytometer means.
