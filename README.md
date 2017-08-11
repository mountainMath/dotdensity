# dotdensity
This package provides several convenience function for dot-density maps. Dot-density maps are fairly straight-forward but there are a couple of pitfalls to pay attention to. This package attempts to
deal with some of these, in particular

1. Scaling to more than 1 dot per unit (e.g. person)
2. Re-aggregating through hierarchical geographic levels

##Scaling
Dot-density maps can get very messy if we draw one dot of each person (or whatever the unit is). Scaling, by
only drawing 1 dot for every *n* people is a good way to alleviate this. But this process can introduce bias. Take
the example where we draw 1 dot for every 50 people of a category, say German speakers. Suppose German speakers
are distributed uniformly, so that each geographic area has 50 people, 20 of which are German speakers. Standard rounding will produce a
map without any German speakers, severely misrepresenting the data. To remedy this we should use a rounding algorithm that is unbiased. Statistical rounding, just like what is employed in the census, will produce unbiased results. So our convenience method that converts counts for categories (e.g. languages) in each geographic regions to dots does exactly that. We also shuffle the order of the dots to ensure that not all people of a certain category end up getting visually over-represented by drawing on top of all others.

##Re-Aggregating
Census data a fine geographies in general under-counts the total number of people. We can deal with this by re-distributing the missing number of people found at a higher geographic aggregation level to lower levels. This will introduce some problems because we won't know exactly where to distribute the missing people, but it will ensure that the overall number of dots drawn accurately reflect overall counts. Typically we will weight the re-distribution by population, but other possibilities exist.

Furthermore, the same method allows the geographic weighting of data to fine block-level population data, where
we don't have general census variables available. Again, this kind of weighted re-distribution emphasise the problem with dot-density maps that they suggest an accuracy that does not exist, but on the other hand it produces very accurate representations of other aspects of the data, like general population density.

##Examples
This package has been designed to be used in conjunction with [cancensus](https://github.com/mountainMath/cancensus) to pull in census data, but it also works with other data.

The following example iamge was created using `cancensus` and `dotdensity`, you can find more details by reading throuhg our [language example vignette](http://htmlpreview.github.io/?https://raw.githubusercontent.com/mountainMath/dotdensity/master/vignettes/language-example.nb.html) for a more thorough look, or you can [view the code](vignettes/language-example.Rmd).

![Vancouver languages](vancouver-languages.png)
