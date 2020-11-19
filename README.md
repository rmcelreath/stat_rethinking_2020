Statistical Rethinking: A Bayesian Course (with Code Examples in R/Stan/Python/Julia)
===============

Winter 2020/2021

Instructor: Richard McElreath

Format: Online, flipped instruction. The lectures are pre-recorded. We'll meet online once a week for an hour to work through the solutions to the assigned problems.

When: Wednesdays 3-4PM CET, starting 2 December 2020 (see full calendar at bottom). A Zoom link will be given to enrolled students.

Registration: Please sign up via <[EventBright](https://www.eventbrite.co.uk/e/statistical-rethinking-course-winter-20202021-tickets-126062047979)>. I've also set aside 100 audit tickets at the same link, for people who want to participate, but who don't need graded work and course credit. Apologies for using an external service, but it will make distributing the access information and course materials easier for all of us.

# Book

We'll use the 2nd edition of my book, Statistical Rethinking. I'll provide a PDF of the book to enrolled students.

# Lectures

The full lecture video playlist is here: <YouTube:[Statistical Rethinking 2019](https://www.youtube.com/playlist?list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI)>. Links to individual lectures, slides and videos are in the calendar at the very bottom. 

# Code examples

Students can engage with the material using either the original R code examples or one of several conversions to other computing environments. The conversions are not always exact, but they are rather complete. Each option is listed below.

## Original R Flavor

For those who want to use the original R code examples in the print book, you need to first install `rstan`. Go to <http://mc-stan.org/> and find the instructions for your platform. Then you can install the `rethinking` package:
```
install.packages(c("devtools","mvtnorm","loo","coda"),dependencies=TRUE)
library(devtools)
install_github("rmcelreath/rethinking")
```
The code is all on github <https://github.com/rmcelreath/rethinking/> and there are additional details about the package there, including information about using the more-up-to-date `cmdstanr` instead of `rstan` as the underlying MCMC engine.

## R + Tidyverse + ggplot2 + brms

The <[Tidyverse/brms](https://bookdown.org/content/4857/)> conversion is very high quality and complete through Chapter 14.

## Python and PyMC3

The <[Python/PyMC3](https://github.com/pymc-devs/resources/tree/master/Rethinking_2)> conversion is quite complete.

## Julia and Turing

The <[Julia/Turing](https://github.com/StatisticalRethinkingJulia)> conversion is not as complete, but is growing fast and presents the Rethinking examples in multiple Julia engines, including the great <[TuringLang](https://github.com/StatisticalRethinkingJulia/TuringModels.jl)>.

## Other

The are several other conversions. See the full list at <https://xcelab.net/rm/statistical-rethinking/>.

# Homework and solutions
I will also post problem sets and solutions here. Check the folders at the top.



# Calendar & Topical Outline

There are 10 weeks of instruction.

| Week ## | Meeting date | Reading | Lectures |
| ------- | -------------- | ------------- | ---------------------- |
| Week 01 | 02 December  | Chapters 1, 2 and 3 | The Golem of Prague <[slides](https://speakerdeck.com/rmcelreath/l01-statistical-rethinking-winter-2019)> <[video](https://www.youtube.com/watch?v=4WVelCswXo4)> <br>Garden of Forking Data <[slides](https://speakerdeck.com/rmcelreath/l02-statistical-rethinking-winter-2019)> <[video](https://www.youtube.com/watch?v=XoVtOAN0htU&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI&index=2)> |
| Week 02 | 09 December | Chapter 4 | Geocentric Models <[slides](https://speakerdeck.com/rmcelreath/l03-statistical-rethinking-winter-2019)> <[video](https://youtu.be/h5aPo5wXN8E)><br> Wiggly Orbits <[slides](https://speakerdeck.com/rmcelreath/l04-statistical-rethinking-winter-2019)> <[video](https://youtu.be/ENxTrFf9a7c)>  
| Week 03 | 06 January | Chapters 5 and 6 | Spurious Waffles <[slides](https://speakerdeck.com/rmcelreath/l05-statistical-rethinking-winter-2019)> <[video](https://www.youtube.com/watch?v=e0tO64mtYMU&index=5&list=PLDcUM9US4XdNM4Edgs7weiyIguLSToZRI)> <br>Haunted DAG <[slides](https://speakerdeck.com/rmcelreath/l06-statistical-rethinking-winter-2019)> <[video](https://youtu.be/l_7yIUqWBmE)> 
| Week 04 | 13 January | Chapter 7 | Ulysses' Compass <[slides](https://speakerdeck.com/rmcelreath/l07-statistical-rethinking-winter-2019)> <[video](https://youtu.be/0Jc6Kgw5qc0)>  <br>Model Comparison <[slides](https://speakerdeck.com/rmcelreath/l08-statistical-rethinking-winter-2019)> <[video](https://youtu.be/gjrsYDJbRh0)> 
| Week 05 | 20 January | Chapters 8 and 9 | Conditional Manatees <[slides](https://speakerdeck.com/rmcelreath/l09-statistical-rethinking-winter-2019)> <[video](https://youtu.be/QhHfo6-Bx8o)> <br>Markov Chain Monte Carlo <[slides](https://speakerdeck.com/rmcelreath/l10-statistical-rethinking-winter-2019)> <[video](https://youtu.be/v-j0UmWf3Us)>  
| Week 06 | 27 January | Chapters 10 and 11 | Maximum entropy & GLMs <[slides](https://speakerdeck.com/rmcelreath/l11-statistical-rethinking-winter-2019)> <[video](https://youtu.be/-4y4X8ELcEM)> <br>God Spiked the Integers <[slides](https://speakerdeck.com/rmcelreath/l12-statistical-rethinking-winter-2019)> <[video](https://youtu.be/hRJtKCIDTwc)>
| Week 07 | 03 February | Chapter 12 | Monsters & Mixtures <[slides](https://speakerdeck.com/rmcelreath/l13-statistical-rethinking-winter-2019)> <[video](https://youtu.be/p7g-CgGCS34)> <br>Ordered Categories, Left & Right <[slides](https://speakerdeck.com/rmcelreath/l14-statistical-rethinking-winter-2019)> <[video](https://youtu.be/zA3Jxv8LOrA)> 
| Week 08 | 10 February | Chapter 13 | Multilevel Models <[slides](https://speakerdeck.com/rmcelreath/l15-statistical-rethinking-winter-2019)> <[video](https://youtu.be/AALYPv5xSos)> <br>Multilevel Models 2 <[slides](https://speakerdeck.com/rmcelreath/l16-statistical-rethinking-winter-2019)> <[video](https://youtu.be/ZG3Oe35R5sY)>  
| Week 09 | 24 February | Chapter 14 | Adventures in Covariance <[slides](https://speakerdeck.com/rmcelreath/l17-statistical-rethinking-winter-2019)> <[video](https://youtu.be/yfXpjmWgyXU)> <br> Slopes, Instruments and Social Relations <[slides](https://speakerdeck.com/rmcelreath/l18-statistical-rethinking-winter-2019)> <[video](https://youtu.be/e5cgiAGBKzI)>
| Week 10 | 03 March | Chapter 15 | Gaussian Processes <[slides](https://speakerdeck.com/rmcelreath/l19-statistical-rethinking-winter-2019)> <[video](https://youtu.be/pwMRbt2CbSU)>  <br>Missing Values and Measurement Error <[slides](https://speakerdeck.com/rmcelreath/l20-statistical-rethinking-winter-2019)> <[video](https://youtu.be/UgLF0aLk85s)>  



