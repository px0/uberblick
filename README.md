# What's this?
This gives you a neat-o overview of all the Klicksters, that you can filter
absolutely arbitrarily. Want only people with even userids? No problemo! People 
that report to you that just left the 3rd floor? You got it! Check it out, it's 
really cool!


# Where can I try it?
There is a live version of this here: [http://overview.px0.de/](http://overview.px0.de/)

# Development
You can find the source code for this project at 
[https://github.com/px0/uberblick](https://github.com/px0/uberblick).
This also uses my Genome ClojureScript wrapper which you can find at 
[https://github.com/px0/genome-cljs](https://github.com/px0/genome-cljs).

The project is written in
[ClojureScript](https://github.com/clojure/clojurescript) and uses
[Reagent](http://reagent-project.github.io/), a ReactJS wrapper, as
the frontend technology. The frontend framework is
[MaterializeCSS](http://materializecss.com/).

The fanciest thing in this whole project is that it is using
[Bootstrapped ClojureScript](http://swannodette.github.io/2015/07/29/clojurescript-17/).
I have been mostly following the instructions laid out
[in this article](http://yogthos.net/posts/2015-11-12-ClojureScript-Eval.html)

## Run in Dev mode
`lein figwheel` and connect to [http://localhost:3449](http://localhost:3449)

## Create a production build
Run this to build the project in a production config: `lein clean && lein with-profile prod cljsbuild once`

# Moar
Here is some other stuff I made for Klick: [http://klick.px0.de](http://klick.px0.de)
