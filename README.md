## A local state port of re-frame 1.2.0

I wanted to use devcards with re-frame. It could be useful for other people too.

This fork allows to run multiple instances of the same application in the same window. It helps if you want to run a bunch of tests and functional tests showing the whole application in different containers in the same window where your test are being executed. For cases like that it just makes stuff simpler.

Check `examples/todomvc-with-state` for a running example of the `todomvc` running two applications at the same time.

Many thanks to all the contributors that keeps our dependencies up and running!

[![Clojars Project](https://img.shields.io/clojars/v/jfigueroama/re-frame.svg)](https://clojars.org/jfigueroama/re-frame)

```clojureScript
(ns reframelib-sample.core
  (:require [reagent.core :as reagent]
            [reagent.ratom :as ratom]
            [devcards.core :as dc]
            [devtools.core :as devtools]

            [re-frame-lib.core
             :refer [new-state subscribe dispatch reg-sub reg-event-db reg-event-fx]])
  (:require-macros [devcards.core :as dc :refer [defcard deftest defcard-rg]]))

(defn add-handlers
  [state]
  (-> state
      (reg-sub :db (fn db-sub [db _] db))
      (reg-event-db :init (fn init-cmd [db _] {:start 1 :end 10 :current 1 :running false}))
      (reg-event-fx
        :start-step
        (fn start-step
          [{db :db} [_ current end]]
          (if (> current end)
            {:db (assoc db :running false)}
            (let [next (inc current)
                  long-calc (reduce + (range 10000000))]
              {:db (assoc db :current next)
               :dispatch-later [{:ms 50 :dispatch [:start-step next end]}]}))))
      (reg-event-fx
        :start
        (fn start-cmd
          [{db :db} _]
          {:db (-> db
                   (assoc :running true))
           :dispatch [:start-step (:start db) (:end db)]}))))

(defonce s1 (add-handlers (new-state)))
(dispatch s1 [:init])
(defonce s2 (add-handlers (new-state)))
(dispatch s2 [:init])

(defn monitor-ui
  [state owner]
  (let [db (subscribe @state [:db])]
    [:div
     [:span "Running: " (str (:running @db))] [:br]
     [:span "Start: " (:start @db)] [:br]
     [:span "End: " (:end @db)] [:br]
     [:span "Current: " (:current @db)] [:br] [:br]
     [:button {:on-click #(dispatch @state [:start])} "Start"]]))

(defcard-rg re-frame-lib-c1
  monitor-ui
  (atom s1))

(defcard-rg re-frame-lib-c2
  monitor-ui
  (atom s2))


(defcard "State 1" (:app-db s1))
(defcard "State 2" (:app-db s2))
```

To test an entire event flow you can use `re-frame-lib.async-test-flow`.
Look at `src/re_frame_lib/async_test_flow.cljs` and `test/re_frame_lib/async_test_flow_test.cljs`:

```clojure
(ns re-frame-lib.async-test-flow-test
  (:require [cljs.test :refer [deftest testing is are]]
            [re-frame-lib.core :refer [new-state reg-event-db dispatch]]
            [re-frame-lib.async-test-flow :refer [run-test-flow]]))

(deftest runs-sucessfully
  (let [state (-> (new-state)
                  (reg-event-db :init  (fn init-evt [db _] (assoc db :value 0)))
                  (reg-event-db :inc   (fn inc-evt [db _] (update db :value inc))))]
    (async done
      (run-test-flow
        state
        [[:init]  ; direct dispatch and implicit testing that we endup with a different db
         {:dispatch [:inc]
          :test     (fn [db] (= 1 (:value db)))}
         {:dispatch (fn [app-state]
                      (dispatch app-state [:inc]))
          :test     (fn [db] (= 2 (:value db)))}]
        {:done         done
         :test-timeout 3000}))))
```

<p align="center"><a href="https://day8.github.io/re-frame" target="_blank" rel="noopener noreferrer"><img src="docs/images/logo/re-frame-colour.png?raw=true" alt="re-frame logo"></a></p>

## Derived Values, Flowing

> This, milord, is my family's axe. We have owned it for almost nine hundred years, see. Of course,
sometimes it needed a new blade. And sometimes it has required a new handle, new designs on the
metalwork, a little refreshing of the ornamentation ... but is this not the nine hundred-year-old
axe of my family? And because it has changed gently over time, it is still a pretty good axe,
y'know. Pretty good.

> -- Terry Pratchett, The Fifth Elephant <br>
> &nbsp;&nbsp;&nbsp; reflecting on identity, flow and derived values  (aka [The Ship of Theseus](https://en.wikipedia.org/wiki/Ship_of_Theseus))
<br/> 
<br/>

<!--
[![CI](https://github.com/day8/re-frame/workflows/ci/badge.svg)](https://github.com/day8/re-frame/actions?workflow=ci)
[![CD](https://github.com/day8/re-frame/workflows/cd/badge.svg)](https://github.com/day8/re-frame/actions?workflow=cd)
[![License](https://img.shields.io/github/license/day8/re-frame.svg)](license.txt)
-->

## Overview

re-frame is a ClojureScript framework for building user interfaces.
It has a data-oriented, functional design. Its primary focus is on high programmer productivity and scaling up to larger Single-Page applications.

Developed in late 2014, and released in 2015, it is mature and stable. It is used by both small startups and companies with over 500 developers, and it has delivered into production applications which are 40K lines of code and beyond. 

Across the last 6 years, it has outlasted multiple generations of Javascript churn - just imagine your team's productivity if you didn't have to contend with technical churn, and have new magic burn your fingers every two years. Brand new, exciting concepts like recoiljs (in the React world), have been a regular part of re-frame from the beginning. 

re-frame is lucky enough to enjoy an unfair advantage - ClojureScript is a Lisp. Alan Kay
once described Lisp as "Maxwell's equations of software". Paul Graham 
described how Lisp was a competitive advantage for his startup.  When we use Lisp, we 
get to leverage 50 years of foliated excellence from the very best minds available.
And then there's also a thriving ClojureScript community which delivers modern ideas and best-in-class tooling.

Although re-frame leverages React (via Reagent), it only needs 
React to be the V in MVC, and no more. re-frame takes a different road to the currently-pervasive idea that Views should be causal (colocated queries, ComponentDidMount, hooks, etc).
In re-frame, events are causal, and views are purely reactive. 

## Documentation 

The re-frame documentation is [available here](https://day8.github.io/re-frame/).

## The Current Version 

[![Clojars Project](https://img.shields.io/clojars/v/re-frame?labelColor=283C67&color=729AD1&style=for-the-badge&logo=clojure&logoColor=fff)](https://clojars.org/re-frame)

For full dependency information, see the [Clojars page](https://clojars.org/re-frame/)

## Getting Help 

[![Get help on Slack](http://img.shields.io/badge/slack-clojurians%20%23re--frame-97C93C?labelColor=283C67&logo=slack&style=for-the-badge)](https://clojurians.slack.com/channels/re-frame)

## Licence

re-frame is [MIT licenced](license.txt)
