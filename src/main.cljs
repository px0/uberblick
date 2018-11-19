(ns main
  (:require
            [cljs.core.async :as async :refer [<! >! chan take! timeout]]
            [cljs-http.client :as http]
            [genome]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [markdown.core :as markdown]
            [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [clojure.walk :as walk]
            [cljs.pprint :as pprint]
            [alandipert.storage-atom :refer [local-storage]]
            ["alasql" :as alasql]
            )
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:import goog.History))

;; -------------------------
;; State

(defonce people-atom (local-storage (atom []) :people))
(defonce filtered-people-atom (atom []))
(defonce sorted-people-atom (atom {}))
;; (defonce filters (local-storage (atom ["(starts-with? :KeyscanStatus \"In\")"]) :filters))

; (take! (genome/<get-all-active-klickster-profiles) (fn [profiles] (->> (apply merge profiles ) keys prn)))

(def filter-people identity) ;todo
;; -------------------------
;; Actions

(defn jsatom [atm]
  (clj->js @atm))

(defn printatom [atm]
  (.log js/console (.stringify js/JSON (jsatom atm) nil 2)))

;; Network



  ;; BusinessUnitName: Department
  ;; WorkTeam: Your actual team

(defn nil->Out [people]
  (map (fn [peep]
         (let [location (:KeyscanStatus peep)]
           (assoc peep :KeyscanStatus (or location "Out!"))))
       people))

(defn set-people-to-all-active-klicksters [atm interval]
  (go-loop []
    (let [people (->> (<! (genome/<get-all-active-klickster-profiles))
                     nil->Out
                     (sort-by :FirstName))]
      (when (> (count people) 0)
        (do
          (reset! atm (filter-people people))
          (println "Downloaded" (count @atm) "Klicksters"))))
    (<! (timeout interval))
    (recur)))




;; -------------------------
;; Views

(defn prettify [data]
  (with-out-str (cljs.pprint/pprint data)))

(defn PersonCard [user]
  [:div.z-depth-1.tooltip {:style {:max-width 100
                                   :margin 5
                                   :text-align :center}}
   [:span.tooltiptext [:pre {:style {:text-align :left}} (prettify user)]]
   [:div {:style {:width 100
                  :height 100
                  :background-image (str "url(" (:PhotoPath user) ")")
                  :background-size "100% auto"
                  :background-repeat :no-repeat}
          :on-click #(.open js/window (genome/profile-link user))}]
   [:span {:style {:width "100%"
                   :font-size 16}}
    (:FirstName user)]])

(defn Floor [title users]
  [:div
   [:h3 title " (" (str (count users)) ")"]
   [:hr]
   [:div {:style {:display :flex
                  :flex-direction :row
                  :flex-wrap :wrap}}
    (for [u users]
       ^{:key (:UserID u)} [PersonCard u])]])

(defn home-page []
  (try
    [:div
     (let [people (->> @people-atom (filter #(< (:UserID %) 5000)))]
       (cond
         (nil? people) [:h2 "There has been an error while applying your filters!"]
         (= 0 (count people)) [:h2 "No one that fits your filters is in!"]
         :else (for [[floor people] people]
                 ^{:key floor} [Floor floor people])))]
    (catch :default e
      (prn e)
      [:h2 "There has been an error while applying your filters!"])))

(defn StalkyPersonCard [user]
  [:div.z-depth-1.tooltip {:style {:max-width 100
                                   :margin 5
                                   :text-align :center}}
   [:span.tooltiptext [:pre {:style {:text-align :left}} (prettify user)]]
   [:div {:style {:width 100
                  :height 100
                  :background-image (str "url(" (:PhotoPath user) ")")
                  :background-size "100% auto"
                  :background-repeat :no-repeat}
          :on-click #(.open js/window (genome/profile-link user))}]
   [:div {:style {:width "100%" :font-size 16}} (:FirstName user)]
   [:div {:style {:width "100%" :margin-top -5 :font-size 16 :font-weight :bold}} (:KeyscanStatus user)]
   ])

(defn Stalky []
  (try
    [:div {:style {:display :flex
                   :flex-direction :row
                   :flex-wrap :wrap}}
     (let [people (->> @people-atom (filter #(< (:UserID %) 5000)))]
       (cond
         (nil? people) [:h2 "There has been an error while applying your filters!"]
         (= 0 (count people)) [:h2 "No one that fits your filters is in!"]
         :else (for [dude people]
                    ^{:key (:UserID dude)} [StalkyPersonCard dude])))]
    (catch :default e
      (prn e)
      [:h2 "There has been an error while applying your filters!"])))

(defn About
"Take the readme, render it to HTML, and set it as the element!"
  []
  (let [readme (atom "")]
    (take! (http/get "/README.md")
           (fn [val]
             (let [content-md (-> val
                               :body
                               markdown/md->html)]
               (prn val)
               (reset! readme content-md))))
    (fn []
      [:div {:dangerouslySetInnerHTML {:__html @readme}}])))


;; (defn Instructions
;;   [current-profile]
;;   [:div
;;    [:p "Here is a list of all available attributes:"
;;     [:p (str 'all-profile-keys)]
;;     "And for the record, here is your profile:"]
;;    [:pre @current-profile]
;;    [:p "If you want to see the Clojure functions that you can use check out "
;;     [:a {:href "http://cljs.info/cheatsheet/"} "this cheatsheet"]
;;     " and this "
;;     [:a {:href "http://kanaka.github.io/clojurescript/web/synonym.html"} " comparison to JavaScript"]]])

;; (defn New-Filter-Field [current-profile inputatm]
;;   [:div.row
;;    [:input.col.s9.offset-s1 {:type "text"
;;                              :placeholder "Add a new filter"
;;                              :value @inputatm
;;                              :on-change #(reset! inputatm (-> % .-target .-value))}]
;;    [:button.col.s1.btn {:on-click #(do
;;                                      (let [success (add-filter @current-profile @inputatm)]
;;                                        (if success
;;                                          (reset! inputatm "")
;;                                          (js/alert "There has been a problem with your filter!")))
;;                                      )} "Add"]])

;; (defn Current-Filters [inputatm]
;;   [:ul.collection
;;    (map-indexed (fn [idx filter]
;;                   ^{:key filter}
;;                   [:li.collection-item (str filter)
;;                    [:div.secondary-content
;;                     [:a {:on-click #(do
;;                                       (prn filter)
;;                                       (reset! inputatm (str filter))
;;                                       (remove-filter idx))}
;;                      [:i.material-icons "edit"]]
;;                     [:a {:on-click #(remove-filter idx)} [:i.material-icons "delete"]]
;;                     ]]) @filters)])

;; (defn Examples [current-profile filter-examples]
;;   [:ul.collection
;;    (map-indexed (fn [idx filter]
;;                   ^{:key filter}
;;                   [:li.collection-item filter
;;                    [:a.secondary-content {:on-click #(add-filter @current-profile filter)}
;;                     [:i.material-icons "playlist_add"]]])
;;                 filter-examples)])

;; (defn Filters []
;;   (let [inputatm (atom "")
;;         current-profile (atom nil)
;;         current-profile-str (atom "")]
;;     (go
;;       (let [profile (reset! current-profile (<! (genome/<current-user-profile)))]
;;         (reset! current-profile-str (-> profile prettify ))))
;;     (fn []
;;       [:div
;;        [:h3 "Filters"]
;;        [Current-Filters inputatm]
;;        [New-Filter-Field current-profile inputatm]
;;        [:hr]
;;        [:h3 "Introduction"]
;;        [:p "Here you can define filters on the collection of Klicksters. They
;;        will typically have the form (command parameter1 parameter2) - yes,
;;        including the parentheses, they are imporant - but they can be
;;        arbitrarily complex. Profile attributes can be referred to by
;;        keywords (anything starting with a ':'), and attributes starting with a
;;        '$' will automaticaly be replaced by attributes of *your* profile. I
;;        know, that sounds confusing, but check out the examples below and play
;;        around for a bit and it'll all make sense!"]
;;        [:hr]
;;        [:h3 "Examples"]
;;        [:p "Here are some example filters to get you started!"]
;;        [Examples current-profile [
;;                                   "(starts-with? :FirstName \"M\")"
;;                                   "(= :LaborRoleID \"APPLDEVL\")"
;;                                   "(not (= :LaborCategoryID \"PROJMGMT\"))"
;;                                   "(= :WorkTeamID $WorkTeamID) ; Klicksters in your team "
;;                                   "(substring? :Title \"Quality\") ; Klicksters that have 'Quality' in their title"
;;                                   "(> :UserID $UserID) ; Klicksters that got hired after you"
;;                                   "(= :KeyscanStatus \"In 4th Flr N\")"
;;                                   "(or (starts-with? :FirstName \"M\") (substring? :Title \"Edit\"))"
;;                                   "(starts-with? :KeyscanStatus \"In\")"
;;                                   "(includes? :Supervisors $UserID) ; Klicksters who report to you"
;;                                   ]]
;;        [:p "Note: Anything after ';' is a comment. If you want to learn some more about this syntax check out "
;;         [:a {:href "http://learnxinyminutes.com/docs/clojure/"} "this quick introduction to Clojure"]
;;         " and the links at the bottom of this page!"]
;;        [:hr]
;;        [:h3 "References"]
;;        [Instructions current-profile-str]])))

(defn authenticate
  ([]
   (let [token (js/prompt "What is your Genome auth token?")]
     (-> token
         ((fn [t] (if (empty? t) nil t)))
         (authenticate))))
  ([token]
   (prn 'token token)
   (genome/set-auth-token token)
   (set! (.-location js/window) "/") ;hide the token!
   (secretary/dispatch! "/")
   ))

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/stalky" []
  (session/put! :current-page #'Stalky))

(secretary/defroute "/about" []
  (session/put! :current-page #'About))

(secretary/defroute "/auth" []
  (authenticate))

(secretary/defroute "/auth/:token" [token]
  (authenticate token))

;; (secretary/defroute "/filters" []
;;   (session/put! :current-page #'Filters))

;; -------------------------
;; History
;; must be called after routes have been defined
(defn hook-browser-navigation! []
  (doto (History.)
    (events/listen
     EventType/NAVIGATE
     (fn [event]
       (secretary/dispatch! (.-token event))))
    (.setEnabled true)))


;; -------------------------
;; Initialize app

(defn mount-root []
  (reagent/render [current-page] (.getElementById js/document "app")))

(defn init! []
  (hook-browser-navigation!)
  (mount-root)
  (set-people-to-all-active-klicksters people-atom (* 5 60 1000))
)


(defn reload! []
  )
