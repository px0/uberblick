(ns uberblick.core
  (:require [accountant.core :as accountant]
            [cljs.core.async :as async :refer [<! >! chan take! timeout]]
            [cljs-http.client :as http]
            [genome-cljs.core :as genome]
            [goog.events :as events]
            [goog.history.EventType :as EventType]
            [markdown.core :as markdown]
            [reagent.core :as reagent :refer [atom]]
            [reagent.session :as session]
            [secretary.core :as secretary :include-macros true]
            [clojure.walk :as walk]
            [cljs.pprint :as pprint]
            [alandipert.storage-atom :refer [local-storage]]
            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop]])
  (:import goog.History))

;; -------------------------
;; State

(defonce people-atom (local-storage (atom []) :people))
(defonce filters (local-storage (atom ["(starts-with? :KeyscanStatus \"In\")"]) :filters))

(def all-profile-keys '(:IsObjectivesAdmin :LaborCategoryID :FirstName :LaborRoleID :OversightPercent :WorkTeamID :BusinessUnitName :MobileNumber :PhotoFileName :IsCandidateAdmin :CanCommunicateClient :UserSystemID :Email :CreatedDate :BillingTargetHoursPerYear :Title :MobileNumberCountryCode :IsClient :IsScheduleConfirmationRulesEnforced :LastName :IsScheduleAdmin :UserName :Extension :TimeZoneName :HomeNumber :IsNotAPerson :UserID :KeyscanUpdated :HasDirectReports :IsAdmin :BusinessUnitID :CountryID :CompanyBusinessUnitID :TimeZoneID :PhotoPath :Name :Roles :CompanyBusinessUnitName :IsWeeklyReviewAdmin :Enabled :TagName :Supervisors :KeyscanStatus :OutOfOfficeReason :Status))
; (take! (genome/<get-all-active-klickster-profiles) (fn [profiles] (->> (apply merge profiles ) keys prn)))

;; -------------------------
;; Actions


;; Network

(enable-console-print!)

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
          (reset! atm people)
          (println "Downloaded" (count @atm) "Klicksters")))) 
    (<! (timeout interval))
    (recur)))

(set-people-to-all-active-klicksters people-atom (* 5 60 1000))


;; -------------------------
;; filter macro test
(defn eval-str [s]
  (try
    (eval (empty-state)
          s
          {:eval       js-eval
           :source-map true
           :context    :expr}
          (fn [result] result))
    (catch :default e
      (println "Error while evaluating string")
      (prn e))))

(defn replace-current-user-symbols
  "Replaces keywords that are given in the form $keyword to the value of lookups to the given profile, e.g.
  the string \"(= :UserID $UserID)\" with the profile including {:UserID 123} will result in the string \"(= :UserID 123)\" "
  [profile expr]
  (let [$->kw (fn [kw] (->> kw str rest (apply str) keyword ))
        $? (fn [kw] (= \$ (first (str kw))))
        transform$ (fn [a] (if ($? a)
                            (get profile ($->kw a))
                            a))]
    (try
      (some->> expr
               (read-string)
               (walk/postwalk transform$)
               str)
      (catch :default e
          (prn 'replace-current-user-symbols e)))))

(defn create-filter-fn 
  "Creates a filter function of the form:
  (fn [user] expr-as-fn)
  from a string 'expr' with one twist: 
  If a keyword is part of the passed-in 'keys' vector then this keyword will be transformed into a lookup of the 'user' binding.
  For instance, calling 
  (create-filter-fn [:Name]  \"(.startsWith :Name \"Max\")\")
  will result in a function like
  (fn [user] (.startsWith (:name user) \"Max\"))

  This can be later used to filter genome users by arbitrary criteria at runtime "
  [thekeys expr]
  (let [my-user (gensym "user")
        replace-where-appropriate (fn [sym]
                                    (cond
                                      (not (keyword? sym)) sym
                                      (some #{sym} thekeys) `(if (~sym ~my-user) (~sym ~my-user) "")
                                      :else sym))]
    (some->>  expr
              (read-string)
              (walk/postwalk replace-where-appropriate ,,,)
              ((fn [new-expr]
                 `(fn [~my-user]
                    (let [~'substring? (fn [~'string ~'subsstr] (> (.indexOf ~'string ~'subsstr) -1))
                          ~'starts-with? (fn [~'string ~'substr] (.startsWith ~'string ~'substr))
                          ~'includes? (fn [~'coll ~'item] (some #{~'item} ~'coll))]
                      ~new-expr))) ,,,,) 
              ((fn [expr] (prn expr) expr))
              (eval-str ,,,))))

(defn filter-people [people]
  (try
    (if-not (empty? @filters)
      (let [my-filters (map #(create-filter-fn all-profile-keys %) @filters)]
        (filter (apply every-pred my-filters) people))
      people)
    (catch :default e
      (println "Error while filtering")
      (prn e))))


;; -------------------------
;; filter functions
(defn add-filter 
  [profile filter-str]
  (let [transformed-filter (replace-current-user-symbols profile filter-str)]
    (if transformed-filter
      (do
        (swap! filters conj transformed-filter)
        transformed-filter)
      nil)))

(defn vec-remove
  "remove elem in coll"
  [coll pos]
  (vec (concat (subvec coll 0 pos) (subvec coll (inc pos)))))

(defn remove-filter 
  [idx]
  (when @filters
    (reset! filters
            (vec-remove @filters idx))))


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
     (let [people (some->> (filter-people @people-atom)
                           (group-by :KeyscanStatus)
                           (into (sorted-map)) ; sort
                           )]
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
     (let [people (filter-people @people-atom)]
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


(defn Instructions 
  [current-profile]
  [:div
   [:p "Here is a list of all available attributes:"
    [:p (str all-profile-keys)]
    "And for the record, here is your profile:"]
   [:pre @current-profile]
   [:p "If you want to see the Clojure functions that you can use check out "
    [:a {:href "http://cljs.info/cheatsheet/"} "this cheatsheet"]
    " and this "
    [:a {:href "http://kanaka.github.io/clojurescript/web/synonym.html"} " comparison to JavaScript"]]])

(defn New-Filter-Field [current-profile inputatm]
  [:div.row
   [:input.col.s9.offset-s1 {:type "text"
                             :placeholder "Add a new filter"
                             :value @inputatm
                             :on-change #(reset! inputatm (-> % .-target .-value))}]
   [:button.col.s1.btn {:on-click #(do
                                     (let [success (add-filter @current-profile @inputatm)]
                                       (if success
                                         (reset! inputatm "")
                                         (js/alert "There has been a problem with your filter!")))
                                     )} "Add"]])

(defn Current-Filters [inputatm]
  [:ul.collection
   (map-indexed (fn [idx filter]
                  ^{:key filter}
                  [:li.collection-item (str filter)
                   [:div.secondary-content
                    [:a {:on-click #(do
                                      (prn filter)
                                      (reset! inputatm (str filter))
                                      (remove-filter idx))}
                     [:i.material-icons "edit"]]
                    [:a {:on-click #(remove-filter idx)} [:i.material-icons "delete"]]
                    ]]) @filters)])

(defn Examples [current-profile filter-examples]
  [:ul.collection
   (map-indexed (fn [idx filter]
                  ^{:key filter}
                  [:li.collection-item filter
                   [:a.secondary-content {:on-click #(add-filter @current-profile filter)}
                    [:i.material-icons "playlist_add"]]])
                filter-examples)])

(defn Filters []
  (let [inputatm (atom "")
        current-profile (atom nil)
        current-profile-str (atom "")]
    (go
      (let [profile (reset! current-profile (<! (genome/<current-user-profile)))]
        (reset! current-profile-str (-> profile prettify ))))
    (fn []
      [:div
       [:h3 "Filters"]
       [Current-Filters inputatm]
       [New-Filter-Field current-profile inputatm]
       [:hr]
       [:h3 "Introduction"]
       [:p "Here you can define filters on the collection of Klicksters. They
       will typically have the form (command parameter1 parameter2) - yes,
       including the parentheses, they are imporant - but they can be
       arbitrarily complex. Profile attributes can be referred to by
       keywords (anything starting with a ':'), and attributes starting with a
       '$' will automaticaly be replaced by attributes of *your* profile. I
       know, that sounds confusing, but check out the examples below and play
       around for a bit and it'll all make sense!"]
       [:hr]
       [:h3 "Examples"]
       [:p "Here are some example filters to get you started!"]
       [Examples current-profile [
                                  "(starts-with? :FirstName \"M\")" 
                                  "(= :LaborRoleID \"APPLDEVL\")"
                                  "(not (= :LaborCategoryID \"PROJMGMT\"))"
                                  "(= :WorkTeamID $WorkTeamID) ; Klicksters in your team "
                                  "(substring? :Title \"Quality\") ; Klicksters that have 'Quality' in their title"
                                  "(> :UserID $UserID) ; Klicksters that got hired after you"
                                  "(= :KeyscanStatus \"In 4th Flr N\")"
                                  "(or (starts-with? :FirstName \"M\") (substring? :Title \"Edit\"))"
                                  "(starts-with? :KeyscanStatus \"In\")"
                                  "(includes? :Supervisors $UserID) ; Klicksters who report to you"
                                  ]]
       [:p "Note: Anything after ';' is a comment. If you want to learn some more about this syntax check out "
        [:a {:href "http://learnxinyminutes.com/docs/clojure/"} "this quick introduction to Clojure"]
        " and the links at the bottom of this page!"]
       [:hr]
       [:h3 "References"]
       [Instructions current-profile-str]])))

(defn authenticate
  ([]
   (let [token (js/prompt "What is your Genome auth token?")]
     (-> token
         ((fn [t] (if (empty? t) nil t)))
         (authenticate))))
  ([token]
   (prn 'token token) 
   (genome/set-auth-token token)
   (set! (.-location js/window) "#filters") ;hide the token!
   (secretary/dispatch! "#filters")
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

(secretary/defroute "/filters" []
  (session/put! :current-page #'Filters))

;; -------------------------
;; History
;; must be called after routes have been defined
;;; use instead of accountant
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
  (comment
    (accountant/configure-navigation!)
    (accountant/dispatch-current!))
  (hook-browser-navigation!)

  (mount-root))
