(ns uberblick.core
  (:require [accountant.core :as accountant]
            [cljs.core.async :as async :refer [<! >! chan take!]]
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

            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defonce people-atom (atom []))
(defonce filters (atom [
                       ;; "(.startsWith :Name \"M\")" 
                        ;; (= :LaborRoleID "APPLDEVL")
                        ;; (> (.indexOf :Title "Quality") -1)

                        ]))
;;examples
;; "(.startsWith :Name \"Max\")" 

(def all-profile-keys '(:IsObjectivesAdmin :LaborCategoryID :FirstName :LaborRoleID :OversightPercent :WorkTeamID :BusinessUnitName :MobileNumber :PhotoFileName :IsCandidateAdmin :CanCommunicateClient :UserSystemID :Email :CreatedDate :BillingTargetHoursPerYear :Title :MobileNumberCountryCode :IsClient :IsScheduleConfirmationRulesEnforced :LastName :IsScheduleAdmin :UserName :Extension :TimeZoneName :HomeNumber :IsNotAPerson :UserID :KeyscanUpdated :HasDirectReports :IsAdmin :BusinessUnitID :CountryID :CompanyBusinessUnitID :TimeZoneID :PhotoPath :Name :Roles :CompanyBusinessUnitName :IsWeeklyReviewAdmin :Enabled :TagName :Supervisors :KeyscanStatus :OutOfOfficeReason :Status))
; (take! (genome/<get-all-active-klickster-profiles) (fn [profiles] (->> (apply merge profiles ) keys prn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions


;; Network

(enable-console-print!)

  ;; BusinessUnitName: Department
  ;; WorkTeam: Your actual team


(go
  (as-> (<! (genome/<get-all-active-klickster-profiles)) $
    (reset! people-atom $)
    ;; (def people $)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter macro test
(defn eval-str [s]
  (try
    (eval (empty-state)
          s;(read-string s)
          {:eval       js-eval
           :source-map true
           :context    :expr}
          (fn [result] result))
    (catch :default e
      (println "Error while evaluating string")
      (prn e))))

;; Todo: Add ":my-bla"
(defn create-filter-fn 
  "Creates a filter function of the form:
  (fn [user] expr-as-fn)
  from a string 'expr' with one twist: 
  If a keyword is part of the passed-in 'keys' vector then this keyword will be transformed into a lookup of the 'user' binding.
  For instance, calling 
  (create-filter-fn [:Name]  \"(.startsWith :Name \"Max\")\")
  will result in a function like
  (fn [user] (.startsWith (:name user) \"Max\"))

  This can be later used to filter genome users by arbitrary criteria at runtime 
  "
  [thekeys expr]
  (let [my-user (gensym "user")
        un-myify (fn [kw] (-> kw str (.replace ":-my" "") keyword))
        replace-where-appropriate (fn [sym]
                                    (cond
                                      (not (keyword? sym)) sym
                                      (some #{sym} thekeys) `(if (~sym ~my-user) (~sym ~my-user) "")
                                      :else sym))]
    (some->>
     (try
       (read-string expr ,,,)
       (catch :default e
         (prn "Error while reading string")
         (prn e)))
     (walk/postwalk replace-where-appropriate ,,,)
     ((fn [new-expr]
        `(fn [~my-user] ~new-expr)) ,,,,) 
     ((fn [expr] (prn 'thekeys expr) expr))
     (eval-str ,,,))))



;; Todo:
;; partition-by :KeyscanStatus. Then display and add filters.
;; add a <users-details test data set monday morning
;; partition the users by 200

(defn filter-people [people]
  (let [all-the-people (try
                         (if-not (empty? @filters)
                           (let [my-filters (map #(create-filter-fn all-profile-keys %) @filters)]
                             (filter (apply every-pred my-filters) people))
                           people)
                         (catch :default e
                           (println "Error while filtering")
                           (prn e)))
        nil->Out (fn [by-floors]
                 (into {}
                       (for [[k v] by-floors]
                         [(if (nil? k) "Out" k) v])))]
    (some->> all-the-people
             (group-by :KeyscanStatus)
             (nil->Out)  ; mark the OUT ones for better processing!
             (genome/filter-in?)
             )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter functions
(defn add-filter 
  [filter-str]
  (swap! filters conj filter-str))

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

(defn PersonCard [user]
  [:div.z-depth-1 {:style {:max-width 100
                          :margin 5 
                           :text-align :center}}
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
   [:h3 title]
   [:hr]
   [:div {:style {:display :flex
                  :flex-direction :row
                  :flex-wrap :wrap}}
    (for [u users]
       ^{:key (:UserID u)} [PersonCard u])
  ]])

(defn home-page []
  [:div
   (let [people (filter-people @people-atom)]
     (cond
       (nil? people) [:h2 "There has been an error while applying your filters!"]
       (= 0 (count people)) [:h2 "No one that fits your filters is in!"]
       :else (for [[floor people] people]
               ^{:key floor} [Floor floor people])))])

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

(defn about [] (About))

(defn prettify [data]
  (with-out-str (cljs.pprint/pprint data)))

(defn Instructions 
  []
  (let [current-profile (atom "Nothing here")]
    (go
      (let [profile (<! (genome/<current-user-profile))]
        (reset! current-profile (-> profile prettify ))
        ))
    (fn []
      [:div
       [:p "Here is a list of all available attributes:"
        [:p (str all-profile-keys)]
        "And for the record, here is your profile:"]
       [:pre @current-profile]])))

(defn New-Filter-Field []
  (let [inputatm (atom "")]
    [:div.row
     [:input.col.s9.offset-s1 {:type "text"
                      :placeholder "Add a new filter"
                      :on-change #(reset! inputatm (-> % .-target .-value))}]
     [:button.col.s1.btn {:on-click #(do (add-filter @inputatm)
                                         (reset! inputatm ""))} "Add"]]))

(defn Current-Filters []
  [:ul.collection
   (map-indexed (fn [idx filter]
                  ^{:key filter}
                  ;; [:li (str filter)]
                  [:li.collection-item (str filter) [:a.secondary-content {:on-click #(remove-filter idx)} [:i.material-icons "delete"]]]) @filters)])

(defn Filters []
  [:div
   [:h3 "Filters"]
   [Current-Filters]
   [New-Filter-Field]
   [:hr]
   [:h3 "References"]
   [Instructions]])

(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about))

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
