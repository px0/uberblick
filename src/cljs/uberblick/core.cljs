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

            [cljs.tools.reader :refer [read-string]]
            [cljs.js :refer [empty-state eval js-eval]])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defonce people-atom (atom []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions


;; Network

(enable-console-print!)

  ;; BusinessUnitName: Department
  ;; WorkTeam: Your actual team


(go
  (as-> (<! (genome/<get-all-active-klickster-profiles)) $
    (group-by :KeyscanStatus $)
    (genome/filter-in? $)
    (reset! people-atom $)
    ;; (def people $)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; filter macro test
(defn eval-str [s]
  (eval (empty-state)
        s;(read-string s)
        {:eval       js-eval
         :source-map true
         :context    :expr}
        (fn [result] result)))


(defn create-filter-fn 
  "creates a filter function thing"
  [keys expr]
  (let [my-user (gensym "user")
        replace-where-appropriate (fn [sym]
                                    (cond
                                      (not (keyword? sym)) sym
                                      (some #{sym} keys) `(~sym ~my-user)
                                      :else sym))]
    (->>
     (read-string expr)
     (walk/postwalk replace-where-appropriate ,,,)
     ((fn [new-expr]
        `(fn [~my-user] ~new-expr)) ,,,))))


(def exp "(.startsWith :Name \"Max\")")
(defn try-filter []
  (= '({:Name "Maximilian"}) 
     (filter
      (create-filter-fn [:Name] exp)
      [{:Name "Bob"} {:Name "Maximilian"}]))
  )

;; Todo:
;; partition-by :KeyscanStatus. Then display and add filters.
;; add a <users-details test data set monday morning
;; partition the users by 200


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
                  :background-repeat :no-repeat}}]
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
   (for [[floor people] @people-atom]
    ^{:key floor} [Floor floor people])])

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


(defn current-page []
  [:div [(session/get :current-page)]])

;; -------------------------
;; Routes

(secretary/set-config! :prefix "#")

(secretary/defroute "/" []
  (session/put! :current-page #'home-page))

(secretary/defroute "/about" []
  (session/put! :current-page #'about))

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
