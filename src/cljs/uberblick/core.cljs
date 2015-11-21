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
            [secretary.core :as secretary :include-macros true])
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:import goog.History))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defonce people-atom (atom []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

;; Network


(enable-console-print!)


#_(go
  (as-> (<! (genome/<all-active-users)) $
    (genome/extract-userids $)
    ;; (drop 200 $)
    ;; (take 200 $) ; chunk the data!
    (<! (genome/<users-details $))
    (map genome/add-full-picturepath $)
    (group-by :KeyscanStatus $)
    (genome/filter-in? $)
    ;; (reset! people-atom $)
    (def people $)
  
    (prn $)))

(defn dbg [thing] (prn thing) thing)

  ;; BusinessUnitName: Department
  ;; WorkTeam: Your actual team


#_(go
  (as-> (<! (genome/<get-all-active-klickster-profiles)) $
    ((fn [everybody]
       (prn 'everybody)
       (prn (count everybody))
       (def fucking-everybody everybody)
       everybody) $)
    (map genome/add-full-picturepath $)
    (group-by :KeyscanStatus $)
    (genome/filter-in? $)
    ;; (reset! people-atom $)
    (def people $)
    (prn $)))


;; Todo:
;; partition-by :KeyscanStatus. Then display and add filters.
;; add a <users-details test data set monday morning
;; partition the users by 200


;; -------------------------
;; Views

(defn PersonCard [user]
  [:div.z-depth-1 {:key (:UserID user)
                   :style {:max-width 100
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
      [PersonCard  u])
    [:br]]])

(defn home-page []
  [:div
   (for [[f p] @people-atom]
     [Floor f p])])

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

(prn (genome/gtest))
