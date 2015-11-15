(ns uberblick.core
    (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]])
    (:require [reagent.core :as reagent :refer [atom]]
              [reagent.session :as session]
              [secretary.core :as secretary :include-macros true]
              [accountant.core :as accountant]

              [goog.events :as events]
              [goog.history.EventType :as EventType]

              [alandipert.storage-atom :refer [local-storage]]
              [cljs-http.client :as http]
              [timothypratley.reanimated.core :as anim]
              [cljs.core.async :refer [<! chan put! take! >!] :as async]
              [markdown.core :refer [md->html] :as markdown])
    (:import goog.History))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

;; Network
(defn- extract-content [] (map #(-> % :body :Entries)))

(defn <get-klickster 
  [userid]
  (http/jsonp "https://genome.klick.com:443/api/User.json"
              {:query-params {"UserID" userid}
               :channel (chan 1 (extract-content))}))

(defn <search-user 
  [keyword]
  (http/jsonp "https://genome.klick.com:443/api/User.json"
              {:query-params {:ForAutocompleter true
                              :Enabled true
                              :IsNotAPerson false
                              :Keyword keyword}
               :channel (chan 1 (extract-content))}))

(defn <all-active-users []
  (http/jsonp "https://genome.klick.com:443/api/User/Search"
            {:channel (chan 1 (extract-content))}))

(defn extract-userids [users] (map :UserID users))

(go
  (-> (<! (<all-active-users))
      (extract-userids)
      prn))

;; -------------------------
;; Views

(defn home-page []
  [:div [:h2 "Welcome to uberblick!!!"]
   [:div [:a {:href "/about"} "go to about page"]]])

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
