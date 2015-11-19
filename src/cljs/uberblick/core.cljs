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
              [cljs.core.async :refer [<! chan put! take! >! alts! timeout] :as async]
              [markdown.core :refer [md->html] :as markdown])
    (:import goog.History))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State

(defonce people-atom (atom []))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

;; Network
(defn- extract-content [] (map #(-> % :body :Entries)))

(defn <all-active-users []
  (http/jsonp "https://genome.klick.com:443/api/User/Search"
            {:channel (chan 1 (extract-content))}))

(defn extract-userids [users] (map :UserID users))

(defn <users-details [userids]
  (let [param-string (clojure.string/join "," userids)
        out (chan)]
    (go
      (>! out (<! (http/jsonp "https://genome.klick.com:443/api/User.json"
                              {:query-params {:UserIDs param-string}
                               :channel (chan 1 (extract-content))})))
      (async/close! out))
    out))

(defn add-full-picturepath
  [user]
  (update-in user [:PhotoPath] #(str "https://genome.klick.com:443" (clojure.string/replace %1 #" " "%20"))))

(defn filter-in? [grouped-users]
  (reduce-kv (fn [m k v]
               (if (and k
                          (.startsWith k "In"))
                 (assoc m k v)
                 m)) {} grouped-users))

#_(go
  (as-> [5702 4966] $
    (<! (<users-details $))
    (map add-full-picturepath $)
    (prn $)))

#_(go
  (as-> (<! (<all-active-users)) $
    (extract-userids $)
    ;; (drop 200 $)
    (take 200 $) ; chunk the data!
    (<! (<users-details $))
    (map add-full-picturepath $)
    (group-by :KeyscanStatus $)
    (filter-in? $)
    ;; (reset! people-atom $)
    (def people $)
  
    (prn $)))

(defn dbg [thing] (prn thing) thing)

(comment
  BusinessUnitName: Department
  WorkTeam: Your actual team
  
  )

(defn <get-all-active-klickster-profiles 
  [userids]
  (let [chunked-ids (partition-all 200 userids)
        chunked-results (map <users-details chunked-ids)]
    (->> chunked-results
         (async/merge) ;put all results on a single channel
         (async/reduce concat []) ;return a channel that'll end up having all results
         )))

(go
  (as-> (<! (<all-active-users)) $
    (extract-userids $)
    ;; (drop 200 $)
    (<! (<get-all-active-klickster-profiles $))
    ((fn [everybody]
       (prn 'everybody)
       (prn (count everybody))
       (def fucking-everybody everybody)
       everybody) $)
    (map add-full-picturepath $)
    (group-by :KeyscanStatus $)
    (filter-in? $)
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

  (defn return-stuff [stuff]
    (let [c (chan)]
      (go
        (>! c stuff)
        (async/close! c))
      c))


  (defn get-all []
    (let [s1 (return-stuff :a)
          s2 (return-stuff :b)
          s3 (return-stuff :c)
          stuffs [s1 s2 s3]]
 
      (->> stuffs
           (async/merge)
           (async/reduce conj [])))) 



(go (prn (<! (get-all))))


