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
;; Example data
(def Max {:IsObjectivesAdmin false, :LaborCategoryID "TECHNCAL", :FirstName "Maximilian", :LaborRoleID "APPLDEVL", :OversightPercent 25, :WorkTeamID "101", :BusinessUnitName "KH4 Tech", :PhotoFileName "4966_2688_sq.jpg", :IsCandidateAdmin false, :CanCommunicateClient false, :UserSystemID 80, :Email "mgerlach@klick.com", :CreatedDate "/Date(1370232000000-0000)/", :BillingTargetHoursPerYear 1650, :Title "Senior Mobile Developer", :IsClient false, :IsScheduleConfirmationRulesEnforced true, :LastName "Gerlach", :IsScheduleAdmin false, :UserName "mgerlach", :Extension "2381", :TimeZoneName "America/Toronto", :IsNotAPerson false, :UserID 4966, :KeyscanUpdated "/Date(1447455535000-0000)/", :HasDirectReports false, :IsAdmin false, :BusinessUnitID 24, :CountryID "CA", :CompanyBusinessUnitID 1, :TimeZoneID 90, :PhotoPath "https://genome.klick.com:443/local-instance/staff%20images/4966_2688_sq.jpg", :Name "Max Gerlach", :Roles [{:RoleID "DWFullAccess", :IsBusinessUnitScope true} {:RoleID "ws", :IsBusinessUnitScope false}], :CompanyBusinessUnitName "Klick Health", :IsWeeklyReviewAdmin false, :Enabled true, :TagName "Max_Gerlach", :Supervisors [3642 3990 4363 3446 3438 3424 3449], :Status {:CanSMS false, :IsOnGenome true, :CanGTalk false}})


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Actions

;; Network
(defn- extract-content [] (map #(-> % :body :Entries)))

(defn <all-active-users []
  (http/jsonp "https://genome.klick.com:443/api/User/Search"
            {:channel (chan 1 (extract-content))}))

(defn extract-userids [users] (map :UserID users))

(defn <users-details [userids]
  (let [param-string (clojure.string/join "," userids)]
    (http/jsonp "https://genome.klick.com:443/api/User.json"
                {:query-params {:UserIDs param-string}
                 :channel (chan 1 (extract-content))})))

(defn add-full-picturepath
  [user]
  (update-in user [:PhotoPath] #(str "https://genome.klick.com:443" (clojure.string/replace %1 #" " "%20"))))

(defn filter-in? [grouped-users]
  (reduce-kv (fn [m k v]
               (when (and k
                          (.startsWith k "In"))
                 (assoc m k v))) {} grouped-users))

(go
  (as-> [5702 4966] $
    (<! (<users-details $))
    (map add-full-picturepath $)
    (prn $)))

(go
  (as-> (<! (<all-active-users)) $
    (extract-userids $)
    (drop 200 $)
    (take 200 $) ; chunk the data!
    (<! (<users-details $))
    (group-by :KeyscanStatus $)
    (filter-in? $)
    (def people $)
    (prn $)))

;; TODO:
;; partition-by :KeyscanStatus. Then display and add filters.
;; add a <users-details test data set monday morning
;; partition the users by 200


;; -------------------------
;; Views

(defn PersonCard [user]
  [:div.z-depth-1 {:style {:max-width 100
                           :text-align :center}}
   [:div {:style {:width 100
                  :height 100
                  :background-image (str "url(" (:PhotoPath user) ")")}}]
   [:span {:style {:width "100%"
                   :font-size 16}}
    (:FirstName user)]])

(defn home-page []
  [PersonCard Max])

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
