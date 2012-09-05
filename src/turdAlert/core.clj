(ns turdAlert.core
   (:require [net.cgrand.enlive-html :as html])
   (:use [turdAlert.homepage :only [logged-in not-logged-in
                                    passwords-not-equal username-taken
                                    forgot-password number-posts]]
         [ net.cgrand.moustache :only [app pass]]
         [ring.adapter.jetty :only [run-jetty]]
        [ring.util.response :only [response file-response redirect-after-post redirect]]
        [ring.middleware.reload :only [wrap-reload]]
        [ring.middleware.file :only [wrap-file]]
        [ring.middleware.stacktrace :only [wrap-stacktrace]]
        [ring.middleware.session :only [wrap-session]]
        [ring.middleware.cookies :only [wrap-cookies]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.session.cookie]))



(def  *webdir* (str (System/getProperty "user.dir") "/resources/"))

(defn render [t]
  (apply str t))

(defn render-snippet [s]
  (apply str (html/emit* s)))

(def render-to-response
     (comp response render))

(defn page-not-found [req]
  {:status 404
   :headers {"Content-type" "text/html"}
   :body "Page Not Found"})

(defn render-request [afn & args]
  (fn [req] (render-to-response (apply afn args))))

(defn serve-file [filename]
  (file-response
   {:root *webdir*
    :html-files? true}))


;; ===========================================
;; The Server: calling serve-app runs Jetty with
;; the app provided
;; ===========================================

(defn app* [app & {:keys [port] :or {port 8880}}]
  (let [nses (if-let [m (meta app)]
               [(-> (:ns (meta app)) str symbol)]
               [])]
    (run-jetty
     (-> app
         (wrap-params)
         (wrap-session {:store (cookie-store)})
         (wrap-reload nses)
         (wrap-stacktrace))
     {:port (Integer. port)})))

(defmacro serve-app
  ([app] `(app* (var ~app)))
  ([app port] `(app* (var ~app) :port ~port)))

;; ===========================================
;; Routes: the handler for the all the routes
;; ===========================================

(defn index [{:keys [topic page total]}]
  (fn [req]
    (if (get-in req [:session :userid])
      (->
       (render-to-response (logged-in {:session (:session req) :topic topic
                                       :page page :total total}))
       (assoc :session (:session req)))
      (render-to-response (not-logged-in {:topic topic :page page :total total})))))


(def routes
  (app
   (wrap-file *webdir*)
   (app
    [] (index {:topic "New Turds" :page 1 :total (number-posts)})
    ["topic" topic]  (index {:topic topic :page 1 :total (number-posts)})
    ["topic" topic "t" total "page" page] (index {:topic topic
                                                  :page (Integer/parseInt page)
                                                  :total (Integer/parseInt total)})
    ["search"] (index {:topic "search"})
    ["usr" &]  (app
              ["usernametaken"] (redirect "/register/u")
              [username "p"] (index {:topic "settings/p"})
              [username "r"] (index {:topic "settings/r"})
              [username] (index {:topic "settings"})
              [&] pass)
    ["register" "p"] (fn [req] (render-to-response (passwords-not-equal)))
    ["register" "u"] (fn [req] (render-to-response (username-taken)))
    ["about"] (index {:topic "About"})
    ["contact"] (index {:topic "contact"})
    ["reset"] (fn [req] (render-to-response (forgot-password))) 
    [&]   page-not-found)))

(defn user? [u t] true)

(defn make-new-post ([u n c s c] "")
  ([n c s c] ""))

(defn make-new-user [e u p] (str u))

(defn reset-password [u] identity)

(defn sign-in [username password]
  (if (user? username password)
    (fn [req] (assoc (redirect "/") :session {:username username :userid "userid"}))
    (fn [req] (dissoc (redirect "/") :session))))

(defn new-report [nick city state content]
  (fn [req]
    (if-let [user (get-in req [:session :username])]
      (redirect (str "/" (make-new-post user nick city state content)))
      (redirect (str "/" (make-new-post nick city state content))))))

(defn new-user [email username password password2]
  (fn [req]
    (if (= password password2)
      (assoc (redirect (str "/usr/" (make-new-user email username password)))
        :session {:username username :userid (user? username password)})
      (redirect "/register/p"))))

(defn reset-password [u] "/reset")
      
(defn log-out [req]  (assoc (redirect "/") :session nil))

(defn forgot [username]
  (fn [req] (redirect "/reset")))

(defn change-p [u newp])

(defn change-password [oldp newp newp2 username]
  (if (user? username oldp)
    (if (= newp newp2)
      (fn [req] (do (change-p username newp)
                    (redirect (str "/usr/" username))))
      (fn [req] (redirect (str "/usr/" username "p"))))
    (fn [req] (redirect (str "/usr/" username "r")))))

(defn new-page [page total]
  (fn [req]
    (if (or ( = (:uri req) "") (:uri req) "/")
      (redirect (str "/topic/New%20Turds" "/t/" total "/page/" page))
      (redirect (str (subs (get req :uri) 0 18) "/t/" total "/page/" page)))))


(def post-handler
  (app
   (fn [req]
     (let [params (:params req)]
       (cond
        (get params "username")
             ((sign-in (get params "username") (get params "password")) req)
        (get params "report")
        ((apply new-report
                (map #(get params %)
                     ["reporter" "state" "city" "new-report"])) req)
        (get params "new-username")
        ((apply new-user
                (map #(get params %)
                     ["email" "new-username" "new-password" "new-password2"])) req)
        (get params "logout") (log-out req)
        (get params "forgot-username")
        ((forgot (get params "forgot-username")) req)
        (get params "old-password")
        ((apply change-password
                (map #(get params %)
                     ["old-pasword" "new-password"
                      "new-password2" (get-in req [:session :username])])) req)
        (get params "page")
        ((new-page (get params "page") (get params "total")) req)
        :else page-not-found)))))


;; ===========================================
;; The Server and Main
;; ===========================================

(def main-app
  (app
   :get routes
   :post post-handler))


(defn -main [port]
  (serve-app main-app port))

;; ===========================================
;; Helper Functions
;; ===========================================


(defmulti parse-int type)
(defmethod parse-int java.lang.Integer [n] n)
(defmethod parse-int java.lang.String [s] (Integer/parseInt s))

(defmacro maybe-substitute
  ([expr] `(if-let [x# ~expr] (html/substitute x#) identity))
  ([expr & exprs] `(maybe-substitute (or ~expr ~@exprs))))

(defmacro maybe-content
  ([expr] `(if-let [x# ~expr] (html/content x#) identity))
  ([expr & exprs] `(maybe-content (or ~expr ~@exprs))))

(defn pluralize [astr n]
  (if (= n 1)
    (str astr)
    (str astr "s")))


