(ns turdAlert.core
   (:require [net.cgrand.enlive-html :as html])
   (:use [turdAlert.homepage :only [logged-in not-logged-in]]
         [ net.cgrand.moustache :only [app]]
         [ring.adapter.jetty :only [run-jetty]]
        [ring.util.response :only [response file-response]]
        [ring.middleware.reload :only [wrap-reload]]
        [ring.middleware.file :only [wrap-file]]
        [ring.middleware.stacktrace :only [wrap-stacktrace]]
        [ring.middleware.session :only [wrap-session]]
        [ring.middleware.cookies :only [wrap-cookies]]
        [ring.middleware.params :only [wrap-params]]
        [ring.middleware.session.store :only [delete-session write-session read-session]]))



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

(defn user? [u] true)

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
         (wrap-file *webdir*)
         (wrap-session {:cookie-name "Turd Alert"})
         (wrap-cookies)
         (wrap-params)
         (wrap-reload nses)
         (wrap-stacktrace))
     {:port (Integer. port)})))

(defmacro serve-app
  ([app] `(app* (var ~app)))
  ([app port] `(app* (var ~app) :port ~port)))

;; ===========================================
;; Routes: the handler for the all the routes
;; ===========================================

(defn index [{:keys [topic page]}]
  (fn [req]
    (if (get-in req [:session :userid])
      (render-to-response (logged-in {:session (:session req) :topic topic :page page}))
      (render-to-response (not-logged-in {:topic topic :page page})))))

(def routes
  (app
   [] (index {})
   ["topic" topic]  (index {:topic topic :page 1})
   ["topic" topic "page=" page] (index {:topic topic :page page})
   [&]   page-not-found))

(defn sign-in [{{username "username"  password "password" :as params} :params :as req}]
  (if-let [userid (user? username password)]
    (index (assoc req :session {:username username :count 0 :userid userid}))))

(def post-handler
  (app
   ["sign-in"] #(sign-in %)
   ["search"] (index {:topic "search"})))

;{username :username count :count userid :userid :as session} :session


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


