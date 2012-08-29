(ns turdAlert.database.util
  (:import [java.net URI])
  (:use [clojure.string :only [split]]))


(defn get-db-specs [ur]
  (let [uri (java.net.URI. ur)]
    (merge
      (hash-map :host (.getHost uri),
     	        :port (.getPort uri),
		:db (subs (.getPath uri) 1),
	        :subname (subs (.getPath uri) 1),
	        :subprotocol (.getScheme uri))
      (if-let [user-info (.getUserInfo uri)]
        (hash-map :user  	((clojure.string/split user-info #"\W") 0),
		  :password	((clojure.string/split user-info #"\W") 1))))))








