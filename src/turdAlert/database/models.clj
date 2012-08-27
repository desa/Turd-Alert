(ns turdAlert.database.models
    (:use turdAlert.database.util
    	  korma.db
          korma.core))

(defdb turdDB
  (get-db-specs
    (System/getenv "DATABASE_URL")))

(defentity users)
(defentity posts)

