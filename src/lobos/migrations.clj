(ns lobos.migrations
  (:refer-clojure :exclude [alter drop
                            bigint boolean char double float time])
  (:use [turdAlert.database.util]
  	[lobos.migration :only [defmigration]]
        [lobos.core]
        [lobos.schema]))
 
;;; Defines the database for lobos migrations
(def turdDB (get-db-specs (System/getenv "DATABASE_URL")))

;;; Adding users to the database
(defmigration add-users-table
  (up [] (create turdDB
      	   (table :users (integer :id :primary-key)
	     (varchar :username 100 :unique)
	     (varchar :password 100 :not-null)
	     (varchar :email 255))))
  (down [] (drop (table :users))))

;;; Adding posts to the database
(defmigration add-posts-table
  (up [] (create turdDB
           (table :posts (integer :id :primary-key)
	     (varchar :title 250)
	     (text :content)
	     (timestamp :created (default (now)))
	     (integer :user [:refer :users :id] :not-null))))
  (down [] (drop (table :posts))))