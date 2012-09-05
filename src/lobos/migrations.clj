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
      	   (table :users
	     (integer :id :primary-key)
	     (varchar :username 100 :unique)
	     (varchar :password 255 :not-null)
	     (varchar :email 255))))
  (down [] (drop (table :users))))

;;; Adding posts to the database
(defmigration add-posts-table
  (up [] (create turdDB
           (table :posts (integer :id :primary-key)
	     (varchar :nickname 100)
	     (varchar :title 250)
	     (varchar :city 100)
	     (varchar :state 100)
	     (integer :votes)
	     (text :post-content)
	     (timestamp :created (default (now)))
	     (integer :user [:refer :users :id] :not-null))))
  (down [] (drop (table :posts))))
