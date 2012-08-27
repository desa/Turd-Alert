
(create
 turddDB
 (table
  :authors
  (integer :id :primary-key)
  (varchar :username 100 :unique)))

(create
 turdDB
 (table
  :playings
  (integer :id :primary-key)
  (varchar :username 100 :unique)
  (varchar :password 100 :not-null)))
