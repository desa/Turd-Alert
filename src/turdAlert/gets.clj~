(ns turdAlert.gets
    (:use korma.core
    	  korma.db
	  turdAlert.database.models))

(defn encrypt-pw [pw] 1000)

(defn get-user [username password]
      (let [digest (encrypt-pw password)
            usrs (select users (fields :id :password) (where {:username username}))
	    usr (usrs 0)]
	(if (= (:password usr) (str digest))
	  (:id usr)
	  nil))) 

(defn new-user [email username password]
      (let [digest (encrypt-pw password)
      	    id (+ 1 (count (select users)))]
      	   (insert users (values {:id id, :username username, :password digest, :email email}))))

(defn reset-password [username] nil) ;;sends an email to the users email

(defn new-post [user nickname title city state content]
  (let [id (+ 1 (count (select posts)))]
  (insert posts
    (values {:id id, :title title, :city city, :state state,
    	     :content content, :nickname nickname, :user user, :votes 0}))))

(defn get-ad [id] nil) ;;returns link for an ad

(defn get-posts [topic page] (select posts))