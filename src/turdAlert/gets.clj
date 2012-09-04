(ns turdAlert.gets
    (:use korma.core
    	  korma.db
	  turdAlert.database.models))

(defn encrypt-pw [pw] 1000)

(defn day-format [day]
  (let [dray (clojure.string/split day #"\W")
        dmonth (dray 1)
        dday   (dray 2)
        dyear  (dray 0)]
    (str dmonth "/" dday "/" dyear)))

(defn time-format [time]
  (subs time 0 5))

(defn date-format [date]
  (let [temp (clojure.string/split date #"\s")
        day (temp 0)
        time (temp 1)]
    (str (day-format day) " " (time-format time))))

(defn get-user [username password]
      (let [digest (encrypt-pw password)
            usrs (select users (fields :id :password) (where {:username username}))]
	(if (empty? usrs)
	    nil
	    (let [usr (usrs 0)]
	      (if (= (:password usr) (str digest))
	      	  (let [usr (usrs 0)] (:id usr))
	      :passworderr)))))

(defn new-user [email username password]
      (let [digest (encrypt-pw password)
      	    id (+ 1 (((select users (aggregate (max :id) :id)) 0) :id))]
	    (try (insert users (values {:id id, :username username, :password digest, :email email}))
	    	 (catch Exception e (str "caught exception: " (.getMessage e))))))

(defn reset-password [username] nil) ;;sends an email to the users email

(defn new-post [user nickname title city state content]
  (let [id (+ 1 (((select posts (aggregate (max :id) :id)) 0) :id))]
  (insert posts
    (values {:id id, :title title, :city city, :state state,
    	     :content content, :nickname nickname, :user user, :votes 0}))))

(defn get-ad [id] nil) ;;returns link for an ad

(defn get-recent 
      ([last n]
	(let [cnt (((select posts (aggregate (count :*) :count)) 0) :count)]
	  (select posts (order :created) (offset (- cnt (+ n last))) (limit n))))
	([n] 
	   (let [cnt (((select posts (aggregate (count :*) :count)) 0) :count)]
	     (select posts (order :created) (offset (- cnt n))))))

(defn get-top [a b]
      (let [cnt (((select posts (aggregate (count :*) :count)) 0) :count)]
        (select posts (aggregate (order :votes) :votes) (offset (- cnt (+ a b))) (limit b))))

(defn number-posts []
      (((select posts (aggregate (count :*) :count)) 0) :count))