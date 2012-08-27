(ns turdAlert.homepage
  (:use [net.cgrand.enlive-html
         :only [deftemplate defsnippet content clone-for nth-child snippet* transformation
                nth-of-type first-child do-> set-attr sniptest at emit* wrap append]]))



(defn add-link [item]
  "puts links around the items of the list items,
    when the items are maps with keys :link and :href.
    Link is made iff the item has a :href value"
  (if-let [link (:link item)]
          ((wrap :a {:href (get item :href "")}) link)
          item))

(defn add-list-item [item]
  "Returns function that adds item as an :li element to the node argument"
  #((append ((wrap :li {:id (format "%s_%d"
                                (get (:attrs %) :id "list_item")
                                 (inc (count (:content %))))
                        :class "list_item"}) item)) %))

(defn make-list [items]
  "Returns function that adds all the items as :li elements to the node argument."
  (apply do->
         (cons (content ())
               (map #(add-list-item %) items))))


(def topic1 {:href "#" :link "topic"})

(defn make-add []
  (content "add"))

(defn format-entry [node entry]
  ((snippet* node  [{:keys [title text submitted-by date location up-votes down-votes]}]
            [:.entry-title] (do->
                         (set-attr :id (format "%s-title" title))
                         (content title))
        [:.entry-text] (content text)
        [:.entry-infos] (content (map #((wrap :span {:id  (key %)})  (val %))
                                      {:submitted-by (format "from %s" submitted-by)
                                       :date date
                                       :location (format "at %s" location)
                                       :up-votes (format "%s up" up-votes)
                                       :down-votes (format "%s down" down-votes)}))) entry))

(def test-entry  {:title "title"
             :text "text"
             :submitted-by "anon"
             :date "8/31/12 at 3:30pm"
             :location "Los Angeles, CA"
             :up-votes 12343
             :down-votes 31})

(defn getUser [s] (str "User" "-Me"))
(defn get-entries2 [u n]
  [format-entry (first (:content n)) test-entry])
(defn get-entries [u n]
  ["hi" "hello"])
(defn get-topics [t]
  (repeat 8 topic1))


(deftemplate not-logged-in  "turdAlert/resources/index.html"
  [{:keys [session topic]  :or {topic "Top Turds"}}]
  [:title] (content "Turd Alert")
  [:#topic-name] (content topic)
  [:#topics] (make-list (map add-link (get-topics topic)))
  [:#entry] #(make-list (get-entries topic %))
  [:.add] (make-add)
  [:.logged-in] (content ""))

(deftemplate logged-in  "turdAlert/resources/index.html"
  [{:keys [session topic]  :or {topic "Top Turds"}}]
  [:title] (content "Turd Alert")
  [:#topic-name] (content topic)
  [:#topics] (make-list (get-topics topic))
  [:#entry] #(make-list (get-entries topic %))
  [:.add] (make-add)
  [:.not-logged-in] (content "")
  [:#user](content (getUser session)))






