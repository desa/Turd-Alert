(ns turdAlert.homepage
  (:use [net.cgrand.enlive-html
         :only [id= select deftemplate defsnippet content clone-for nth-child snippet* transformation
                nth-of-type first-child do-> set-attr sniptest at emit* wrap append]]))

;; ============================================
;; test data
;; ============================================

(def test-entry  {:title "title"
             :text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Phasellus
		facilisis bibendum fermentum. Donec dolor ante, iaculis at tincidunt ac,
		dapibus in risus. Donec aliquam consectetur libero, eget dignissim nisl
		laoreet quis. Suspendisse eget enim nec enim interdum commodo ut facilisis
		nunc. Aenean nec molestie nibh. Aliquam dolor enim, consequat vitae
		lobortis posuere, convallis eleifend lorem. Donec ipsum velit, venenatis
		at sollicitudin id, sollicitudin sit amet libero. Vestibulum cursus tellus
		eget mauris interdum ac bibendum sapien dictum. In hac habitasse platea
		dictumst."
             :submitted-by "anon"
             :date "8/31/12 at 3:30pm"
             :location "Los Angeles, CA"
             :up-votes 12343
                  :down-votes 31})

(def topic1 {:href "#" :link "topic"})

;; =============================================
;; Helper functions for the templates
;; =============================================

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
                        :class (format "%s-item" (get (:attrs %) :id "list"))}) item)) %))

(defn make-list [items]
  "Returns function that adds all the items as :li elements to the node argument."
  (apply do->
         (cons (content ())
               (map #(add-list-item %) items))))

(defn merge-node [src]
  "Returns function that puts the content of nodes from the src nodes that match
the id of the node argument into the content of the node."
  (fn [nd]
    (let [{ {id :id} :attrs, cont :content, :as node} nd]
      ((content  (get-in (first (select src [(id= id)])) [:content])) node))))
                                     
(def about-blurb
  "This is the about blurb")

(def contact-blurb
  ["Contact: " (add-link {:href "mailto:webmaster@turdalert.com"
                          :link "webmaster@turdalert.com"})])
(def contact-about
  ((wrap :div {:id "contact-about"}) [((wrap :p {:id "about-blurb"}) about-blurb)
                                      ((wrap :div {:id "contact-blurb"}) contact-blurb)]))
(def num-per-page 10)

;; ============================================
;; Functions to get certain data or check data.
;; ============================================
(defn recent-turds [a b]
  [])

(defn top-turds [t n]
  [])

(defn get-ad [id]
  "take the id and get an add that fits that spot"
  (str "add"))

(defn get-posts [topic page total]
  "Given topic t, get 10 (or whatever) entries for page p"
  (let [ent test-entry]
    (repeat num-per-page ent)))

(defn get-posts2 [topic page total]
  (if (= topic "New Turds")
    (recent-turds (* num-per-page (dec page)) (* num-per-page page))
    (top-turds (- total (* num-per-page (dec page))) num-per-page)))


(defn get-topics [t p]
  "Given topic t, get other topics for page p"
  [{:href "/topic/Top%20Turds" :link "Top Turds"}
   {:href "/topic/New%20Turds" :link "New Turds"}])

(defn makeUri [usr]
  "given user usr, make uri for their profile."
  (str usr))

(defn passwordFormat?
  "is password s in correct format? If no, put popup to notify them"
  [s] "")

(defn number-posts [] 100)

(def str-number-posts "100")

;; ============================================
;; Snippets
;; ============================================

(defsnippet format-entry "resources/entry.html" [:#entry] 
  [{:keys [title text submitted-by date location up-votes down-votes]}]
  [:.entry-title] (do->
                   (set-attr :id (format "%s-title" title))
                   (content title))
  [:.entry-text] (content text)
  [:.entry-infos] (content (map #((wrap :span {:id  (subs (str (key %)) 1)
                                               :class "entry-info"})  (val %))
                                {:submitted-by (format "from %s" submitted-by)
                                 :date date
                                 :location (format "at %s" location)
                                 :up-votes (format "%s up" up-votes)
                                 :down-votes (format "%s down" down-votes)})))

(defsnippet logged-in-deps "resources/log-deps.html" [:.logged-in]
  [username]
  [:a#settings-link] (set-attr :href (str "/usr/" username))
  [:#user] (content (str username))
  [:input.hidden-input] (set-attr :value (format "%s" username)))

(defsnippet not-logged-in-deps "resources/log-deps.html" [:.not-logged-in]
  []
  [:#sign-in-form] (set-attr :action "")
  [:#password-form] #((set-attr :onBlur (passwordFormat? (get-in % [:attrs :value]))) %)
  [:#forgot-password-button] (set-attr :action ""))

(defsnippet settings "resources/settings.html" [:.settings]
  [username n]
  [:#usr] (content username)
  [:#message] (fn [node]
                (cond
                 (= n 0) ((content (str "Hey " username "!")) node)
                 (= n 1) ((content "Passwords do not match") node)
                 (= n 2) ((content "That is not your password!") node))))


(defsnippet pagination "resources/entry.html" [:#pagination-links]
  [page total]
  [:#first-button]  (fn [node]
                        (if (= page 1)
                          ((set-attr :class "display-none") node)
                          ((set-attr :class "page-button") node)))
  [:#previous] (set-attr :value (format "%d" (dec page)))
  [:#previous-button] (fn [node]
                        (if (= page 1)
                          ((set-attr :class "display-none") node)
                          ((set-attr :class "page-button") node)))
  [:#previous-hidden] (set-attr :value (format "%d" total))
  [:#next-hidden] (set-attr :value (format "%d" total))
  [:#next] (set-attr :value (format "%d" (inc page)))
  [:#next-button] (fn [node]
                    (if (= page (quot total num-per-page))
                          ((set-attr :class "display-none") node)
                          ((set-attr :class "page-button") node)))
  [:#last] (set-attr :value (format "%d" (quot total num-per-page)))
  [:#last-button] (fn [node]
                    (if (= page (quot total num-per-page))
                          ((set-attr :class "display-none") node)
                          ((set-attr :class "page-button") node))))

  
;; ============================================
;; Templates
;; ============================================

(deftemplate not-logged-in  "resources/index.html"
  [{session :session, topic :topic, page :page, total :total,
    :or {topic "New Turds" page 1 total (number-posts)}}]
  [:title] (content "Turd Alert")
  [:#topic-name] (content topic)
  [:#topics] (make-list (map add-link (get-topics topic page)))
  [:#entry] (cond
             (= topic "about") (content contact-about)
             (= topic "contact") (content contact-about)
             :else (make-list (map format-entry (get-posts topic page total))))
  [:#pagination]  (cond
                   (= topic "Top Turds") (content (pagination page total))
                   (= topic "New Turds") (content  (pagination page total)))
  [:.add] #((content (get-ad (get-in % [:attrs :id]))) %)
  [:.log-dep] (merge-node (not-logged-in-deps)))

(deftemplate logged-in  "resources/index.html"
  [{session :session topic :topic page :page total :total
    :or {topic "New Turds" page 1 total (number-posts)}}]
  [:title] (content "Turd Alert")
  [:#topic-name] (content topic)
  [:#topics]  (make-list (map add-link (get-topics topic page)))
  [:#entry] (cond
             (= topic "settings") (content (settings (:username session) 0))
             (= topic "settings/p") (content (settings (:username session) 1))
             (= topic "settings/r") (content (settings (:username session) 2))
             (= topic "about") (content contact-about)
             (= topic "contact") (content contact-about)
             :else (make-list (map format-entry (get-posts topic page total))))
  [:#pagination] (cond
                  (= topic "Top Turds") (content (pagination page total))
                  (= topic "New Turds") (content (pagination page total)))
  [:.add] #((content (get-ad (get-in % [:attrs :id]))) %)
  [:.log-dep] (merge-node (logged-in-deps (:username session))))

(deftemplate passwords-not-equal "resources/index.html"
  []
  [:title] (content "Turd Alert")
  [:#topic-name] (content "New Turds")
  [:#topics]  (make-list (map add-link (get-topics "New Turds" 1)))
  [:.add] #((content (get-ad (get-in % [:attrs :id]))) %)
  [:#entry] (make-list (map format-entry (get-posts "New Turds" 1)))
  [:#pagination] (content (pagination 1 (number-posts)))
  [:head] (append ((wrap :style {:type "text/css"}) ["#register-block {display:block;}"])
                  ((wrap :style {:type "text/css"}) ["#passwords-not-equal {display:block;}"]))
  [:.log-dep] (merge-node (not-logged-in-deps)))                  

(deftemplate username-taken "resources/index.html"
  []
  [:title] (content "Turd Alert")
  [:#topic-name] (content "New Turds")
  [:#topics]  (make-list (map add-link (get-topics "New Turds" 0)))
  [:.add] #((content (get-ad (get-in % [:attrs :id]))) %)
  [:#entry] (make-list (map format-entry (get-posts "New Turds" 1)))
  [:#pagination]  (content (pagination 1 (number-posts)))
  [:head] (append ((wrap :style {:type "text/css"}) ["#register-block {display:block;}"])
                  ((wrap :style {:type "text/css"}) ["#username-taken {display:block;}"]))
  [:.log-dep] (merge-node (not-logged-in-deps)))

(deftemplate forgot-password "resources/index.html"
  []
  [:title] (content "Turd Alert")
  [:#topic-name] (content "New Turds")
  [:#topics]  (make-list (map add-link (get-topics "New Turds" 0)))
  [:#entry] (make-list (map format-entry (get-posts "New Turds" 1)))
  [:#pagination]  (content (pagination 1 (number-posts)))
  [:.add] #((content (get-ad (get-in % [:attrs :id]))) %)
  [:head] (append ((wrap :style {:type "text/css"}) ["#password-reset {display:block}"]))
  [:.log-dep] (merge-node (not-logged-in-deps)))


  






