#!/usr/bin/env bb

(require '[clojure.string :as str])
(load-file "agent.bb")

;; =============================================================================
;; Core Functions 
;; =============================================================================

(defn print-banner []
  (println "🚀 Ek (एक) v0.1.0-alpha"))

(defn suggest-blueprint [description]
  ;; @TODO Eventually to be replaced by SQLite vector search
  (cond 
    (and (str/includes? description "web") (str/includes? description "database"))
    [:web/server :web/routing :web/templating :db/connection]
    
    (and (str/includes? description "api") (str/includes? description "auth"))
    [:web/server :web/routing :web/json :auth/jwt]
    
    (str/includes? description "web")
    [:web/server :web/routing :web/templating]
    
    (str/includes? description "api") 
    [:web/server :web/routing :web/json]
    
    (str/includes? description "library")
    [:dev/testing :dev/docs]
    
    :else
    [:web/server :web/routing]))

(defn get-blueprint-name [blueprint description]
  (cond
    (and (str/includes? description "web") (str/includes? description "database"))
    ":web/full-stack"
    
    (and (str/includes? description "api") (str/includes? description "auth"))
    ":api/secure"
    
    (str/includes? description "web")
    ":web/simple-app"
    
    (str/includes? description "api") 
    ":api/basic"
    
    (str/includes? description "library")
    ":lib/standard"
    
    :else
    ":web/minimal"))

(defn get-library-options [component]
  (case component
    :web/server {:default "ring" 
                 :options {"ring" "Ring - Most popular, great middleware"
                          "undertow" "Undertow - High performance, reactive"
                          "jetty" "Jetty - Lightweight, embedded focus"}}
    :web/routing {:default "reitit"
                  :options {"reitit" "Reitit - Data-driven, fast, great for APIs"
                           "compojure" "Compojure - Macro-based, familiar syntax"
                           "bidi" "Bidi - Bidirectional routing"}}
    :web/templating {:default "hiccup"
                     :options {"hiccup" "Hiccup - Clojure data structures → HTML"
                              "selmer" "Selmer - Django-style templates"
                              "mustache" "Mustache - Logic-less templates"}}
    :web/json {:default "cheshire"
               :options {"cheshire" "Cheshire - Fast, feature-rich JSON"
                        "jsonista" "Jsonista - High performance JSON"
                        "data.json" "data.json - Core Clojure JSON"}}
    :auth/jwt {:default "buddy-auth"
               :options {"buddy-auth" "Buddy - Comprehensive auth library"
                        "clj-jwt" "clj-jwt - Simple JWT implementation"
                        "ring-oauth2" "ring-oauth2 - OAuth2 integration"}}
    :db/connection {:default "next.jdbc"
                    :options {"next.jdbc" "next.jdbc - Modern JDBC wrapper"
                             "clojure.java.jdbc" "java.jdbc - Classic, stable"
                             "hugsql" "HugSQL - SQL as Clojure functions"}}
    {:default "default" :options {"default" "Standard implementation"}}))

(defn get-dependencies [blueprint library-choices]
  "Returns the latest dependency versions for the selected blueprint and library choices"
  (mapcat (fn [component]
            (let [selected-lib (get library-choices component 
                                   (:default (get-library-options component)))]
              (case [component selected-lib]
                ;; Web Server Dependencies (Updated 2025)
                [:web/server "ring"] 
                ["ring/ring-core:1.14.1" "ring/ring-jetty-adapter:1.14.1"]
                
                [:web/server "undertow"] 
                ["io.undertow/undertow-core:2.3.19.Final" "ring/ring-core:1.14.1"]
                
                [:web/server "jetty"] 
                ["ring/ring-jetty-adapter:1.14.1" "ring/ring-core:1.14.1"]
                
                ;; Routing Dependencies (Updated 2025)
                [:web/routing "reitit"] 
                ["metosin/reitit:0.9.0" "metosin/reitit-ring:0.9.0"]
                
                [:web/routing "compojure"] 
                ["compojure/compojure:1.7.1" "ring/ring-core:1.14.1"]
                
                [:web/routing "bidi"] 
                ["bidi/bidi:2.1.6" "ring/ring-core:1.14.1"]
                
                ;; Templating Dependencies (Updated 2025)
                [:web/templating "hiccup"] 
                ["hiccup/hiccup:2.0.0-RC5"]
                
                [:web/templating "selmer"] 
                ["selmer/selmer:1.12.68"]
                
                [:web/templating "mustache"] 
                ["de.ubercode.clostache/clostache:1.4.0"]
                
                ;; JSON Dependencies (Updated 2025)
                [:web/json "cheshire"] 
                ["cheshire/cheshire:6.0.0"]
                
                [:web/json "jsonista"] 
                ["metosin/jsonista:0.3.13"]
                
                [:web/json "data.json"] 
                ["org.clojure/data.json:2.5.1"]
                
                ;; Authentication Dependencies (Updated 2025)
                [:auth/jwt "buddy-auth"] 
                ["buddy/buddy-auth:3.0.1" "buddy/buddy-sign:3.6.1-359"]
                
                [:auth/jwt "clj-jwt"] 
                ["clj-jwt/clj-jwt:0.1.1"]
                
                [:auth/jwt "ring-oauth2"] 
                ["ring-oauth2/ring-oauth2:0.2.0"]
                
                ;; Database Dependencies (Updated 2025)
                [:db/connection "next.jdbc"] 
                ["com.github.seancorfield/next.jdbc:1.3.1002" "com.zaxxer/HikariCP:6.2.1"]
                
                [:db/connection "clojure.java.jdbc"] 
                ["org.clojure/java.jdbc:0.7.12"]
                
                [:db/connection "hugsql"] 
                ["com.layerware/hugsql:0.5.3" "com.github.seancorfield/next.jdbc:1.3.1002"]
                
                ;; Development Dependencies (Updated 2025)
                [:dev/testing] 
                ["org.clojure/test.check:1.1.1"]
                
                [:dev/docs] 
                ["codox/codox:0.10.8"]
                
                ;; Default case
                [])))
          blueprint))


(defn parse-library-overrides [args]
  (let [flag-args (filter #(str/starts-with? % "--") args)]
    (->> flag-args
         (map (fn [flag]
                (let [value-idx (inc (.indexOf args flag))
                      value (when (< value-idx (count args)) (nth args value-idx))
                      component-type (subs flag 2)] ; Remove --
                  (case component-type
                    "server" [:web/server value]
                    "routing" [:web/routing value] 
                    "templating" [:web/templating value]
                    "json" [:web/json value]
                    "auth" [:auth/jwt value]
                    "database" [:db/connection value]
                    [nil nil]))))
         (filter #(not (nil? (first %))))
         (into {}))))

(defn apply-library-overrides [library-choices overrides]
  (merge library-choices overrides))

(defn run-project-creation-agent [project-name blueprint library-choices dependencies]
  (run-agent project-name
             ;; @TODO make build tool as an option 
             (format "Create Clojure project %s, libraries %s. Use deps.edn and make sure there is a main namespace"
                     project-name (pr-str dependencies))))


;; =============================================================================
;; Interactive Dependency Customization
;; =============================================================================

(defn display-component-options [component]
  "Display numbered options for a component and return the options list"
  (let [lib-info (get-library-options component)
        options-vec (vec (:options lib-info))
        default-lib (:default lib-info)]
    
    (println (str "\n🔧 " (name component) " options:"))
    (doseq [[idx [lib desc]] (map-indexed vector options-vec)]
      (let [default-marker (if (= lib default-lib) " ⭐ (default)" "")
            number (inc idx)]
        (println (str "   " number ". " lib default-marker))
        (println (str "      " desc))))
    
    options-vec))

(defn get-user-choice [options-vec default-lib]
  "Get user's choice by number, return the selected library"
  (print (str "Enter choice (1-" (count options-vec) ") or press Enter for default: "))
  (flush)
  (let [input (str/trim (or (read-line) ""))
        choice-num (try 
                    (Integer/parseInt input)
                    (catch Exception _ nil))]
    (cond
      (empty? input) default-lib
      (and choice-num 
           (>= choice-num 1) 
           (<= choice-num (count options-vec)))
      (first (nth options-vec (dec choice-num)))
      :else
      (do
        (println "❌ Invalid choice. Using default.")
        default-lib))))

(defn interactive-library-customization [blueprint]
  "Interactive customization of library choices for each component in blueprint"
  (println "\n🔧 Dependency Customization:")
  (println "Choose libraries for each component (or press Enter for defaults)\n")
  
  (reduce (fn [library-choices component]
            (let [lib-info (get-library-options component)]
              (if (not= (:default lib-info) "default") ; Skip components without options
                (let [options-vec (display-component-options component)
                      default-lib (:default lib-info)
                      selected-lib (get-user-choice options-vec default-lib)]
                  (println (str "✓ Selected: " selected-lib "\n"))
                  (assoc library-choices component selected-lib))
                library-choices)))
          {}
          blueprint))

;; =============================================================================
;; Experience Functions
;; =============================================================================


(defn experience-create-natural [description]
  (let [project-name (-> description
                        (str/replace #"[^\w\s]" "")
                        (str/replace #"\s+" "-")
                        str/lower-case)
        suggested-blueprint (suggest-blueprint description)
        blueprint-name (get-blueprint-name suggested-blueprint description)
        initial-library-choices {}]
    
    (println (str "🎯 Blueprint selected: " blueprint-name))
    (println)
    (println (str "📋 Blueprint: " (str/join ", " suggested-blueprint)))
    (println)
    
    (let [dependencies (get-dependencies suggested-blueprint initial-library-choices)]
      (println (str "📦 Dependencies: " (str/join ", " dependencies)))
      (println)
      
      (print "Customize dependencies? [y/N]: ")
      (flush)
      (let [customize (str/lower-case (str/trim (or (read-line) "n")))
            final-library-choices (if (= customize "y")
                                    (interactive-library-customization suggested-blueprint)
                                    initial-library-choices)
            final-dependencies (get-dependencies suggested-blueprint final-library-choices)]
        (println)
        (println "📋 Final Configuration:")
        (println (str "   Project: " project-name))
        (println (str "   Blueprint: " blueprint-name))
        (when (seq final-library-choices)
          (println "   Custom Libraries:")
          (doseq [[component lib] final-library-choices]
            (println (str "     " (name component) " → " lib))))
        (println (str "   Dependencies: " (str/join ", " final-dependencies)))
        (println)
        
        ;; Call the project creation agent
        (run-project-creation-agent project-name suggested-blueprint final-library-choices final-dependencies)))))



(defn experience-create-named [name description]
  (let [suggested-blueprint (suggest-blueprint description)
        blueprint-name (get-blueprint-name suggested-blueprint description)
        initial-library-choices {}]
    
    (println (str "🎯 Blueprint selected: " blueprint-name))
    (println)
    (println (str "📋 Blueprint: " (str/join ", " suggested-blueprint)))
    (println)
    
    (let [dependencies (get-dependencies suggested-blueprint initial-library-choices)]
      (println (str "📦 Dependencies: " (str/join ", " dependencies)))
      (println)
      
      (print "Customize dependencies? [y/N]: ")
      (flush)
      (let [customize (str/lower-case (str/trim (or (read-line) "n")))
            final-library-choices (if (= customize "y")
                                    (interactive-library-customization suggested-blueprint)
                                    initial-library-choices)
            final-dependencies (get-dependencies suggested-blueprint final-library-choices)]
        
        (println)
        (println "📋 Final Configuration:")
        (println (str "   Project: " name))
        (println (str "   Blueprint: " blueprint-name))
        (when (seq final-library-choices)
          (println "   Custom Libraries:")
          (doseq [[component lib] final-library-choices]
            (println (str "     " component " → " lib))))
        (println (str "   Dependencies: " (str/join ", " final-dependencies)))
        (println)
        
        ;; Call the project creation agent
        (run-project-creation-agent name suggested-blueprint final-library-choices final-dependencies)))))


(defn experience-create-with-overrides [project-name description library-overrides]
  (let [suggested-blueprint (suggest-blueprint description)
        blueprint-name (get-blueprint-name suggested-blueprint description)
        final-library-choices (apply-library-overrides {} library-overrides)]
    (println (str "🎯 Blueprint selected: " blueprint-name))
    (println)
    (println (str "📋 Blueprint: " (str/join ", " suggested-blueprint)))
    (println)
    
    (let [dependencies (get-dependencies suggested-blueprint final-library-choices)]
      (println (str "📦 Dependencies: " (str/join ", " dependencies)))
      (println)
      ;; Call the project creation agent
      (run-project-creation-agent project-name suggested-blueprint final-library-choices dependencies))))

(defn experience-interactive []
  (println "💬 Interactive Project Creator")
  (println "Let's build something together!\n")
  
  (print "What are you trying to build? ")
  (flush)
  (let [input (read-line)
        suggested-blueprint (suggest-blueprint input)
        blueprint-name (get-blueprint-name suggested-blueprint input)]
    
    (print "What should we call your project? ")
    (flush)
    (let [project-name (read-line)]
      (println)
      (println (str "🎯 Blueprint selected: " blueprint-name))
      (println)
      (println (str "📋 Blueprint: " (str/join ", " suggested-blueprint)))
      (println)
      
      (let [dependencies (get-dependencies suggested-blueprint {})]
        (println (str "📦 Dependencies: " (str/join ", " dependencies)))
        (println)
        
        (print "Customize dependencies? [y/N]: ")
        (flush)
        (let [customize-deps (str/lower-case (str/trim (or (read-line) "n")))
              final-library-choices (if (= customize-deps "y")
                                      (interactive-library-customization suggested-blueprint)
                                      {})
              final-dependencies (get-dependencies suggested-blueprint final-library-choices)]
          
          (println)
          (println "📋 Final Configuration:")
          (println (str "   Project: " project-name))
          (println (str "   Blueprint: " blueprint-name))
          (when (seq final-library-choices)
            (println "   Custom Libraries:")
            (doseq [[component lib] final-library-choices]
              (println (str "     " (name component) " → " lib))))
          (println (str "   Dependencies: " (str/join ", " final-dependencies)))
          (println)
          
          (print "Looks good? [Y/n]: ")
          (flush)
          (let [confirm (str/lower-case (str/trim (or (read-line) "y")))]
            (if (not= confirm "n")
              ;; Call the project creation agent
              (run-project-creation-agent project-name suggested-blueprint final-library-choices final-dependencies)
              (println "👋 Maybe next time!"))))))))


(defn experience-list []
  (println "📋 Blueprint Templates:")
  (println "   :web/simple-app   • Basic web application")
  (println "   :web/full-stack   • Web app with database")
  (println "   :api/basic        • REST API service") 
  (println "   :api/secure       • API with authentication")
  (println "   :lib/standard     • Clojure library")
  
  (println "\n🧩 Available Components:")
  (println "   web/server        • HTTP server")
  (println "   web/routing       • URL routing")
  (println "   web/templating    • HTML templating")
  (println "   web/json          • JSON handling")
  (println "   auth/jwt          • JWT authentication") 
  (println "   db/connection     • Database access"))

(defn experience-libraries 
  ([]
   (println "📚 Library Options by Component:")
   (println "\n🌐 Web Components:")
   (println "   web/server     • ring, undertow, jetty")
   (println "   web/routing    • reitit, compojure, bidi")
   (println "   web/templating • hiccup, selmer, mustache")
   (println "   web/json       • cheshire, jsonista, data.json")
   
   (println "\n🔐 Authentication:")
   (println "   auth/jwt       • buddy-auth, clj-jwt, ring-oauth2")
   
   (println "\n🗄️  Database:")
   (println "   db/connection  • next.jdbc, java.jdbc, hugsql")
   
   (println "\n💡 Usage:")
   (println "   ek libraries web/server    # Show server options")
   (println "   ek create \"API\" --server undertow --routing compojure"))
  
  ([component-name]
   (let [component-key (keyword component-name)
         lib-info (get-library-options component-key)]
     (if (not= (:default lib-info) "default")
       (do
         (println (str "📚 Library Options for " component-name ":"))
         (println)
         (doseq [[lib desc] (:options lib-info)]
           (let [marker (if (= lib (:default lib-info)) " ⭐ (default)" "")]
             (println (str "   " lib marker))
             (println (str "      " desc))))
         (println)
         (println "💡 Usage examples:")
         (println (str "   ek create my-app \"web app\" --" (last (str/split component-name #"/")) " " (first (keys (:options lib-info)))))
         (println (str "   ek create \"API service\" --" (last (str/split component-name #"/")) " " (last (keys (:options lib-info))))))
       (println (str "❓ Component '" component-name "' not found or has no library options"))))))

(defn experience-blueprints []
  (println "🧩 Blueprint Catalog")
  (println)
  (println "🌐 Web Applications:")
  (println "   :web/simple-app   • [:web/server :web/routing :web/templating]")
  (println "   :web/full-stack   • [:web/server :web/routing :web/templating :db/connection]")
  
  (println "\n⚡ API Services:")
  (println "   :api/basic        • [:web/server :web/routing :web/json]")
  (println "   :api/secure       • [:web/server :web/routing :web/json :auth/jwt]")
  
  (println "\n📚 Libraries:")
  (println "   :lib/standard     • [:dev/testing :dev/docs]")
  
  (println "\n💡 Usage:")
  (println "   ek create \"web app\"     → selects :web/simple-app")
  (println "   ek create \"REST API\"    → selects :api/basic"))

(defn show-help []
  (print-banner)
  (println "\nUsage:")
  (println "  ek create \"description\"               # Natural language")  
  (println "  ek create name \"description\"          # Named project")
  (println "  ek create name \"desc\" --server ring   # With library overrides")
  (println "  ek blueprints                          # Browse blueprints")
  (println "  ek libraries [component]               # Show library options")
  (println "\nExamples:")
  (println "  ek create \"web app\"")
  (println "  ek create acme-web \"web app\"")
  (println "  ek create my-api \"REST service\" --routing compojure")
  (println "  ek create \"API\" --server undertow --json jsonista")
  (println "\nLibrary Overrides:")
  (println "  --server      ring, undertow, jetty")
  (println "  --routing     reitit, compojure, bidi") 
  (println "  --templating  hiccup, selmer, mustache")
  (println "  --json        cheshire, jsonista, data.json")
  (println "  --auth        buddy-auth, clj-jwt")
  (println "  --database    next.jdbc, java.jdbc, hugsql"))

;; =============================================================================
;; CLI Router
;; =============================================================================

(defn main [& args]
  (cond
    ;; No args or help
    (or (empty? args) 
        (= (first args) "help")
        (= (first args) "--help"))
    (show-help)
    
    ;; Create commands
    (= (first args) "create")
    (cond
      (= (count args) 1)           ; Just "ek create" 
      (experience-interactive)
      
      (= (count args) 2)           ; "ek create description"
      (let [description (second args)
            library-overrides (parse-library-overrides args)]
        (if (seq library-overrides)
          (do
            (println "🔧 Library overrides applied:")
            (doseq [[component lib] library-overrides]
              (println (str "   " (name component) " → " lib)))
            (println)
            (experience-create-with-overrides 
             (-> description (str/replace #"[^\w\s]" "") (str/replace #"\s+" "-") str/lower-case)
             description 
             library-overrides))
          (experience-create-natural description)))
      
      (>= (count args) 3)          ; "ek create name description [--flags]"
      (let [name (second args)
            non-flag-args (take-while #(not (str/starts-with? % "--")) (drop 2 args))
            description (str/join " " non-flag-args)
            library-overrides (parse-library-overrides args)]
        (if (seq library-overrides)
          (do
            (println "🔧 Library overrides applied:")
            (doseq [[component lib] library-overrides]
              (println (str "   " (name component) " → " lib)))
            (println)
            (experience-create-with-overrides name description library-overrides))
          (experience-create-named name description))))
    
    ;; List 
    (= (first args) "list")
    (experience-list)
    
    ;; Blueprints
    (= (first args) "blueprints")
    (experience-blueprints)
    
    ;; Libraries
    (= (first args) "libraries")
    (if (= (count args) 2)
      (experience-libraries (second args))
      (experience-libraries))
    
    ;; Unknown command
    :else
    (do 
      (println (str "❓ Unknown command: " (first args)))
      (println "Try: ek help"))))

;; Entry point
(apply main *command-line-args*)
