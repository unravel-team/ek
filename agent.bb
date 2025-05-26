#!/usr/bin/env bb

(require '[babashka.http-client :as http]
         '[babashka.fs :as fs]
         '[babashka.process :as p]
         '[cheshire.core :as json]
         '[clojure.string :as str]
         '[clojure.java.io :as io])

;; Configuration - set your Claude API key
(def config
  {:api-key (or (System/getenv "ANTHROPIC_API_KEY") "your-claude-api-key-here")
   :api-endpoint "https://api.anthropic.com/v1/messages"
   :model "claude-3-5-sonnet-latest"
   :max-tokens 4096})

;; Tool implementations
(defn read-file-tool [file-path]
  "Read contents of a file"
  (try
    {:success true
     :content (slurp file-path)
     :message (str "Successfully read file: " file-path)}
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :message (str "Failed to read file: " file-path)})))

(defn list-files-tool [directory]
  "List files in a directory"
  (try
    (let [files (->> (fs/list-dir (or directory "."))
                     (map str)
                     (sort))]
      {:success true
       :files files
       :message (str "Found " (count files) " items in " (or directory "current directory"))})
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :message (str "Failed to list directory: " (or directory "current directory"))})))

(defn edit-file-tool [file-path content]
  "Write content to a file"
  (try
    (fs/create-dirs (fs/parent file-path))
    (spit file-path content)
    {:success true
     :message (str "Successfully wrote to file: " file-path)}
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :message (str "Failed to write file: " file-path)})))

(defn run-shell-command-tool [command]
  "Execute a shell command"
  (try
    (let [result (p/shell {:out :string :err :string} command)]
      {:success (zero? (:exit result))
       :exit-code (:exit result)
       :stdout (:out result)
       :stderr (:err result)
       :message (if (zero? (:exit result))
                  (str "Command executed successfully: " command)
                  (str "Command failed with exit code " (:exit result) ": " command))})
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :message (str "Failed to execute command: " command)})))

(defn create-directory-tool [dir-path]
  "Create a directory"
  (try
    (fs/create-dirs dir-path)
    {:success true
     :message (str "Successfully created directory: " dir-path)}
    (catch Exception e
      {:success false
       :error (.getMessage e)
       :message (str "Failed to create directory: " dir-path)})))

;; Available tools registry
(def tools
  {"read_file" {:description "Read the contents of a file"
                :input_schema {:type "object"
                              :properties {:file_path {:type "string"
                                                      :description "Path to the file to read"}}
                              :required ["file_path"]}
                :function read-file-tool}
   "list_files" {:description "List files and directories"
                 :input_schema {:type "object"
                               :properties {:directory {:type "string"
                                                       :description "Directory to list (optional, defaults to current directory)"}}
                               :required []}
                 :function list-files-tool}
   "edit_file" {:description "Write content to a file"
                :input_schema {:type "object"
                              :properties {:file_path {:type "string"
                                                      :description "Path to the file to write"}
                                          :content {:type "string"
                                                   :description "Content to write to the file"}}
                              :required ["file_path" "content"]}
                :function edit-file-tool}
   "run_shell_command" {:description "Execute a shell command"
                        :input_schema {:type "object"
                                      :properties {:command {:type "string"
                                                            :description "Shell command to execute"}}
                                      :required ["command"]}
                        :function run-shell-command-tool}
   "create_directory" {:description "Create a directory"
                       :input_schema {:type "object"
                                     :properties {:dir_path {:type "string"
                                                            :description "Path of the directory to create"}}
                                     :required ["dir_path"]}
                       :function create-directory-tool}})

(defn format-tools-for-claude []
  "Format tools for Claude API"
  (mapv (fn [[name tool]]
          {:name name
           :description (:description tool)
           :input_schema (:input_schema tool)})
        tools))

(defn execute-tool [tool-name args]
  "Execute a tool with given arguments"
  (if-let [tool (get tools tool-name)]
    (let [tool-fn (:function tool)]
      (case tool-name
        "read_file" (tool-fn (:file_path args))
        "list_files" (tool-fn (:directory args))
        "edit_file" (tool-fn (:file_path args) (:content args))
        "run_shell_command" (tool-fn (:command args))
        "create_directory" (tool-fn (:dir_path args))
        {:success false :message (str "Unknown tool execution pattern for: " tool-name)}))
    {:success false :message (str "Tool not found: " tool-name)}))

(defn make-claude-request [messages system-prompt]
  "Make a request to Claude API"
  (try
    (let [request-body {:model (:model config)
                        :max_tokens (:max-tokens config)
                        :system system-prompt
                        :messages messages
                        :tools (format-tools-for-claude)}
          response (http/post (:api-endpoint config)
                              {:headers {"x-api-key" (:api-key config)
                                         "Content-Type" "application/json"
                                         "anthropic-version" "2023-06-01"}
                               :body (json/generate-string request-body)
                               :throw false})]
      (if (= 200 (:status response))
        (json/parse-string (:body response) true)
        {:error (str "API request failed with status: " (:status response) 
                     " - " (:body response))}))
    (catch Exception e
      {:error (str "API request failed: " (.getMessage e))})))

(defn process-claude-content [content]
  "Process Claude's response content"
  (let [text-parts (filter #(= "text" (:type %)) content)
        tool-parts (filter #(= "tool_use" (:type %)) content)]
    {:text (str/join "\n" (map :text text-parts))
     :tool_calls tool-parts}))

(defn execute-tool-calls [tool-calls]
  "Execute tool calls and return results for Claude"
  (mapv (fn [tool-call]
          (let [tool-name (:name tool-call)
                tool-id (:id tool-call)
                args (:input tool-call)
                result (execute-tool tool-name args)]
            (println (str "🔧 Executed " tool-name ": " (:message result)))
            {:type "tool_result"
             :tool_use_id tool-id
             :content (if (:success result)
                        (json/generate-string (dissoc result :success))
                        (json/generate-string result))}))
        tool-calls))

(defn chat-with-claude [user-input conversation-history system-prompt]
  "Send message to Claude and handle tool calls"
  (let [messages (conj conversation-history {:role "user" :content user-input})
        response (make-claude-request messages system-prompt)]
    (if (:error response)
      (do
        (println "❌ Error:" (:error response))
        conversation-history)
      
      (let [content (:content response)
            processed (process-claude-content content)
            text-response (:text processed)
            tool-calls (:tool_calls processed)]
        (println "Tool calls in response " (count (:tool_calls processed)))
        
        ;; Print any text response
        (when (not (str/blank? text-response))
          (println (str "🤖 " text-response)))
        
        (if (seq tool-calls)
          ;; Handle tool calls
          (let [assistant-message {:role "assistant" :content content}
                tool-results (execute-tool-calls tool-calls)
                tool-message {:role "user" :content tool-results}]
            [:tool (conj (conj messages assistant-message) tool-message)])
          
          ;; No tool calls, just add the response
          [:no-tool (conj messages {:role "assistant" :content content})])))))

(defn show-available-tools []
  "Display available tools"
  (println "\n🛠️  Available tools:")
  (doseq [[name tool] tools]
    (println (str "  • " name ": " (:description tool)))))

(defn show-help []
  "Display help information"
  (println "\n📋 Commands:")
  (println "  • 'exit' or 'quit' - Exit the program")
  (println "  • 'tools' - Show available tools")
  (println "  • 'help' - Show this help message")
  (println "  • 'clear' - Clear conversation history")
  (println "  • Any other input - Chat with Claude AI")
  (println "\n💡 Examples:")
  (println "  • 'Create a simple Node.js web server'")
  (println "  • 'Build a Python script for data processing'")
  (println "  • 'Set up a Clojure project with deps.edn'"))

(defn is-exit-command? [input]
  "Check if input is an exit command"
  (contains? #{"exit" "quit" "q" ":q"} (str/lower-case (str/trim input))))

(defn run-agent [project-name user-input]
  "Main agent loop"
  (println "🚀 Ek Agent in progress...")
  (println "============================")
  ;; @TODO maintain system prompt in resources
  (let [system-prompt "You are a project builder that creates complete projects using ONLY tool calls until finished. Don't use lein or deps templates but write files from scratch

ABSOLUTE REQUIREMENTS:
1. When asked to create a project, respond with ONLY tool calls - NO TEXT until project is complete
2. Make continuous tool calls in sequence without stopping
3. A project is NOT complete until ALL files are created
4. DO NOT provide any text responses between tool calls
5. Only provide final summary text AFTER the project runs successfully
6. Always prefer libraries and versions provided and try to keep the setup minimal"]
    (loop [conversation-history []]
      (cond
        (is-exit-command? user-input)
        (println "👋 Thanks for using Claude AI Project Assistant! Goodbye!")
        
        (= (str/lower-case user-input) "tools")
        (do
          (show-available-tools)
          (recur conversation-history))
        
        (= (str/lower-case user-input) "help")
        (do
          (show-help)
          (recur conversation-history))
        
        (= (str/lower-case user-input) "clear")
        (do
          (println "🧹 Conversation history cleared!")
          (recur []))
        
        (str/blank? user-input)
        (recur conversation-history)
        
        :else
        (let [[t updated-history] (chat-with-claude user-input conversation-history system-prompt)]
          (if (= t :tool)
            (recur updated-history)
            (println (format "✅ Project %s created successfully!" project-name))))))))

;; Run the agent
(when (= *file* (System/getProperty "babashka.file"))
  (show-help)
  (print "\n💬 You: ")
  (flush)
  (let [user-input (str/trim (read-line))]
    (run-agent user-input)))
