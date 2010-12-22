;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:author "Brenton Ashworth"}
  sandbar.forms2
  "Form layout and processing. A set of protocols are defined for working with
  forms. Default implementations of the protocols are provided. Form
  implementors may build forms using a combination of the defaults and custom
  implementations of the protocols. Finally, constructor functions are provided
  to allow for concise form definitions.

  In all dealings with forms, form data is represented as a map of simple data
  types. This may include strings, numbers and vectors of such. For example:

  {:name \"Brenton\"
   :age 10
   :region 2
   :roles [\"admin\" \"user\"]
   :languages [1 2 3]}

  When data is loaded from an external data source, it must be transformed
  into this format. When a form is submitted, the data will arrive in this
  format for validation and all response functions will receive it in this
  way.

  Several protocols below take an env argument. This will be a map which may
  contain the keys:

  :errors A map of field names to error vectors. Each error vector will
          contain one or more strings which are the error messages for that
          field.
  :labels A map of field names to strings. Each string is the label to be used
          for that field. This will allow all labels to be placed into a
          single map and will make internationalization simple to implement."
  (:use [sandbar.core :only [get-param]]
        [sandbar.validation :only [validation-errors]])
  (:require [hiccup.core :as html]
            [clojure.string :as string]))

;; See sandbar.forms2.example-basic for usage.

;; ==============
;; Form protocols
;; ==============

(defprotocol Field
  "Standard field types include: :textfield :textarea :cancel-button
  :submit-button :hidden :checkbox, etc."
  (field-map [this] [this data]
             "Return a map of the field's :type, :name and :value.")
  (render-field [this data env] "Return renderd HTML for this field."))

(defprotocol Layout
  "Layout form fields and render as HTML. Returns a map with a key :body
  where the value is the rendered HTML. Depending on the implementation,
  optional keys may be added to pass other information back from the layout."
  (render-layout [this request fields data env]
                 "Return a map containing :body and other optional keys."))

(defprotocol Form
  "Render form HTML."
  (unique-id [this] "Return a unique identifier for this form.")
  (render-form [this request fields data env]
               "Return a map containing :body and other optional keys."))

(defprotocol Defaults
  "Get the default data for a form."
  (default-form-data [this request] "Return a map of default form data."))

(defprotocol DataSource
  "Load data for a form from an external data source."
  (load-form-data [this request] "Return a map of form data."))

(defprotocol Control
  "Add hidden control fields when a form is created. The response-map is a map
  with keys :fields and :response. The add-control function will take and
  return a response-map. The value of :response will be merged into the
  final Ring response. To use this data when a form is submitted, implement
  SubmitProcessor."
  (add-control [this request response-map]
               "Return map with :fields and :response."))

(defprotocol SubmitResponse
  "Each function will usually return a Ring response or a function of the
  response which returns a Ring response map."
  (canceled [this data] "Return a map or function the request.")
  (failure [this data errors] "Return a map or function request.")
  (success [this data] "Return a map function request."))

(defprotocol SubmitProcessor
  "Filter control information out of the submitted data. If an exceptional
  state is encountered, generate a response. Status contains :request :data
  and :return keys."
  (process-submit [this response status] "Return the new status."))
  
(defprotocol FormHandler
  "Process a complete form view request. There may be many different kinds
  of form handlers. Depending on the implementation, my return a Ring response
  map or map to be used as an intermediate result."
  (process-request [this request]
                   "Return a map containing :body and other optional keys."))

(defprotocol Routes
  (routes [this] "Returns a routing function."))

;; =======================
;; Default implementations
;; =======================

;;
;; Form fields
;;

(defn field-type-dispatch
  [field-map]
  (:type field-map))

(defmulti field-label
  "Create a field label for a form element."
  field-type-dispatch)

(defmethod field-label :default
  [{:keys [label required]}]
  (let [div [:div.field-label label]]
    (if required
      (vec (conj div [:span.required "*"]))
      div)))

(defmulti form-field-cell
  "Create a field cell which includes the field, field label and optionally
  on error message."
  field-type-dispatch)

(defmethod form-field-cell :default
  [{:keys [id html errors] :as field-map}]
  (let [error-message (first errors) ;; TODO show all errors
        error-attrs (if error-message
                      {}
                      {:style "display:none;"})]
    [:div.field-cell
     (field-label field-map)
     [:div.field-error-message error-attrs error-message]
     html]))

(defrecord Textfield [field-name attributes] Field
  (field-map [this] (field-map this {}))
  (field-map [this data] {:type :textfield
                          :name field-name
                          :value (or (field-name data) "")})
  (render-field [this data env]
    (let [d (merge (field-map this data)
                   {:label (or (:label this)
                               (get (:labels env) field-name))
                    :errors (get (:errors env) field-name)
                    :required (:required this)
                    :id (:id attributes)})
         field-html [:input
                     (merge {:type "text"
                             :name (name field-name)
                             :value (:value d)
                             :class "textfield"}
                            attributes)]]
     (html/html (form-field-cell (assoc d :html field-html))))))

(defrecord Hidden [field-name value] Field
  (field-map [this] (field-map this {}))
  (field-map [this data] {:type :hidden
                          :name field-name
                          :value (or (get data field-name)
                                     value
                                     "")})
  (render-field [this data env]
                [:input {:type "hidden"
                         :name (name field-name)
                         :value (:value (field-map this data))}]))

(defrecord Button [field-name value] Field
  (field-map [this] {:type (if (= field-name :cancel)
                             :cancel-button
                             :submit-button)
                     :name field-name
                     :value value})
  (field-map [this data] (field-map this))
  (render-field [this data env]
                (html/html [:input.sandbar-button
                            {:type "submit"
                             :name (name field-name)
                             :value value}])))

;;
;; Form layout
;;

(defn button? [field]
  (contains? #{:cancel-button :submit-button} (:type (field-map field))))

(defrecord GridLayout [title] Layout
  (render-layout [this request fields data env]
    (let [buttons (filter button? fields)
          fields (filter (complement button?) fields)
          rendered-fields (map #(render-field % data env) fields)
          rendered-buttons (map #(render-field % data env) buttons)
          title (if (fn? title)
                  (title request)
                  title)
          body (html/html [:table
                           [:tr
                            [:td
                             [:div rendered-fields]
                             [:div.buttons
                              [:span.basic-buttons rendered-buttons]]]]])
          result {:body body}]
      (if title
        (assoc result :title title)
        result))))

;;
;; Form
;;

(defrecord SandbarForm [form-name action-method layout attributes] Form
  (unique-id [this] form-name)
  (render-form [this request fields data env]
    (let [[action method] (action-method request)
          layout (render-layout layout request fields data env)
          method-str (.toUpperCase (name method))]
      (assoc layout
        :body
        (html/html
         [:div.sandbar-form
          (-> (if (contains? #{:get :post} method)
                [:form (merge {:method method-str :action action} attributes)]
                [:form (merge {:method "POST" :action action} attributes)
                 [:input {:type "hidden" :name "_method" :value method-str}]])
              (conj (:body layout))
              (vec))])))))



;;
;; Data
;;

(extend-protocol Defaults
  clojure.lang.Fn
  (default-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (default-form-data [this request] this))

(extend-protocol DataSource
  nil
  (load-form-data [this request] nil)
  clojure.lang.Fn
  (load-form-data [this request] (this request))
  clojure.lang.IPersistentMap
  (load-form-data [this request] this))

;;
;; Form Handling
;;

(defrecord EmbeddedFormHandler [form fields controls data-source defaults]
  FormHandler
  (process-request [this request]
    (let [form-id (unique-id form)
          errors (-> request :flash form-id :errors)
          data (when errors
                 (-> request :flash form-id :data))
          data (or data
                   (load-form-data data-source request)
                   (default-form-data defaults request))
          env (if errors {:errors errors} {})
          response {}
          fields (if (fn? fields)
                   (fields request)
                   fields)
          {:keys [fields response]}
          (if (seq controls)
            (reduce (fn [f next-control]
                      (add-control next-control request f))
                    {:fields fields
                     :response {}}
                    controls)
            fields)
          result (render-form form request fields data env)]
      (if errors
        (assoc result :errors errors)
        result))))

(defn marshal [params]
  (let [keys (map keyword (keys params))
        params (zipmap keys (vals params))]
    params))

#_(defn RedirectResponse [cancel-uri success-fn] SubmitResponse
  (canceled [this data] (redirect cancel-url))
  (failure [this data errors]
           (fn [request]
             (redirect (let [failure-uri (get (-> request :headers) "referer")]
                         ;; Store errors and form data in flash
                         failure-uri))))
  (success [this data] (redirect (success-fn data))))

(defn process-form-submit
  "Call process-submit on each processor until one returns a non-nil value
  or until you run out of processors."
  [response processors status]
  (loop [processors processors
         status status]
    (if (or (:return status) (not (seq processors)))
      status
      (recur (rest processors) (process-submit (first processors)
                                               response
                                               status)))))

(defrecord SubmitHandler [response controls validator] FormHandler
  (process-request [this request]
    (let [{:keys [return data]}
          (process-form-submit response
                               (conj controls validator)
                               {:request request
                                :data (-> request :params marshal)
                                :return nil})]
      (or return (success response data)))))

;;
;; Control
;;

(defrecord CancelControl []
  Control
  (add-control [this request {:keys [fields response]}]
    {:fields
     (if-let [cancel-buttons (->> fields
                                  (filter button?)
                                  (map field-map)
                                  (filter #(= (:type %) :cancel-button)))]
       (let [h (vec (map #(Hidden. :_cancel (:name %))
                         cancel-buttons))]
         (vec (concat fields h)))
       fields)
     :response response})
  SubmitProcessor
  (process-submit [this response {:keys [request data]}]
                  (let [cancel-name (keyword (:_cancel data))
                        cancel (cancel-name data)
                        data (dissoc data :_cancel cancel-name)
                        return {:request request
                                :data data}]
                    (if cancel
                      (merge return
                             {:return (canceled response data)})
                      return))))

(defrecord FunctionValidate [vfn] SubmitProcessor
  (process-submit [this response {:keys [data] :as status}]
    (let [errors (validation-errors (vfn data))]
      (if errors
        (merge status
               {:return (failure response data errors)})
        status))))

;; ============
;; Constructors
;; ============

(defn textfield
  "Create a form textfield. The first argument is the field name. Optional
  named arguments are label and required. Any other named arguments will be
  added to the field's html attributes.

  Examples:

  (textfield :age)
  (textfield :age :label \"Age\")
  (textfield :age :label \"Age\" :required true)
  (textfield :age :label \"Age\" :required true :size 50)
  (textfield :age :size 50 :id :age :value \"x\")"
  [field-name & {:keys [label required] :as options}]
  (let [attributes (-> (merge {:size 35} options)
                       (dissoc :label :required))
        field (Textfield. field-name attributes)
        field (if label (assoc field :label label) field)
        field (if required (assoc field :required required) field)]
    field))

(defn button [type & {:keys [label] :as options}]
  (let [label (or label (case type
                              :save "Save"
                              :cancel "Cancel"
                              "Submit"))]
    (Button. type label)))

(defn grid-layout
  "This will implement all of the features of the current grid layout."
  [& {:keys [title] :as options}]
  (let [title (or title "")]
    (GridLayout. title)))

(defn replace-params
  "Replace all routes params by values contained in the given params map."
  [m s]
  (reduce #(string/replace-first %1
                                 (str ":" (first %2))
                                 (second %2))
          s m))

(defn form
  "Create a form..."
  [form-name & {:keys [create-method update-method create-action
                       update-action layout]
                :as options}]
  (let [attributes (dissoc options
                           :create-method :update-method :create-action
                           :update-action :layout)
        action-method
        (fn [request]
          (let [route-params (:route-params request)
                id (get route-params "id")
                update-action (if (fn? update-action)
                                (update-action request)
                                update-action)
                create-action (if (fn? create-action)
                                (create-action request)
                                create-action)]
            [(replace-params route-params
                             (cond (and id update-action) update-action
                                   create-action create-action
                                   :else (:uri request)))
             (cond (and id update-method) update-method
                   create-method create-method
                   :else :post)]))
        layout (or layout (grid-layout))]
    (SandbarForm. form-name action-method layout attributes)))

(defn cancel-control []
  (CancelControl.))

(defn embedded-form
  "Create an embedded form handler."
  [form fields & {:keys [data-source defaults controls]
                  :as options}]
  (let [controls (or controls [(cancel-control)])
        defaults (or defaults {})]
    (EmbeddedFormHandler. form
                          fields
                          controls
                          data-source
                          defaults)))

(defn validator-function [vfn]
  (FunctionValidate. vfn))

(defn submit-handler
  [response & {:keys [controls validator]
               :as options}]
  (let [controls (or controls [(cancel-control)])
        validator (cond (nil? validator) (validator-function identity)
                        (satisfies? SubmitProcessor validator) validator
                        :else (validator-function validator))]
    (SubmitHandler. response controls validator)))
