;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.forms
  "Forms and form layouts."
  (:use [clojure.tools.macro :only [name-with-attributes]]
        [ring.util.response :only [redirect]]
        [compojure.core :only [routes GET POST PUT]]
        [sandbar.stateful-session :only [flash-put!
                                         flash-get]]
        [sandbar.core :only [cpath get-param property-lookup]]
        [sandbar.util :only [index-by]]
        [clojure.tools.logging :only (info error)]
        [sandbar.validation :only [if-valid
                                   required-fields
                                   build-validator
                                   validation-errors]])
  (:require [clojure.string :as string]
            [clojure.data.json :as json]
            [clojure.pprint :as pretty]
            [compojure.route :as route]))

;;
;; Utilities
;;

(defn store-errors-and-redirect [name redirect-page]
  (fn [form-data errors]
    (do (flash-put! name
                    (merge {:form-data form-data} errors))
        redirect-page)))

(defn get-params
  "Get params from a map where the keys may be strings or keywords."
  [keys params]
  (reduce (fn [a b]
            (let [v (get-param params b)]
              (assoc a b v)))
          {}
          keys))

(defn filter-nil-vec [coll]
  (vec (filter #(not (nil? %)) coll)))

(defn form-cancelled?
  ([params]
     (form-cancelled? params "Cancel"))
  ([params value]
     (let [cancel-values (conj #{"cancel" "Cancel"} value)
           cancel (get-param params :cancel)]
       (contains? cancel-values cancel))))

(defn nil-or-empty-string? [v]
  (or (not v)
      (and (string? v)
           (empty? v))))

(defn clean-form-input
  "Set empty values to nil and remove the id if it is nil."
  [m]
  (let [original-meta (meta m)]
    (with-meta
      (apply merge
             (map #(hash-map (first %)
                             (let [value (last %)]
                               (if (nil-or-empty-string? value)
                                 nil
                                 value)))
                  (if (not (nil-or-empty-string? (:id m)))
                    m
                    (dissoc m :id))))
      original-meta)))

(defn replace-params
  "Replace all routes params by values contained in the given params map."
  [m s]
  (reduce #(string/replace-first %1
                                 (str ":" (first %2))
                                 (second %2))
          s m))

;;
;; Form Elements
;;

(defn field-label
  "Create a field label for a form element."
  [label key req]
  (let [label (if (map? label)
                (property-lookup label key)
                label)
        req (cond (keyword? req) req
                  (contains? (set req) key) :required
                  :else :optional)
        div [:div {:class "field-label"} label]]
    (if (= req :required)
      (vec (conj div [:span {:class "required"} "*"]))
      div)))

(defn hidden
  "Create a hidden form field."
  ([field-name] (hidden field-name nil))
  ([field-name value]
     {:type :hidden
      :label ""
      :field-name field-name
      :html [:input {:type "hidden"
                     :name (name field-name)
                     :value (or value "")}]}))

#_(defn htmlfield
  ([fname content]
     (htmlfield nil content {:id fname}))
  ([label content options & more]
     (let [fname (:id options)]
       {:type :htmlfield
        :label (if label (field-label label fname :optional) [:div])
        :field-name fname
        :html [:div options content]})))

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
  (let [options (-> (merge {:size 35} options)
                    (dissoc :label :required))]
    (assoc {:type :textfield
            :label (fn [l r]
                     (field-label (or label l) field-name r))
            :field-name field-name
            :html [:input
                   (merge {:type "Text" :name (name field-name) :value ""
                           :class "textfield"} options)]}
      :required (true? required))))

(defn password
  "Create a form password field. See doc for textfield."
  [& args]
  (let [textfield (apply textfield args)]
    (-> textfield
        (assoc :type :password)
        (assoc :html [:input (merge (last (:html textfield))
                                    {:type "Password"})]))))

(defn textarea
  "Create a form textarea. The first argument is the field name. Optional named
  arguments are label and required. Any other arguments will be added to the
  field's html attributes.

  Examples:

  (textarea :notes)
  (textarea :notes :label \"Notes\")
  (textarea :notes :label \"Notes\" :required true)
  (textarea :notes :rows 5 :cols 80)"
  [field-name & {:keys [label required] :as options}]
  (let [options (dissoc options :label :required)]
    (assoc {:type :textarea
            :label (fn [l r]
                     (field-label (or label l) field-name r))
            :field-name field-name
            :html [:textarea (merge {:name (name field-name)} options)]}
      :required (true? required))))

(defn- checkbox-label-fn [field-name title class-name]
  (fn [t]
    [:span {:class class-name} (or title
                                   (if (map? t)
                                     (get t field-name field-name)
                                     t))]))

(defn checkbox
  "Create a form checkbox. The first argument is the field name. The named
  argument label is optional. Any other named arguments will be added to the
  field's html attributes. The value returned from a checkbox with will true
  or nil.

  Examples:

  (checkbox :elated)
  (checkbox :elated :label \"Elated\")
  (checkbox :elated :label \"Elated\" :id :elated)"
  [field-name & {:keys [label] :as options}]
  {:type :checkbox
   :label (checkbox-label-fn field-name label "field-label")
   :field-name field-name
   :html [:input
          (merge {:type "checkbox"
                  :name (name field-name)
                  :value "checkbox-true"} options)]
   :required false})

(defn- select-options [coll key-fn value-fn props]
  (let [value-fn (fn [m]
                   (let [v (value-fn m)]
                     (get props v (name v))))]
    (map #(vector :option {:value  (key-fn %)} (value-fn %))
         coll)))

(defmulti make-bindings (fn [type request bindings field-name] type))

(defn- make-bindings*
  "Create default bindings for missing values and, if the source is a function
  of the request, call it."
  [request bindings field-name default-coll]
  (let [{:keys [source value visible data]} (field-name bindings)
        source (or source default-coll)
        source (if (fn? source)
                 (source request)
                 source)
        value (or value name)
        data (or data identity)
        visible (or visible identity)]
    {:source source
     :value value
     :visible visible
     :data data}))

(defmethod make-bindings :select [type request bindings field-name]
           (make-bindings* request bindings field-name [:yes :no]))

(defmethod make-bindings :multi-checkbox [type request bindings field-name]
           (make-bindings* request bindings field-name [:red :blue :green]))

(defn select
  "Create a form select element. The first argument is the field name. Other
  optional named arguments are label, prompt and required:

  label: The label for the select element.
  prompt: A map representing the default selection. The key will be the select
          option's value and the val will be its visible name.
  required: true or false, is this field required?

  Any additional named arguments will be added to the select element's html
  attributes.

  Use the :bindings option of make-form to set the data source for this field."
  [field-name & {:keys [label prompt required]
                 :as options}]
  (let [options (dissoc options :prompt :required :label)
        prompt (first prompt)
        select-html [:select (merge {:name (name field-name)} options)]
        select-html (if prompt
                      (concat select-html
                              [[:option
                                {:value (key prompt)} (val prompt)]])
                      select-html)
        html-function
        (fn [request bindings properties]
          (let [{:keys [source value visible]}
                (make-bindings :select request bindings field-name)]
            (vec
             (concat
              select-html
              (select-options source value visible properties)))))]
    (assoc {:type :select
            :label (fn [l r]
                     (field-label (or label l) field-name r))
            :field-name field-name
            :html html-function}
      :required (true? required))))

(defn multi-select
  "Create a multi-select form field. This is the same as a select field but
  with the attribute multiple set to true. The default size is 5 but can be
  changed by passing a size option. This element does not take a prompt
  option. See doc for select.

  Use the :bindings option of make-form to set the data source for this field."
  [field-name & {:keys [title source value visible required]
                 :as options}]
  (let [options (merge {:multiple true :size 5} options)]
    (-> (apply select field-name (interleave (keys options)
                                             (vals options)))
        (assoc :type :multi-select))))

(defn wrap-checkboxes-in-group [coll]
  [:div {:class "group"}
     (map #(vector :div {:class "group-checkbox"} %) coll)])

(defn multi-checkbox
  "Create a form multi-checkbox, which is a group of checkboxes. The first
  argument is the field name. The named argument label may be used to set the
  label for the group.

  This field is functionally equivalent to using a multi-select field.

  Use the :bindings option of make-form to set the data source for this field."
  [field-name & {:keys [label] :as options}]
  {:type :multi-checkbox
   :label (checkbox-label-fn field-name label "group-label")
   :field-name field-name
   :html (fn [request bindings properties]
           (let [{:keys [source value visible]}
                 (make-bindings :multi-checkbox request bindings field-name)
                 filter-pred (:filter options)
                 coll (if filter-pred
                        (filter (partial filter-pred request) source)
                        source)]
             (wrap-checkboxes-in-group
              (map
               #(vector :input
                        {:type "checkbox" :name field-name :value (value %)}
                        (property-lookup properties (keyword (visible %))))
               coll))))})

(defn form-to
  [[method action attrs] & body]
  (let [method-str (.toUpperCase (name method))]
    (-> (if (contains? #{:get :post} method)
          [:form (merge {:method method-str, :action action} attrs)]
          [:form (merge {:method "POST", :action action} attrs)
           (:html (hidden (str "_method") method-str))])
        (concat body)
        (vec))))

;;
;; Buttons
;;

(defn cancel-button
  ([] (cancel-button "Cancel"))
  ([v]
     [:input {:type "submit" :value v :name "cancel"
              :class "sandbar-button"}]))

(defn submit-button
  ([] (submit-button "Submit"))
  ([v]
     [:input {:type "submit" :value v :name "submit"
              :class "sandbar-button"}]))

(defn reset-button
  ([] (reset-button "Reset"))
  ([v]
     [:input {:type "reset" :value v :class "sandbar-button"}]))

(defn create-form-button [[k visible-name]]
  (case k
        :submit (submit-button visible-name)
        :save (submit-button visible-name)
        :cancel (cancel-button visible-name)
        :reset (reset-button visible-name)
        :save-and-new (submit-button visible-name)
        :submit-and-new (submit-button visible-name)
        (submit-button visible-name)))

(defn compile-buttons [buttons]
  (let [labels (take 2 (reverse buttons))
        labels (if (= (second labels) :labels) (first labels) nil)
        buttons (take-while #(not (keyword? %)) buttons)]
    (map #(cond (= (count %) 1)
                (let [button-type (first %)
                      value (button-type labels)
                      value (or value (string/capitalize
                                       (name button-type)))]
                  [button-type value])
                (and (= (count %) 2) (keyword? (second %)))
                [(first %) (get labels (second %) (second %))]
                :else %)
         buttons)))

(defn special-button-text
  "Returns a map which contains the value of the Cancel and Save and New
   buttons. These values may be set by the user, so they can be anything, and
   we need them in order to decide were to redirect after a form submission."
  [buttons]
  (let [buttons (compile-buttons buttons)
        cancel (second (first (filter #(= (first %) :cancel) buttons)))
        save-and-new (second
                      (first
                       (filter #(contains? #{:save-and-new :submit-and-new}
                                           (first %))
                               buttons)))]
    {:cancel cancel :save-and-new save-and-new}))

(defn display-buttons [class-prefix buttons]
  (let [buttons (compile-buttons buttons)]
    (vec
     (concat
      [:span {:class (str class-prefix "-buttons")}]
      (interleave (map create-form-button buttons) (cycle ["&nbsp;"]))))))

(defn- get-colspan [rows]
  (let [cells (drop 1 (first rows))]
    (reduce (fn [colspan cell]
              (if (map? (second cell))
                (+ colspan (Integer/valueOf (or (:colspan (second cell)) 1)))
                (+ colspan 1)))
            0
            cells)))

(defn append-buttons-to-table
  "Append a row of buttons to the end of the table within the passed div. The
   table can be in any location within the div but there can be only one."
  [div buttons]
  (let [[beginning end] (split-with #(not (and (coll? %)
                                               (= :table (first %)))) div)
        table (first end)
        end (drop 1 end)
        colspan (get-colspan (rest table))
        new-table (when table
                    (conj table
                          [:tr 
                           [:td {:colspan colspan}
                            [:div.buttons
                             (display-buttons "basic" buttons)]]]))]
    (vec (if new-table
           (concat beginning [new-table] end)
           div))))

;;
;; Marshal and Binding data
;;

(defn marshal-checkboxes
  "Get true or nil values for all keys in key-set. These keys represent
  checkboxes. If the checkbox is not present then is was not selected and
  will be set to nil."
  [m params key-set]
  (let [new-map (reduce
                 (fn [a b]
                   (let [k (key b)
                         k (if (keyword? k) k (keyword k))]
                     (if (and (contains? key-set k)
                              (= "checkbox-true" (val b)))
                       (assoc a k true)
                       a)))
                 m
                 params)]
    (reduce (fn [a b] (if (b a) a (assoc a b false)))
            new-map
            key-set)))

(defn marshal-multi
  "Translate from the form representation of the submitted data to the native
  representation. This will work on fields of type multi-select, select and
  multi-checkbox. All of these fields have a list as a data source. Each
  element in the list corresponds to an option that a user may select. The
  value and visible name of the option are each a function on the element. The
  native representation is also a function of the element. For each selected
  option, this function will find the source element and then get the native
  representation."
  [m bindings params key-set type]
  (reduce (fn [form-data next-multi]
            (let [{:keys [source value data]} (bindings next-multi)
                  data (or data identity)
                  source-fn (if source
                              (let [indexed (index-by value source)]
                                (fn [x] (data (get indexed x nil))))
                              (fn [x] (keyword x)))
                  values (next-multi form-data)
                  values (cond (sequential? values)
                               (vec values)
                               (not (nil? values))
                               [values]
                               :else [])
                  mapped-values (vec (map source-fn values))
                  mapped-values (if (= type :_selects)
                                  (first mapped-values)
                                  mapped-values)]
              (assoc form-data next-multi mapped-values)))
          m
          key-set))

(defn- set-form-field-value* [form-state input-field update-fn]
  (let [field-name (:field-name input-field)
        previous-val ((keyword field-name) (:form-data form-state))]
    (if previous-val
      (update-fn previous-val (:html input-field))
      input-field)))

(defn set-input-form-field-value [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (assoc input-field :html
            (vector :input
                    (assoc
                        (last html) :value previous-value))))))

(defmulti set-form-field-value (fn [a b] (:type b)))

(defmethod set-form-field-value :default [state field]
           field)

(defmethod set-form-field-value :textfield [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :hidden [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :password [form-state input-field]
  (set-input-form-field-value form-state input-field))

(defmethod set-form-field-value :checkbox [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (let [truthy #{"Y" "y" "yes" :yes true}]
       (assoc input-field :html
             (vector :input (if (contains? truthy previous-value)
                              (assoc (last html) :checked "true")
                              (last html))))))))

(defmethod set-form-field-value :textarea [form-state input-field]
  (set-form-field-value*
   form-state
   input-field
   (fn [previous-value html]
     (assoc input-field :html (conj html previous-value)))))

(defmethod set-form-field-value :select [form-state input-field]
  (let [field-name (:field-name input-field)
        input-html (:html input-field)
        previous-val ((keyword field-name) (:form-data form-state))
        values (if (coll? previous-val)
                 previous-val
                 [previous-val])]
    (if (seq values)
      (assoc input-field :html
             (apply vector
                    (map #(if (and (vector? %)
                                   (= :option (first %))
                                   (some (fn [x]
                                           (= (:value (second %)) x))
                                         values))
                            [:option {:value (:value (second %))
                                      :selected "selected"}
                             (last %)]
                            %)
                         input-html)))
      input-field)))

(defmethod set-form-field-value :multi-select [form-state input-field]
  (set-form-field-value form-state (assoc input-field :type :select)))

(defmethod set-form-field-value :multi-checkbox [form-state input-field]
  (let [checkboxes (map last (last (:html input-field)))
        field-name (:field-name input-field)
        field-value (field-name (:form-data form-state))
        value-set (set field-value)
        new-checkboxes (map #(vector :input
                                     (let [attrs (second %)]
                                       (if (contains? value-set
                                                      (:value attrs))
                                         (assoc attrs :checked "true")
                                         attrs))
                                     (last %))
                            checkboxes)]
    (assoc input-field :html
           (wrap-checkboxes-in-group new-checkboxes))))

;;
;; Layout
;;

(defn layout-table
  "Create a table using the layout vector. The layout vector is a vector of
   integers, one for each row indicating the number of cell that go in that
   row. If the number of cells in a row is less than the max value then
   the last cell will be given a colspan value that is large enough to fill
   the table."
  [layout & cell-values]
  (loop [layout (take (count cell-values) (concat layout (repeat 1)))
         max-width (apply max layout)
         cell-values (filter #(not (nil? %)) cell-values)
         rows []]
    (if (seq cell-values)
      (let [row-cell-values (take (first layout) cell-values)
            final-colspan (+ 1 (- max-width (count row-cell-values)))
            row-values (map #(vector 1 %) row-cell-values)
            row-values (if (> final-colspan 1)
                         (concat (butlast row-values)
                                 [[final-colspan (last (last row-values))]])
                         row-values)]
        (recur (rest layout)
               max-width
               (drop (first layout) cell-values)
               (if (seq row-values)
                 (conj rows
                       (vec
                        (concat [:tr]
                                (vec
                                 (map #(let [cell-value (last %)
                                             cell-value (if (vector? cell-value)
                                                          cell-value
                                                          [cell-value])]
                                         (vec
                                          (if (> (first %) 1)
                                            (concat [:td {:valign "top"
                                                          :colspan (first %)}]
                                                    cell-value)
                                            (concat [:td {:valign "top"}]
                                                    cell-value))))
                                      row-values)))))
                 rows)))
      (vec (concat [:table] rows)))))

(defmulti create-form-field-cell (fn [state m p] (:type m)))

(defmethod create-form-field-cell :checkbox [form-state m props]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec [:div
                                   (:html (set-form-field-value form-state m))
                                   (label props)])]
    (if error-message
      [:div
       [:div.error-message error-message]
       field-row]
      field-row)))

(defmethod create-form-field-cell :multi-checkbox [form-state m props]
  (let [{:keys [_ label field-name html]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))
        label (label props)]
    (if error-message
      [:div
       [:div.error-message error-message]
       label field-row]
      [:div label field-row])))

(defmethod create-form-field-cell :default [form-state m props]
  (let [{:keys [_ label field-name html required]} m
        error-message (first (field-name form-state))
        field-row (filter-nil-vec
                   (:html (set-form-field-value form-state m)))
        id (:id (last (:html m)))
        error-id (when id {:id (str (name id) "-error")})]
    [:div.sandbar-field
      (label props (if required :required :optional))
      (if error-message
        [:div.error-message (if id error-id {}) error-message]
        [:div.error-message (merge {:style "display:none;"} error-id)])
      field-row]))

(defmulti create-hidden-field (fn [state m] (:type m)))

(defmethod create-hidden-field :hidden [form-state m]
           (:html (set-form-field-value form-state m)))

(defmethod create-hidden-field :checkbox [form-state m]
           [:input {:type "hidden"
                    :name "_checkboxes"
                    :value (:field-name m)}])

(defmethod create-hidden-field :multi-checkbox [form-state m]
           [:input {:type "hidden"
                    :name "_multi-checkboxes"
                    :value (:field-name m)}])

(defmethod create-hidden-field :multi-select [form-state m]
           [:input {:type "hidden"
                    :name "_multi-selects"
                    :value (:field-name m)}])

(defmethod create-hidden-field :select [form-state m]
           [:input {:type "hidden"
                    :name "_selects"
                    :value (:field-name m)}])

(def one-column-layout (repeat 1))

(defmulti display-form-errors (fn [& args] (first args)))

(defmethod display-form-errors :default [form-name form-errors]
           [:div.form-error-message
            (map #(vector :div %) form-errors)])

(defn form-layout-grid* [form-name layout form-state coll props]
  (let [div (if-let [form-errors (:form form-state)]
              [:div
               (display-form-errors form-name form-errors)]
              [:div])
        the-form
        (conj div 
              (apply layout-table
                     layout
                     (map #(if-let [cell (create-form-field-cell form-state
                                                                 %
                                                                 props)]
                             (vector cell)
                             nil)
                          (filter #(not (= (:type %) :hidden)) coll))))]
    (vec (concat the-form
                 (map #(create-hidden-field form-state %)
                      (let [hidden-types #{:hidden :checkbox :multi-checkbox
                                           :multi-select :select}]
                        (filter #(contains? hidden-types (:type %)) coll)))))))

(defn unbind [form request bindings fields]
  (do (info "unbind/form:")
      (pretty/pprint form))
  (let [form-data (:form-data form)
        binding-fields (filter #(contains? #{:multi-checkbox
                                             :multi-select
                                             :select}
                                           (:type %))
                               fields)]
    (merge form
           {:form-data
            (reduce (fn [form-data {:keys [field-name type]}]
                      (let [type (if (= type :multi-select)
                                   :select
                                   type)
                            {:keys [value data source]}
                            (make-bindings type request bindings field-name)
                            indexed (index-by data source)
                            value (fn [c] (->> c
                                               (get indexed)
                                               value))
                            current (field-name form-data)]
                        (assoc form-data field-name
                               (if (sequential? current)
                                 (vec (map value current))
                                 (value current)))))
                    form-data
                    binding-fields)})))

(defn form-layout-grid
  ([form-name fields request]
     (form-layout-grid one-column-layout form-name fields request {} {} {}))
  ([layout form-name fields request]
     (form-layout-grid layout form-name fields request {} {} {}))
  ([layout form-name fields request init-data bindings props]
     (let [form-state (-> (or (flash-get form-name)
                              {:form-data init-data})
                          (unbind request bindings fields))]
       (do (info "form-layout-grid/form-state:")
           (pretty/pprint form-state))
       (form-layout-grid* form-name layout form-state fields props))))

(defmulti template (fn [& args] (first args)))

(defmethod template :default [_ method action options field-table]
           (template :basic method action options field-table))

(defn form-header [form-title buttons]
  [:div {:class "form-header"}
   [:table
    [:tr
     [:td
      [:span {:class "form-title"} form-title]]
     [:td {:align "right"}
      (display-buttons "header" buttons)]]]])

(defn form-footer [buttons]
  [:div {:class "form-footer"}
   (display-buttons "footer" buttons)])

(defmethod template :over-under [_
                                 method
                                 action
                                 {:keys [buttons title attrs]}
                                 field-table]
           (let [title (or title "Your Title Goes Here")
                 buttons (or buttons [[:submit] [:cancel]])]
             [:div {:class "sandbar-form"}
             (form-to [method (cpath action) attrs]
                      (form-header title buttons)
                      field-table
                      (form-footer buttons))]))

(defmethod template :basic [_ method action {:keys [buttons attrs]} field-div]
           (let [buttons (or buttons [[:submit] [:reset]])]
             [:div {:class "sandbar-form"}
              (form-to [method (cpath action) attrs]
                       (append-buttons-to-table field-div buttons))]))

(defn- wrap-marshal-multi
  [marshal bindings params type]
  (let [multi (get-param params type)
        multi (when multi
                (set (map keyword (if (sequential? multi)
                                    multi
                                    [multi]))))]
    (if multi
      (fn [m] (-> m
                  marshal
                  (dissoc type)
                  (marshal-multi bindings params multi type)))
      marshal)))

(defn- build-marshal-function
  "Build the default marshaling function. Removes all of the temporary fields."
  [request bindings]
  (fn [params]
    (let [keys (map keyword (keys params))
          marshal (fn [m] (dissoc m :submit :* :_method))
          checkboxes (get-param params :_checkboxes)
          checkboxes (when checkboxes
                       (set (map keyword (if (sequential? checkboxes)
                                           checkboxes
                                           [checkboxes]))))
          marshal (if checkboxes
                    (fn [m] (-> m
                                marshal
                                (dissoc :_checkboxes)
                                (marshal-checkboxes params
                                                    checkboxes)))
                    
                    marshal)
          marshal (wrap-marshal-multi marshal
                                      (partial make-bindings
                                               :multi-checkbox
                                               request
                                               bindings)
                                      params
                                      :_multi-checkboxes)
          marshal (wrap-marshal-multi marshal
                                      (partial make-bindings
                                               :select
                                               request
                                               bindings)
                                      params
                                      :_multi-selects)
          marshal (wrap-marshal-multi marshal
                                      (partial make-bindings
                                               :select
                                               request
                                               bindings)
                                      params
                                      :_selects)]
      (-> (get-params keys params)
          marshal
          clean-form-input))))

(defn- submit-form
  "Handle a form post or put request."
  [request name validator options]
  (let [{:keys [on-success on-cancel bindings]} options
        {:keys [params]} request
        bindings (or bindings {})
        on-success (or on-success (constantly "/"))
        on-cancel (or on-cancel "/")
        marshal (build-marshal-function request bindings)]
    (redirect
     (replace-params (:route-params request)
      (if (form-cancelled? params)
        on-cancel
        (let [form-data (marshal params)
              failure (get (-> request :headers) "referer")]
          (do (info "marshal:")
              (pretty/pprint form-data))
          (if-valid validator form-data
                    on-success
                    (store-errors-and-redirect name failure))))))))

(defn- set-required
  "Use the provided validator to set which fields are required."
  [fields validator]
  (let [required (set (keys (validation-errors (validator {}))))]
    (vec (map #(if (contains? required (:field-name %))
                 (assoc % :required true)
                 %)
              fields))))

(defn- actualize-html
  "The html value for each field can either be an html data structure or a
  function of the request which generates that structure. This function will
  ensure that all such functions have been called and that all html values
  are generated."
  [fields request bindings properties]
  (vec (map #(let [html (:html %)
                   html (if (fn? html)
                          (html request bindings properties)
                          html)]
               (assoc % :html html))
            fields)))

(defn- show-form
  "Handle a form get request."
  [request name form-data validator options]
  (let [{:keys [load bindings defaults buttons title layout fields style
                properties create-method update-method create-action
                update-action]}
        options
        {:keys [params uri]} request
        bindings (or bindings {})
        properties (or properties {})
        load (or load (constantly nil))
        defaults (if defaults
                   (cond (map? defaults) defaults
                         (fn? defaults) (defaults request)
                         :else {})
                   {})
        route-params (:route-params request)
        id (get route-params "id")
        form-data (cond id (or form-data (load id))
                        defaults defaults
                        :else {})
        buttons (if buttons
                  {:buttons buttons}
                  {:buttons [[:submit] [:cancel]]})
        attrs (merge buttons
                     (when title {:title title}))
        layout (or layout [1])
        fields (-> (cond (vector? fields) fields
                         (fn? fields) (fields request form-data))
                   (set-required validator)
                   (actualize-html request bindings properties))
        style (or style :default)
        update-action (if (fn? update-action)
                        (update-action request)
                        update-action)
        create-action (if (fn? create-action)
                        (create-action request)
                        create-action)
        method (cond (and id update-method) update-method
                     create-method create-method
                     :else :post)
        action (replace-params route-params
                (cond (and id update-action) update-action
                      create-action create-action
                      :else uri))]
    (template style method action attrs
              (form-layout-grid layout
                                name
                                fields
                                request
                                form-data
                                bindings
                                properties))))

(defn make-form
  "Create a form handler function. The resulting function is a function of the
  request and optionally the form data. All options have reasonable default
  values.

  Any useful form should include:

  fields:     Either a vector of fields or a function of the request and form
              data which returns a vector of fields. Defaults to nil.
  load:       A function of the id that will load the form data. id is
              (get (:route-params request) 'id'). Defaults to a function that
              returns nil.
  on-success: A function of the validated form data which returns the uri to be
              redirected to. Defaults to (constantly \"/\").
  on-cancel:  The uri to be redirected to when the cancel button in pushed.
              Defaults to \"/\".
  bindings:   When using multi-select, multi-checkbox and select fields you
              need to configure the data source for each field. bindings is a
              map of field names to data sources where a data source is defined
              as:
              {:value name :visible identity :source (fn [request] ...)
               :data identity}
              source can be a function or a literal list. data will return
              the native representation. value and visible are what appear in
              the html.

  For makeing RESTful forms use:

  create-method: The request method to use when creating a new resource.
  update-method: The request method to use when updating a resource.
  create-action: The uri to use when creating a new resource. May be a string
                 or a function of the request.
  update-action: The uri to use when updating a resource. May be a string or
                 a function of the request.

  Optionally you may often use:

  properties: A map which will be used to look up field labels to be displayed
              on the form.
  validator:  A function of a map which returns a validated map. Defaults to
              identity.
  defaults:   A map of the default form values or a function of the request
              which returns a map of default values.
  buttons:    A sequence of form buttons. Defaults to [[:submit] [:cancel]]
  layout:     A vector which indicates the field layout. Defaults to [1].

  On rare occations you will need:

  style:      The form style. Defaults to :default but could also be :over-under
  title:      A function of the request and form data which returns the
              current form title. May also be a String. It is used when
              the style requires a title and returned in the resulting
              response map.

  Example usage:

  (def names-form
     (make-form :names-form
       :fields [(textfield :my-name :label \"My Name\")]
       :load #(fetch %)
       :on-success #(do (save %)
                        \"/landing/page/\")
       :on-cancel \"/landing/page\"
       :create-action \"/names\"
       :update-action \"/names/:id\"
       :update-method :put))

  (defroutes my-routes
    (GET \"/names/new\" request (layout (names-form request)))
    (POST \"/names\" request (simple-form request))
    (GET \"/names/:id/edit\" request (layout (simple-form request)))
    (PUT \"/names/:id\" request (simple-form request)))"
  [name & {:keys [validator title] :as options}]
  (fn [request & [form-data]]
    (let [{:keys [request-method]} request
          title (cond (string? title) title
                      (fn? title) (title request)
                      :else nil)
          options (assoc options :title title)
          validator (or validator identity)
          response (if (some #{request-method} [:post :put])
                     (submit-form request name validator options)
                     (show-form request name form-data validator options))]
      (if (map? response)
        response
        {:title title
         :body response}))))

(defmacro defform [name & options]
  "Define a form handler function. The name may optionally be
  followed by a doc-string and metadata map. See make-form for details."
  (let [[name options] (name-with-attributes name options)]
    `(def ~name (make-form ~(keyword name) ~@options))))

(defn make-rest-form
  [name & {:keys [resource page-layout] :as options}]
  (let [page-layout (or page-layout identity)
        create-action resource
        update-action (str resource "/:id")
        update-method :put
        new-action (str resource "/new")
        edit-action (str resource "/:id/edit")
        options (-> options
                    (dissoc :resource :page-layout)
                    (merge {:create-action create-action
                            :update-action update-action
                            :update-method update-method}))
        form-handler (apply make-form name (interleave (keys options)
                                                       (vals options)))]
    (routes
     (GET new-action request (page-layout (form-handler request)))
     (POST resource request (form-handler request))
     (GET edit-action request (page-layout (form-handler request)))
     (PUT update-action request (form-handler request)))))

(defmacro defrestform [name & options]
  "Define a form handler function wrapped in a RESTful routing function. The
  name may optionally be followed by a doc-string and metadata map. See
  make-rest-form for details."
  (let [[name options] (name-with-attributes name options)]
    `(def ~name (make-rest-form ~(keyword name) ~@options))))

