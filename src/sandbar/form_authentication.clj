;; Copyright (c) Brenton Ashworth. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file COPYING at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns sandbar.form-authentication
  "Provide support for form based authentication."
  (:use [ring.util [response :only [redirect]]]
        [compojure.core]
        [sandbar.auth :only [logout!
                             create-authenticator]]
        [sandbar.stateful-session :only [session-get
                                         session-put!
                                         flash-put!
                                         session-delete-key!]]
        [sandbar.core :only [property-lookup]]
        [sandbar.validation :only [build-validator
                                   non-empty-string
                                   if-valid]]
        [sandbar.forms :only [form-layout-grid
                              textfield
                              password
                              template
                              get-params
                              store-errors-and-redirect]]))

(defprotocol FormAuthAdapter
  "Protocol for processing form authenticaion credentials."
  (load-user [this username password] "Load user information based on the
    provided username and password. Return a map which contains at least a
    username and a set of roles under the keys :username and :roles.")
  (validate-password [this] "Create a validator function that will be
    used to validate the user's password. The user passed to this function
    will have been loaded from load-user."))

(defn form-authentication [request]
  (do (session-put! :auth-redirect-uri
                    (:uri request))
      (redirect "/login")))

(defn login-page [adapter request]
  (template :default
            :post
            (:uri request)
            {:buttons [[:submit "Login"] [:reset]]}
            (form-layout-grid [1 1]
                              :login
                              [(textfield :username :size 25 :required true)
                               (password :password :size 25 :required true)]
                              request
                              {}
                              {}
                              adapter)))

(defn login-validator [adapter]
  (let [pw-validator (validate-password adapter)]
    (build-validator (non-empty-string :username :password adapter)
                     :ensure
                     pw-validator)))

(defn authenticate! [adapter params]
  (let [input (get-params [:username :password] params)
        user-data (load-user adapter (str (:username input)) (str (:password input)))
        success (or (session-get :auth-redirect-uri)
                    (property-lookup adapter :login-page))
        failure "login"]
    (redirect
     (if-valid (login-validator adapter) user-data
               #(do
                  (session-put! :current-user
                                {:name (:username %)
                                 :roles (:roles %)})
                  (session-delete-key! :auth-redirect-uri)
                  success)
               #(do (flash-put!
                     :login
                     (merge {:form-data (dissoc (into {} %1)
                                                :username
                                                :password)}
                            %2))
                    failure)))))

(defn form-authentication-routes
  ([layout adapter]
     (form-authentication-routes "" layout adapter))
  ([path-prefix layout adapter]
     (routes
      (GET (str path-prefix "/login*") request
           (layout request
                   (login-page adapter request)))
      (POST (str path-prefix "/login*") {params :params}
            (authenticate! adapter params))
      (GET (str path-prefix "/logout*") []
           (logout! adapter)))))

(defmethod create-authenticator :form [type n options]
  (let [record (gensym "form_adapter_")
        ctor (gensym "form_adapter_ctor_")
        load (or (:load options) `(fn [u# p#] nil))
        validate (or (:validator options) `identity)
        properties (or (:properties options) {})]
    `(do
       (defrecord ~record []
         sandbar.form-authentication/FormAuthAdapter
         (sandbar.form-authentication/load-user [this# username# password#]
                                                (~load username# password#))
         (sandbar.form-authentication/validate-password [this#]
                                                        ~validate))
       (defn ~ctor []
         (merge (new ~record) ~properties))
       (defn ~n [arg#]
         (if (map? arg#)
           (sandbar.form-authentication/form-authentication arg#)
           (sandbar.form-authentication/form-authentication-routes
            arg#
            (~ctor)))))))
