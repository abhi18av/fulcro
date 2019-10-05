(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
    [clojure.walk :as walk]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    #?(:clj  [com.fulcrologic.fulcro.dom-server :as dom]
       :cljs [com.fulcrologic.fulcro.dom :as dom])
    [edn-query-language.core :as eql]
    [clojure.spec.alpha :as s]
    [ghostwheel.core :refer [>defn =>]]
    [com.fulcrologic.fulcro.algorithms.data-targeting :as targeting]
    [com.fulcrologic.fulcro.algorithms.denormalize :as fdn]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]
    [com.fulcrologic.fulcro.components :as comp]))


(>defn tree-path->db-path
  "Convert a 'denormalized' path into a normalized one by walking the path in state and honoring ident-based edges.

  For example, one might find this to be true for a normalized db:

  ```
  state => {:person/id {1 {:person/id 1 :person/spouse [:person/id 3]}
                        3 {:person/id 3 :person/first-name ...}}}

  (tree-path->db-path state [:person/id 1 :person/spouse :person/first-name])
  => [:person/id 3 :person/first-name]
  ```
  "
  ([state path]
   [map? vector? => vector?]
   (loop [[h & t] path
          new-path []]
     (if h
       (let [np (conj new-path h)
             c (clojure.core/get-in state np)]
         (if (eql/ident? c)
           (recur t c)
           (recur t (conj new-path h))))
       (if (not= path new-path)
         new-path
         path)))))

(>defn get-in
  "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
  ([state-map path]
   [map? vector? => any?]
   (get-in state-map path nil))

  ([state-map path not-found]
   [map? vector? any? => any?]
   (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))


(defn ui->props
  "Obtain a tree of props for a UI instance from the current application state. Useful in mutations where you want
  to denormalize an entity from the state database. `this` can often be obtained from the mutation `env` at the
  `:component` key."
  ([this]
   (ui->props (comp/component->state-map this) (comp/react-type this) (comp/get-ident this)))
  ([state-map component-class ident]
   (fdn/db->tree (comp/get-query component-class state-map) (get-in state-map ident) state-map)))

(def integrate-ident*
  "[state ident & named-parameters]

  Integrate an ident into any number of places in the app state. This function is safe to use within mutation
  implementations as a general helper function.

  The named parameters can be specified any number of times. They are:

  - append:  A vector (path) to a list in your app state where this new object's ident should be appended. Will not append
  the ident if that ident is already in the list.
  - prepend: A vector (path) to a list in your app state where this new object's ident should be prepended. Will not place
  the ident if that ident is already in the list.
  - replace: A vector (path) to a specific location in app-state where this object's ident should be placed. Can target a to-one or to-many.
   If the target is a vector element then that element must already exist in the vector.

  NOTE: `ident` does not have to be an ident if you want to place denormalized data.  It can really be anything.

  Returns the updated state map."
  targeting/integrate-ident*)

(def remove-ident*
  " [state-map ident path-to-idents]

  Removes an ident, if it exists, from a list of idents in app state. This
  function is safe to use within mutations."
  merge/remove-ident*)

