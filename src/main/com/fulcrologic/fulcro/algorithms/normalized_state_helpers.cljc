(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
    [clojure.set :as set]
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


(defn- dissoc-in
  "Dissociates an entry from a nested associative structure returning a new
             nested structure. keys is a sequence of keys. Any empty maps that result
             will not be present in the new structure."
  [m [k & ks :as keys]]
  (if ks
    (if-let [nextmap (get m k)]
      (let [newmap (dissoc-in nextmap ks)]
        (if (seq newmap)
          (assoc m k newmap)
          (dissoc m k)))
      m)
    (dissoc m k)))


(defn- normalized-paths
  "Walks the tree in a depth first manner and returns the normalized possible paths"
  [m]
  (letfn [(paths* [ps ks m]
            (reduce-kv
              (fn [ps k v]
                (if (map? v)
                  (paths* ps (conj ks k) v)
                  (conj ps (conj ks k))))
              ps
              m))]
    (filter #(< (count %) 4)
            (paths* () [] m))))


(>defn remove-entity*
  "Remove the given entity at the given ident. Also scans all tables and removes any to-one or to-many idents that are
  found that match `ident` (removes dangling pointers to the removed entity).

  The optional `cascade` parameter is a set of keywords that represent edges that should cause recursive deletes
  (i.e. it indicates edge names that *own* something, indicating it is safe to remove those entities as well).

  Returns the new state map with the entity(ies) removed."

  ([state-map ident]
   [map? eql/ident? => map?]
   (remove-entity* state-map ident #{}))

  ([state1 ident cascade]
   [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]
   (let [tables (keep (fn [k]
                        (let [candidate (get state1 k)]
                          (when (and (map? candidate) (every? map? (vals candidate)))
                            k))) (keys state1))

         non-tables (keep (fn [k]
                            (let [candidate (get state1 k)]
                              (when (vector? candidate)
                                [k])))
                          (keys state1))

         remove-idents-at-path (fn [state1 path]
                                 (let [v (clojure.core/get-in state1 path)]
                                   (cond
                                     (int? v) state1
                                     (= v ident) (dissoc-in state1 path)
                                     (every? eql/ident? v) (merge/remove-ident* state1 ident path)
                                     :else state1)))

         candidate-paths (fn [state1 table-name]
                           (filter (fn [a-path]
                                     (= table-name (first a-path)))
                                   (normalized-paths state1)))

         remove-ident-from-table (fn [state1 table]
                                   (reduce
                                     remove-idents-at-path
                                     state1
                                     (concat (candidate-paths state1 :person/id) non-tables)))

         state-without-entity (->
                                ;; remove the (non) table-nested pointers to the entity
                                (reduce remove-ident-from-table
                                        state1
                                        tables)
                                ;; remove the top-level entity
                                (dissoc-in ident))

         target-entity (get-in state1 ident)

         cascaded-idents (fn [original-state target-entity cascade]
                           (map
                             (fn [x] (clojure.core/get-in original-state
                                                          (conj [(first ident) ((first ident) target-entity)] x)))
                             (set/intersection
                               cascade
                               (set (keys target-entity)))))

         final-state (reduce
                       (fn [s edge]
                         (if (every? eql/ident? edge)
                           (reduce (fn [s2 ident] (remove-entity* s2 ident cascade)) s edge)
                           (remove-entity* s edge cascade)))
                       state-without-entity
                       (cascaded-idents state1
                                        target-entity
                                        cascade))]

     final-state)))
