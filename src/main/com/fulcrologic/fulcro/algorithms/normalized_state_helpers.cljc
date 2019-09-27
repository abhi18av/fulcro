(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers
  "Functions that can be used against a normalized Fulcro state database."
  #?(:cljs (:require-macros com.fulcrologic.fulcro.algorithms.normalized-state-helpers))
  (:refer-clojure :exclude [get-in])
  (:require
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
  [state path]
  [map? vector? => vector?]
  (loop [[h & t] path
         new-path []]
    (if h
      (let [np (conj new-path h)
            c  (clojure.core/get-in state np)]
        (if (eql/ident? c)
          (recur t c)
          (recur t (conj new-path h))))
      new-path)))

(>defn get-in
  "Just like clojure.core/get-in, but if an element of the path is an ident it will follow the ident instead."
  ([state-map path]
   [map? vector? => any?]
   (get-in state-map path nil))
  ([state-map path not-found]
   [map? vector? any? => any?]
   (clojure.core/get-in state-map (tree-path->db-path state-map path) not-found)))

;; TODO: see deep-remove-entity* (from incubator). I think the incubator implementation is technically insufficient. GC
;; should check/clean all tables, but only one level deep (not a walk). Also, the cascading would be useful to implement.
(>defn remove-entity*
  "Remove the given entity at the given ident. Also scans all tables and removes any to-one or to-many idents that are
   found that match `ident` (removes dangling pointers to the removed entity).  The optional `cascade` parameter is a
   set of keywords that represent edges that should cause recursive deletes (i.e. it indicates edge names that *own*
   something, indicating it is safe to remove those entities as well).

   Returns the new state map with the entity(ies) removed."
  ([state-map ident]
   [map? eql/ident? => map?])
  ([state-map ident cascade]
   [map? eql/ident? (s/coll-of keyword? :kind set?) => map?]))

;; TODO: path-to-edge...should that be tree-path or db-path?
;; TODO: needs cascade argument like remove-entity*
;; NOTE: This is pretty much remove-ident*, but with the option of recursively removing the stuff
;; pointed to...might need a better name.
(defn remove-edge* [state-map path-to-edge]
  )

(defn ui->props
  "Obtain a tree of props for a UI instance from the current application state. Useful in mutations where you want
  to denormalize an entity from the state database. `this` can often be obtained from the mutation `env` at the
  `:component` key."
  ([this]
   (ui->props (comp/component->state-map this) (comp/react-type this) (comp/get-ident this)))
  ([state-map component-class ident]
   (fdn/db->tree (comp/get-query component-class state-map) (get-in state-map ident) state-map)))

;; These might belong in mutation ns???
(defn update-caller!
  "Runs clojure.core/update on the table entry in the state database that corresponds to the mutation caller (which
   can be explicitly set via `:ref` when calling `transact!`).

   Equivalent to `(swap! (:state env) update-in (:ref env) ...)`."
  [{:keys [state ref] :as mutation-env} & args]
  (apply swap! state update-in ref args))

(defn update-caller-in!
  "Like swap! but starts at the ref from `env`, adds in supplied `path` elements (resolving across idents if necessary).
   Finally runs an update-in on that resultant path with the given `args`.
   Roughly equivalent to:

   ```
   (swap! (:state env) update-in (tree-path->db-path @state (into (:ref env) path)) args)
   ```

   with a small bit of additional sanity checking."
  [{:keys [state ref] :as mutation-env} path & args]
  (let [path (tree-path->db-path @state (into ref path))]
    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state)))

;; aliases to integrate-ident* and remove-ident*, with doc strings...

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
  "
  [state-map ident path-to-idents]

  Removes an ident, if it exists, from a list of idents in app state. This
  function is safe to use within mutations."
  merge/remove-ident*)

;; TODO: Untested...make up an env with a state atom and see if it works in clj/cljs
;; TODO: better name???
#?(:clj
   (defmacro swap!->
     "A macro that is equivalent to:

     ```
     (swap! (:state env) (fn [s] (-> s ...forms...)))
     ```

     E.g.

     ```
     (swap!-> env
       (merge/merge-component ...)
       (integrate-ident* ...))
     ```


     "
     [mutation-env & forms]
     `(swap! (:state ~mutation-env) (fn [s#]
                                      (-> s#
                                        ~@forms)))))
