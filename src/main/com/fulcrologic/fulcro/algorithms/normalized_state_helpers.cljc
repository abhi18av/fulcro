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

         candidate-paths (fn [state table-name]
                           (filter (fn [a-path]
                                     (let [vl (clojure.core/get-in state a-path)]
                                       (if (coll? vl)
                                         (or
                                           (some #{ident} vl)
                                           (= ident vl))
                                         (= ident (take 2 a-path)))))
                                   (normalized-paths state)))

         remove-ident-from-table (fn [state1 table]
                                   (reduce
                                     remove-idents-at-path
                                     state1
                                     (concat (candidate-paths state1 (first ident)) non-tables)))

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

;;================================

(>defn remove-edge*
  "Remove the given edge at the given path. Also scans all tables and removes any to-one or to-many idents that are
  found that match `edge` (removes dangling pointers to the removed entity(ies).

  The optional `cascade` parameter is a set of keywords that represent edges that should cause recursive deletes
  (i.e. it indicates edge names that *own* something, indicating it is safe to remove those entities as well).

  Returns the new state map with the entity(ies) removed."

  ([state-map path-to-edge]
   [map? vector? => any?]
   (remove-edge* state-map path-to-edge #{}))


  ([state-map path-to-edge cascade]
   [map? vector? (s/coll-of keyword? :kind set?) => map?]
   (let [candidate (let [vl (clojure.core/get-in state-map path-to-edge)]
                     (if-not (vector? vl)
                       nil
                      (cond
                        (eql/ident? vl) [vl]
                        (every? eql/ident? vl) vl)))
         final-state (if (some #{path-to-edge} (normalized-paths state-map))
                       (reduce
                         #(remove-entity* %1 %2 cascade)
                         state-map
                         candidate)
                       state-map)]
     final-state)))


(comment

  (let [state {:fastest-car    [:car/id 1]
               :denorm         {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                                    :b [:person/id 1]}}}
               :favourite-cars [[:car/id 1] [:car/id 2]]
               :car/id         {1 {:car/registered-owner [:person/id 1]}
                                2 {:car/registered-owner [:person/id 1]}}
               :person/id      {1 {:person/age        42
                                   :person/cars       [[:car/id 1] [:car/id 2]]
                                   :person/address    [:address/id 1]
                                   :alternate-address [:address/id 2]}}
               :address/id     {1 {:address/state "Oregon"}
                                2 {:address/state "Idaho"}}}]

    ;(normalized-paths state)
    ;;NOTE passes here since strings are `seq` of chars
    (remove-edge* state [:address/id 1 :address/state])      ;=> state
    ;;NOTE fails with numbers
    ;(remove-edge* state [:person/id 1 :person/age])      ;=> state
    ;;;;;
    ;;DONE
    ;"Refuses to remove a denormalized edge"
    ;(remove-edge* state [:denorm :level-1 :level2 :b])      ;=> state
    ;;DONE
    ;"Removes top-level to-one edge"
    ;(remove-edge* state [:fastest-car])                     ;=> nil
    ;;DONE
    ;"Removes top-level to-many edge"
    ;(remove-edge* state [:favourite-cars])                  ;=> nil
    ;;DONE
    ;"Removes table-nested to-one edge"
    ;(remove-edge* state [:person/id 1 :person/address])     ;=> nil
    ;;DONE
    ;"Removes table-nested to-many edge"
    ;(remove-edge* state [:person/id 1 :person/cars])        ;=> nil
    )




  (let [state {:person/id      {1 {:person/id   1
                                   :latest-car  [:car/id 2]
                                   :person/cars [[:car/id 1] [:car/id 2]]}
                                2 {:person/id 2}}
               :fastest-car    [:car/id 1]
               :favourite-cars [[:car/id 1] [:car/id 2]]
               :car/id         {1 {:car/id     1
                                   :car/engine [:engine/id 1]}
                                2 {:car/id     2
                                   :car/engine [:engine/id 2]}}
               :engine/id      {1 {:engine/id 1}
                                2 {:engine/id 2}}}]
    ;;DONE
    ;"Removes top-level to-one edge"
    ;(remove-edge* state [:fastest-car] #{:car/engine})  ;=> true
    ;;DONE
    ;"Removes top-level to-many edge"
    ;(remove-edge* state [:favourite-cars] #{:car/engine}) ;=> true
    ;;DONE
    ;"Removes table-nested to-one edge"
    ;(remove-edge* state [:person/id 1 :latest-car] #{:car/engine}) ;=> true
    ;;DONE
    ;"Removes table-nested to-many edge"
    ;(remove-edge* state [:person/id 1 :person/cars] #{:car/engine}) ;=> true
    )




  (let [state {:person/id      {1 {:person/id   1
                                   :latest-car  [:car/id 2]
                                   :person/cars [[:car/id 1] [:car/id 2]]}}
               :fastest-car    [:car/id 1]
               :favourite-cars [[:car/id 1] [:car/id 2]]
               :car/id         {1 {:car/id     1
                                   :car/colors [[:color/id 1]]}
                                2 {:car/id     2
                                   :car/colors [[:color/id 1]
                                                [:color/id 2]]}}
               :color/id       {1 {:color/id 1}
                                2 {:color/id 2}}}]

    ;;DONE
    ;"Removes top-level to-one edge"
    ;(remove-edge* state [:fastest-car] #{:car/colors})      ;=> true
    ;;DONE
    ;"Removes top-level to-many edge"
    ;(remove-edge* state [:favourite-cars] #{:car/colors}) ;=> true
    ;;DONE
    ;"Removes table-nested to-one edge"
    ;(remove-edge* state [:person/id 1 :latest-car] #{:car/colors}) ;=> true
    ;;DONE
    ;"Removes table-nested to-many edge"
    ;(remove-edge* state [:person/id 1 :person/cars] #{:car/colors}) ;=> true
    )


  '())



;;============================================================================

;; TODO clarify the exact usage
(>defn sort-idents-by
  "
  Intended to be used as
   ```
   (sort-idents-by :entity/field vector-of-idents)
   ```
  Can facilitate:
  ```
  (swap! state update-in [:entity 1 :list] sort-idents-by :list/field)
  ```
  "
  [entity-field vector-of-idents]
  [keyword? vector? => any?]
  (sort-by second vector-of-idents))

(comment

  (def state (atom {:grandparents [[:person/id 3] [:person/id 2]]
                    :person/id    {1 {:person/name     "person-1"
                                      :person/children [[:person/id 3]
                                                        [:person/id 9]
                                                        [:person/id 5]]}
                                   2 {:person/name "person-2"
                                      :person/cars [[:car/id 1]
                                                    [:car/id 2]]}}
                    :car/id       {1 {:car/model "model-1"}
                                   2 {:car/model "model-2"}}}))


  (sort-by second (clojure.core/get-in @state [:person/id 1 :person/children]))

  (swap! state update-in [:person/id 1 :person/children] sort-idents-by :person/id)

  '())


;============================================================================

;;MAYBE These might belong in mutation ns???
(defn update-caller!
  "Runs clojure.core/update on the table entry in the state database that corresponds
   to the mutation caller (which can be explicitly set via `:ref` when calling `transact!`).
   Equivalent to `(swap! (:state env) update-in (:ref env) ...)`."
  [{:keys [state ref] :as mutation-env} & args]
  (apply swap! state update-in ref args))


(comment


  (let [mutation-env {:ref   [:person/id 1]
                      :state (atom {:person/id {1
                                                {:person/id 1 :person/name "Dad"}}})}]
    (apply swap! (:state mutation-env) update-in (:ref mutation-env)
           (vector assoc :person/name "Mom")))



  (let [mutation-env {:ref   [:person/id 1]
                      :state (atom {:person/id {1
                                                {:person/id 1 :person/name "Dad"}}})}]
    (update-caller! mutation-env
                    assoc :person/name "Mom"))




  '())

;============================================================================

;;MAYBE These might belong in mutation ns???
(defn update-caller-in!
  "Like swap! but starts at the ref from `env`, adds in supplied `path` elements
  (resolving across idents if necessary). Finally runs an update-in on that resultant
  path with the given `args`.
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



(comment

  (let [state (atom {:person/id {1 {:person/id       1 :person/name "Dad"
                                    :person/children [[:person/id 2] [:person/id 3]]}
                                 2 {:person/id 2 :person/name "Son"}
                                 3 {:person/id 3 :person/name "Daughter"}}})
        ref [:person/id 1]
        path (tree-path->db-path @state (into ref [:person/id 2]))
        args (vector assoc :person/name "Mom")]

    (if (and path (get-in @state path))
      (apply swap! state update-in path args)
      @state))





  (let [state (atom {:person/id {1 {:person/id       1 :person/name "Dad"
                                    :person/children [[:person/id 2] [:person/id 3]]}
                                 2 {:person/id 2 :person/name "Son"}
                                 3 {:person/id 3 :person/name "Daughter"}}})
        ref [:person/id 1]
        path (tree-path->db-path @state (into ref [:person/id 2]))
        args (vector assoc :person/name "Mom")]

    (and path (get-in @state path)))




  ;;;;;


  '())

;============================================================================

;; TODO: Untested...make up an env with a state atom and see if it works in clj/cljs

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




(comment
  (def env {:state (atom {})})
  (swap!-> env
           (assoc :x 1)
           (update :x inc))
  (assoc :x 1)
  (update :x inc))

