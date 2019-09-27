(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers-spec
  (:require
    [fulcro-spec.core :refer [assertions specification component when-mocking behavior]]
    [com.fulcrologic.fulcro.algorithms.normalized-state-helpers :as nsh]))

;; TASK: Tests for tree-path->db-path
;; to-one and to-many (e.g. resolving things like:
;; [:root-key 3] when root key is a to-many list of idents
;; [:table-key 3 :to-one-field :attr]
;; Make sure that a path that does not go through idents still resolves correctly (in case of denormalized state)

(specification "get-in"
  (behavior "Follows edges from root that are denormalized"
    (let [state {:a [:b 1]
                 :b {1 {:c [:d 1]}}
                 :d {1 {:value 42}}}]
      (assertions
        (nsh/get-in state [:a :c :value]) => 42)))
  (behavior "Follows edges from root that are denormalized"
    (let [state {:a {:c {:value 42}}}]
      (assertions
        (nsh/get-in state [:a :c :value]) => 42)))
  ;; What other cases can you think of?  What about the support for not-found?, etc.
  )

;; tests others

(comment
  (def env {:state (atom {})})
  (nsh/swap!-> env
    (assoc :x 1)
    (update :x inc))

  env
  )
