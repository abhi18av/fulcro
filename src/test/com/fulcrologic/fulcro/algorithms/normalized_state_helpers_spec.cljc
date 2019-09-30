(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers-spec
  (:require
    [fulcro-spec.core :refer [assertions specification component when-mocking behavior]]
    [com.fulcrologic.fulcro.algorithms.normalized-state-helpers :as nsh]))


;;============================================================================
;; helpers

(def normalized-state {:car/id      {1 {:car/id 1, :car/model "T-150"},
                                     2 {:car/id 2, :car/model "Ferrari"},
                                     3 {:car/id 3, :car/model "Mercedez"}},
                       :person/id   {1 {:person/id     1,
                                        :person/name   "Joe",
                                        :person/age    24,
                                        :person/spouse [:person/id 2],
                                        :person/cars   [[:car/id 1] [:car/id 2]]},
                                     2 {:person/id     2,
                                        :person/name   "Dafny",
                                        :person/age    21,
                                        :person/spouse [:person/id 2],
                                        :person/cars   [[:car/id 3]]}},
                       :root/person [[:person/id 1] [:person/id 2]]})


(def denormalized-state-to-one {:root/person {:person/id     1,
                                              :person/name   "Joe",
                                              :person/age    24,
                                              :person/spouse {:person/id   2,
                                                              :person/name "Dafny",
                                                              :person/age  21,
                                                              :person/cars [{:car/id    3,
                                                                             :car/model "Mercedez"}]}
                                              :person/cars   [{:car/id 1, :car/model "T-150"}
                                                              {:car/id 2, :car/model "Ferrari"}]}})

(def denormalized-state-to-many {:root/person [{:person/id     1,
                                                :person/name   "Joe",
                                                :person/age    24,
                                                :person/spouse {:person/id   2,
                                                                :person/name "Dafny",
                                                                :person/age  21,
                                                                :person/cars [{:car/id    3,
                                                                               :car/model "Mercedez"}]}
                                                :person/cars   [{:car/id 1, :car/model "T-150"}
                                                                {:car/id 2, :car/model "Ferrari"}]}
                                               {:person/id   3,
                                                :person/name "Billy",
                                                :person/age  26,
                                                :person/cars [{:car/id 4, :car/model "Tesla"}]}]})




;;============================================================================




(specification "tree-path->db-path" :focus
  (behavior "In normalized DB, when root key is a to-many list of idents"
    (let [state normalized-state]
      (assertions
        (nsh/tree-path->db-path state [:person/id 1 :person/spouse :person/name])
        => [:person/id 2 :person/name])))

  (behavior "In normalized DB, follows idents ending with to-many relations"
    (let [state normalized-state]
      (assertions
        (nsh/tree-path->db-path state [:person/id 1 :person/spouse :person/cars :car/id])
        => [:person/id 2 :person/cars :car/id])))

  (behavior "In normalized DB, returns not-found when the path doesn't exist"
    (let [state normalized-state]
      (assertions
        (nsh/tree-path->db-path state [:root/person 4 :person/id] "not-found")
        => "not-found")))

  #_(behavior "In denormalized DB, follows idents ending with to-many relations"
              (let [state denormalized-state]
                ;; TODO [:table-key 3 :to-one-field :attr]
                (assertions
                  (nsh/tree-path->db-path state [:car/id 1 :car/model])
                  => [:person/id 2 :person/first-name])))

  )

;;============================================================================

(specification "get-in" :focus
  (behavior "Follows edges from root that are denormalized"
    (let [state {:a [:b 1]
                 :b {1 {:c [:d 1]}}
                 :d {1 {:value 42}}}]
      (assertions
        (nsh/get-in state [:a :c :value]) => 42)))

  (behavior "Behaves like a normal clojure get-in"
    (let [state {:a {:c {:value 42}}}]
      (assertions
        (nsh/get-in state [:a :c :value]) => 42)))

  ;; FIXME
  (behavior "Follows edges from root that are denormalized"
    (let [state normalized-state]
      (assertions
        (nsh/get-in normalized-state [:person/id 1 :person/spouse :person/name]) => "Dafny")))

  (behavior "Returns not-found when provided this option"
    (let [state normalized-state]
      (assertions
        (nsh/get-in state [:person/id 3 :person/spouse :person/name] "not-found") => "not-found")))


  )


;;============================================================================

(specification "remove-entity*"
  (behavior "1"
    (let [state {:a [:b 1]
                 :b {1 {:c [:d 1]}}
                 :d {1 {:value 42}}}]
      (assertions
        (nsh/get-in state [:a :c :value]) => 42)))
  )


;============================================================================

(specification "remove-edge*"
  (behavior "1"
    (let [state {:a [:b 1]
                 :b {1 {:c [:d 1]}}
                 :d {1 {:value 42}}}]
      #_(assertions
          (nsh/get-in state [:a :c :value]) => 42)))
  )

;============================================================================



(specification "ui->props"
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


(specification "update-caller-in!"
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



(specification "swap!->"
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



(comment
  (def env {:state (atom {})})
  (nsh/swap!-> env
               (assoc :x 1)
               (update :x inc))

  env

  '())

