(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers-spec
  (:require
    [fulcro-spec.core :refer [assertions specification component when-mocking behavior =>]]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.algorithms.normalized-state-helpers :as nsh]))

(specification "tree-path->db-path"

  (behavior "Resolves top-level to-one references"
    (let [state {:fastest-car [:car/id 1]
                 :car/id      {1 {:car/model "model-1"}
                               2 {:car/model "model-2"}}}]
      (assertions
        (nsh/tree-path->db-path state [:fastest-car])
        => [:car/id 1])))

  (behavior "Resolves top-level to-many references"
    (let [state {:grandparents [[:person/id 1] [:person/id 2]]
                 :person/id    {1 {:person/name "person-1"}
                                2 {:person/name "person-2"}}}]
      (assertions
        (nsh/tree-path->db-path state [:grandparents 1])
        => [:person/id 2])))

  (behavior "Resolves table-nested to-one references"
    (let [state {:person/id {1 {:person/name  "person-1"
                                :person/email [:email/id 1]}
                             2 {:person/name  "person-2"
                                :person/email [:email/id 2]}}
                 :email/id  {1 {:email/provider "Google"}}}]
      (assertions
        (nsh/tree-path->db-path state [:person/id 1 :person/email])
        => [:email/id 1])))

  (behavior "Resolves table-nested to-many references"
    (let [state {:person/id {1 {:person/name "person-1"
                                :person/cars [[:car/id 1] [:car/id 2]]}
                             2 {:person/name "person-2"
                                :person/cars [[:car/id 1]]}}
                 :car/id    {1 {:car/model "model-1"}
                             2 {:car/model "model-2"}}}]
      (assertions
        (nsh/tree-path->db-path state [:person/id 1 :person/cars 0])
        => [:car/id 1]))))

(specification "get-in"
  (behavior "Follows edges from root that are normalized"
    (let [state {:person/id {1 {:person/name "person-1"}
                             2 {:person/name "person-2"}}}]
      (assertions
        (get-in state [:person/id 1 :person/name]) => "person-1")))

  (behavior "Behaves like clojure.core/get-in for denormalized data"
    (let [denorm-data {:a [[:b 1]] :b [:b 1]}
          state {:denorm {:level-1 {:level-2 denorm-data}}}]
      (assertions
        (nsh/get-in state [:denorm :level-1 :level-2]) => denorm-data)))

  (behavior "Returns nil when the value isn't found"
    (let [state {:person/id {1 {:person/name "person-1"
                                :person/cars [[:car/id 1] [:car/id 2]]}
                             2 {:person/name "person-2"
                                :person/cars [[:car/id 1]]}}}]
      (assertions
        (nsh/get-in state [:person/id 1 :person/email]) => nil
        (nsh/get-in state [:car/id 1]) => nil)))


  (behavior "Returns not-found when provided this option"
    (let [state {:person/id {1 {:person/name  "person-1"
                                :person/email [:email/id 1]}
                             2 {:person/name  "person-2"
                                :person/email [:email/id 2]}}}]
      (assertions
        (nsh/get-in state [:person/id 3 :person/name] "not-found") => "not-found"))))

(defsc Person [this {:keys [:person/id :person/name :person/children] :as props}]
  {:query [:person/id :person/name {:person/children '...}]
   :ident :person/id})

(specification "ui->props"
  (behavior "Pulls the props from the component given the app state"
    (let [state {:person/id {1 {:person/id 1 :person/name "Dad" :person/children [[:person/id 2] [:person/id 3]]}
                             2 {:person/id 2 :person/name "Son"}
                             3 {:person/id 3 :person/name "Daughter"}}}]
      (nsh/ui->props state Person [:person/id 1]) => {:person/id       1 :person/name "Dad"
                                                      :person/children [{:person/id 2 :person/name "Son"}
                                                                        {:person/id 3 :person/name "Daughter"}]})))

