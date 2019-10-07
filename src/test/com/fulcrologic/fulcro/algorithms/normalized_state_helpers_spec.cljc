(ns com.fulcrologic.fulcro.algorithms.normalized-state-helpers-spec
  (:require
    [fulcro-spec.core :refer [assertions specification component when-mocking behavior =>]]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.algorithms.normalized-state-helpers :as nsh]))


;;============================================================================


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

;;============================================================================

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


;;============================================================================

(specification "remove-entity*"
  (behavior "Without cascading"
    (let [denorm-data {:a [[:person/id 1] [:person/id 2]]
                       :b [:person/id 1]}

          state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 denorm-data}}
                 :person/id    {1 {:person/name  "person-1"
                                   :person/cars  [[:car/id 1] [:car/id 2]]
                                   :person/email [:email/id 1]}
                                2 {:person/name  "person-2"
                                   :person/cars  [[:car/id 1]]
                                   :person/email [:email/id 2]}}
                 :car/id       {1 {:car/model "model-1"}
                                2 {:car/model "model-2"}}
                 :email/id     {1 {:email/provider "Google"}
                                2 {:email/provider "Microsoft"}}}]
      (assertions
        "Removes the entity itself from the database"
        (-> (nsh/remove-entity* state [:person/id 1] #{})
            (get-in [:person/id 1])
            ) => nil
        "Removes top-level to-one references"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) :fastest-car) => []
        "Removes top-level to-many refs"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) :grandparents) => [[:person/id 2]]
        "Ignores denormalized data"
        (-> (nsh/remove-entity* state [:person/id 1] #{}) (get-in [:denorm :level-1 :level-2])) => denorm-data
        "Removes table-nested to-one references"
        (-> (nsh/remove-entity* state [:email/id 1] #{}) (get-in [:person/id 1 :person/email]) nil?) => true
        "Removes table-nested to-many refs"
        (-> (nsh/remove-entity* state [:car/id 1] #{}) (get-in [:person/id 1 :person/cars])) => [[:car/id 2]]
        )))


  (behavior "With cascading, non-recursive behavior"
    (let [state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                                    :b [:person/id 1]}}}
                 :person/id    {1 {:person/id       1
                                   :person/spouse   [:person/id 2]
                                   :person/email    [:email/id 1]
                                   :person/cars     [[:car/id 1]
                                                     [:car/id 2]]
                                   :person/children [[:person/id 3]]}
                                2 {:person/id       2
                                   :person/spouse   [:person/id 1]
                                   :person/children [[:person/id 3]]}
                                3 {:person/id 3}}
                 :car/id       {1 {:car/id    1
                                   :car/model "model-1"}
                                2 {:car/id    2
                                   :car/model "model-2"}}
                 :engine/id    {1 {:engine/id   1
                                   :engine/name "engine-1"}}
                 :email/id     {1 {:email/id       1
                                   :email/provider "Google"}}}]
      (assertions

        "Removes a single, to-one and non-recursive cascased entity"
        (-> (nsh/remove-entity* state [:person/id 1] #{:person/email})
            (get [:email/id 1])
            nil?) => true

        "Removes a single, to-many and non-recursive cascased entity"
        (let [new-state (nsh/remove-entity* state [:person/id 1] #{:person/cars})]
          (and
            (get new-state [:car/id 1])
            (get new-state [:car/id 2]))) => nil

        "Removes multiple, to-one and non-recursive cascased entity"
        (let [new-state (nsh/remove-entity* state [:person/id 1]
                                            #{:person/email :person/cars})]
          (and
            (get new-state [:email/id 1])
            (get new-state [:car/id 1])
            (get new-state [:car/id 2]))) => nil)))

  (behavior "With cascading, recursive behavior"
    (let [state {:fastest-car  [:car/id 1]
                 :grandparents [[:person/id 1] [:person/id 2]]
                 :denorm       {:level-1 {:level-2 {:a [[:person/id 1] [:person/id 2]]
                                                    :b [:person/id 1]}}}
                 :person/id    {1 {:person/id       1
                                   :person/spouse   [:person/id 2]
                                   :person/email    [:email/id 1]
                                   :person/cars     [[:car/id 1]
                                                     [:car/id 2]]
                                   :person/children [[:person/id 3]]}
                                2 {:person/id       2
                                   :person/spouse   [:person/id 1]
                                   :person/children [[:person/id 3]]}
                                3 {:person/id 3}}
                 :car/id       {1 {:car/id    1
                                   :car/model "model-1"}
                                2 {:car/id    2
                                   :car/model "model-2"}}
                 :engine/id    {1 {:engine/id   1
                                   :engine/name "engine-1"}}
                 :email/id     {1 {:email/id       1
                                   :email/provider "Google"}}}]
      (assertions

        "Removes a single, to-many cascased entities"
        (let [new-state (nsh/remove-entity* state [:person/id 1] #{:person/children})]
          (get new-state [:person/id 3])) => nil

        "Removes multiple, to-many cascased entities"
        (let [new-state (nsh/remove-entity* state [:person/id 1]
                                            #{:person/children :person/spouse})]
          (and
            (get new-state [:person/id 2])
            (get new-state [:person/id 3]))) => nil))))


;============================================================================
(specification "remove-edge*"
  (component "Simple to-one edge removal"
    (let [state {:fastest-car [:car/id 1]
                 :car/id      {1 {:car/engine [:engine/id 1]}}
                 :engine/id   {1 {:engine/id    1
                                  :engine/model "engine-1"}}}]
      (assertions
        "Removes top-level to-one edge"
        (-> (nsh/remove-edge* state [:fastest-car])
            (get-in [:fastest-car])) => {}

        "Removes table-nested edge"
        (-> (nsh/remove-edge* state [:car/id 1 :car/engine])
            (get-in [:car/id 1 :car/engine])) => {}

        "Refuses to remove something that is not a normalized edge"
        (nsh/remove-edge* state [:car/id 1 :car/model]) => state
        (nsh/remove-edge* state [:car/id 1]) => state
        (nsh/remove-edge* state [:car/id]) => state)))


  ;; TODO
  #_(component "Recursive to-one edge removal"
               (let [state {:a [:b 1]
                            :b {1 {:c [:d 1]}}
                            :d {1 {:value 42 :other [:e 2] :keeper [:e 3]}}
                            :e {2 {:x 1}
                                3 {:y 1}}}]

                 (assertions
                   "Can cascade a delete across named edge, removing the ident on the :other edge from it's own table"
                   (-> (nsh/remove-edge* state [:b 1 :c] #{:other})
                       (get-in [:e 2])
                       nil?) => true)

                 (assertions
                   "Can cascade a delete across named edges, leaving the ident on the :keeper edge it's in own table"
                   (-> (nsh/remove-edge* state [:b 1 :c] #{:other})
                       (get-in [:e 3])) => {:y 1})
                 ;; TODO: other cases, like to-many w/o cascading,  to-many with cascading, etc.
                 )))

;============================================================================

;; TODO
(specification "sort-idents-by"
  (behavior "1"
    (let [state (atom {:grandparents [[:person/id 3] [:person/id 2]]
                       :person/id    {1 {:person/name     "person-1"
                                         :person/children [[:person/id 3]
                                                           [:person/id 9]
                                                           [:person/id 5]]}
                                      2 {:person/name "person-2"
                                         :person/cars [[:car/id 1]
                                                       [:car/id 2]]}}
                       :car/id       {1 {:car/model "model-1"}
                                      2 {:car/model "model-2"}}})]
      (nsh/sort-idents-by
        :person/id
        (clojure.core/get-in @state [:person/id 1 :person/children])) => [])))


;============================================================================

(defsc Person [this {:keys [:person/id :person/name :person/children] :as props}]
  {:query [:person/id :person/name {:person/children '...}]
   :ident :person/id})

(specification "ui->props"
  (behavior "Pulls the props from the component given the app state"
    (let [state {:person/id {1 {:person/id       1 :person/name "Dad"
                                :person/children [[:person/id 2] [:person/id 3]]}
                             2 {:person/id 2 :person/name "Son"}
                             3 {:person/id 3 :person/name "Daughter"}}}]
      (nsh/ui->props state Person [:person/id 1]) => {:person/id       1 :person/name "Dad"
                                                      :person/children [{:person/id 2 :person/name "Son"}
                                                                        {:person/id 3 :person/name "Daughter"}]})))

;============================================================================

(specification "update-caller!"
  (behavior "1"
    (let [state-atom (atom {:person/id {1 {:person/id 1 :person/name "Dad"}}})
          mutation-env {:ref [:person/id 1] :state state-atom}]
      (nsh/update-caller! mutation-env assoc :person/name "Mom")

      (assertions
        "Updates the caller from the env of the mutation"
        (nsh/get-in @state-atom [:person/id 1]) => {:person/id 1 :person/name "Mom"}))))

;============================================================================
;; TODO
(specification "update-caller-in!"
  (behavior "1"
    (let [state-atom (atom {:person/id {1 {:person/id       1 :person/name "Dad"
                                           :person/children [[:person/id 2] [:person/id 3]]}
                                        2 {:person/id 2 :person/name "Son"}
                                        3 {:person/id 3 :person/name "Daughter"}}})
          mutation-env {:ref [:person/id 1] :state state-atom}]
      (nsh/update-caller! mutation-env assoc :person/name "Mom")

      (assertions
        "Updates the caller from the env of the mutation"
        (nsh/get-in @state-atom [:person/id 1]) => {:person/id 1 :person/name "Mom"}))))


;============================================================================

(specification "swap!->"
  (behavior "1"
    (let [state {:a {:c {:value 42}}}]
      (assertions
        (nsh/swap!-> state [:a :c :value]) => 42)))
  )



(comment
  (def env {:state (atom {})})
  (nsh/swap!-> env
               (assoc :x 1)
               (update :x inc))

  env

  '())

