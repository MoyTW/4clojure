;; This seems pretty easy?
(fn __ [k m]
  (= nil (get m k :not-present)))