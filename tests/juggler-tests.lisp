(defpackage #:juggler-tests
  (:use :cl :eos))

(in-package :juggler-tests)

(def-suite :juggler
    :description "Main test suite, all suites inherit from this.")

(in-suite* :juggler)

(test (3d-vector-equality :suite :juggler)
  "Ensure that two printed vectors are `equalp' to each other.

This means #V(1 1 1) is equal with respect to the slots to #V(1 1 1)."
  (is (equalp #V(1 1 1) #V(1 1 1)))
  (is (not (equalp #V(2 3 4) #V(2 3 3))))
  (is (equalp #V(1.1 1.1 1.1) #V(1.1 1.1 1.1))))

(test (magnitude :suite :juggler)
  (is (= 1 (juggler::magnitude #V(1 0 0))))
  (is (= 1 (juggler::magnitude #V(-1 0 0))))
  (is (= 0 (juggler::magnitude #V(0 0 0)))))

(test (scale :suite :juggler)
  "Scaling should give V*n."
  (is (equalp #V(0 0 0) (juggler::scale #V(0 0 0) 100)))
  (is (equalp #V(10 10 10) (juggler::scale #V(1 1 1) 10)))
  (is (equalp #V(-10 -10 -10) (juggler::scale #V(10 10 10) -1))))

(test (negate :suite :juggler
              :depends-on scale)
  (is (equalp #V(-1 -1 -1) (juggler::negate #V(1 1 1))))
  (is (equalp #V(0 0 0) (juggler::negate #V(0 0 0))))
  (is (equalp #V(1 1 1) (juggler::negate #V(-1 -1 -1)))))

(test (divide-vector :suite :juggler
                     :depends-on scale)
  (is (equalp #V(1/2 1/2 1/2) (juggler::divide-vector #V(1 1 1) 2))))

(test (unit-vector :suite :juggler
                   :depends-on (and . (scale magnitude)))
  (is (equalp #V(1 0 0) (juggler::unit-vector #V(10 0 0))))
  (signals division-by-zero (juggler::unit-vector #V(0 0 0))))

(test (add-vector :suite :juggler)
  (is (equalp #V(0 0 0)
              (juggler::add-vector #V(0 0 0) #V(0 0 0))))
  (is (equalp #V(1 2 3)
              (juggler::add-vector #V(-1 -2 -3) #V(2 4 6))))
  (is (equalp #V(2/3 2/3 -2/3)
              (juggler::add-vector #V(1/3 1/3 -1/3) #V(1/3 1/3 -1/3)))))

(test (subtract-vector :suite :juggler
                       :depends-on (and . (add-vector negate)))
  (is (equalp #V(-3 -3 -3) (juggler::subtract-vector #V(2 2 2) #V(5 5 5)))))

(test (ray :suite :juggler
           :depends-on (and . (add-vector scale)))
  (is (equalp #V(1 1 1) (juggler::ray #V(0 0 0) #V(1 1 1) 1)))
  (is (equalp #V(1 1 1) (juggler::ray #V(0 0 0) #V(1/2 1/2 1/2) 2)))
  (is (equalp #V(1 1 1) (juggler::ray #V(1 1 1) #V(0 0 0) 0)))
  (is (equalp #V(1 1 1) (juggler::ray #V(1 1 1) #V(10 10 10) 0)))
  (is (equalp #V(2 2 2) (juggler::ray #V(1 1 1) #V(10 10 10) 1/10))))


(test (cross-product :suite :juggler)
  (is (equalp #V(0 0 1) (juggler::cross-product #V(1 0 0) #V(0 1 0)))))

(test (dot-product :suite :juggler)
  (is (= 6 (juggler::dot-product #V(1 1 1) #V(2 2 2)))))
