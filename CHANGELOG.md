# Revision history for tensort

## 0.1.0.0 -- 2024-05-30

* First version. Released to an eager world!

## 0.2.0.0 -- 2024-05-31

* Add Logarithmic Tensort

* Rename and update Exchangesort

* Simplify code and structure

* Cleanup exports

* Cleanup Types

* Improve documentation

* Add to package file

* Expand supported dependency versions

* Add tests

## 0.2.0.1 -- 2024-06-12

* Add guards for short lists in input

* Improve testing

* Improve documentation

* Add very basic benchmarking

## 0.2.0.2 -- 2024-06-13

* Cleanup testing and CI

## 0.2.0.3 -- 2024-06-16

* Improve testing compatibility (fix QuickCheck breaking Stackage build)

## 1.0.0.0 -- 2024-08-21

* Add Recursive Robustsort

* Add Rotationsort

* Fix Bubblesort to more closely match Ackley's non-'optimized' version

* Add Benchmarking

* Expand README

* Replace Exchangesort with Rotationsort in Robustsort

* Use Sortable type in Tensort and Robustsort so they can be used recursively

* Add top-level Tensort and Robustsort functions wrapped in a type converter so
  they can be easily used to sort Bits (Integers)

* Add more helper functions

* Many more updates to the algorithms - see README for details
