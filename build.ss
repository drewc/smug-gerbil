#!/usr/bin/env gxi
;; -*- Gerbil -*-

;;; The below would work with drewc's new make, but it isn't upstreamed yet
;;(import :std/build-script) (defbuild-script '("primitive" "simple" "tokens" "smug"))

(import :std/make)
(def srcdir (path-normalize (path-directory (this-source-file))))

(make '("primitive" "simple" "tokens") srcdir: srcdir build-deps: "build-deps")
(make '("smug") prefix: "drewc" srcdir: srcdir build-deps: "build-deps-smug")
