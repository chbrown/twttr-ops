(ns twttr.io
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.data.json :as json]
            [byte-streams :as bs]
            [byte-transforms :as bt]))

(defn read-json-str
  [string]
  (json/read-str string :key-fn keyword :eof-error? false))

(defn write-json-str
  [x]
  (json/write-str x :escape-unicode false :escape-js-separators false :escape-slash false))

(defn- gzipped?
  [f]
  (cond (string? f) (str/ends-with? f ".gz")
        (instance? java.io.File f) (gzipped? (.getName ^java.io.File f))))

(defprotocol JsonIO
  (read-json-lines [input]
    "Read line-delimited JSON immediately, parse keys as keywords,
    returning a vector, or nil for empty input (even non-existent files)")
  (write-json-lines [output objects]
    "Write line-delimited JSON data structures immediately, returning nil"))

(extend-protocol JsonIO
  java.io.File
  (read-json-lines [input]
    (when (.exists input)
      (with-open [^java.io.InputStream input-stream (cond-> (io/input-stream input)
                                                      (gzipped? input) (bt/decompress :gzip))
                  ; convert the java.io.InputStream to a java.io.BufferedReader
                  rdr (io/reader input-stream)]
        (mapv read-json-str (line-seq rdr)))))
  (write-json-lines [output objects]
    (let [strings (map write-json-str objects)
          ; to-input-stream can't handle Character instances, so we repeat a string
          lines (mapcat vector strings (repeat "\n"))]
      (with-open [^java.io.InputStream input-stream (cond-> (bs/to-input-stream lines)
                                                      (gzipped? output) (bt/compress :gzip))]
        (bs/transfer input-stream output)))))
