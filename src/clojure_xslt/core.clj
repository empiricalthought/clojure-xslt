(ns clojure-xslt.core
  (:import [javax.xml.transform TransformerFactory Transformer Templates]
           [javax.xml.transform.stream StreamSource StreamResult])
  (:use [clojure.java.io :only (input-stream output-stream)]))

(def ^:dynamic *xsl-params* {})
(def ^:dynamic *xsl-output-properties* {})
(def ^:dynamic *uri-resolver* nil)
(def ^:dynamic *transformer-factory-features* {})
(def ^:dynamic *transformer-factory-attributes* {})
(def ^:dynamic *error-listener* nil)

(derive java.io.InputStream ::stream)
(derive java.io.OutputStream ::stream)
(derive java.io.Reader ::stream)
(derive java.io.Writer ::stream)


(defn make-transformer-factory
  ([]
     (make-transformer-factory (TransformerFactory/newInstance)))
  ([class-name class-loader]
     (make-transformer-factory
      (TransformerFactory/newInstance class-name class-loader)))
  ([factory]
     (let [set-features (fn [tf]
                          (doseq [[k v] *transformer-factory-features*]
                            (.setFeature tf k v)))
           set-attributes (fn [tf]
                            (doseq [[k v] *transformer-factory-attributes*]
                              (.setAttribute tf k v)))]
       (when *uri-resolver*
         (.setURIResolver factory *uri-resolver*))
       (when *error-listener*
         (.setErrorListener factory *error-listener*))
       (set-features factory)
       (set-attributes factory))
     factory))


(defmulti make-templates class)

(defmethod make-templates ::stream
  [xslt-input]
  (let [factory (TransformerFactory/newInstance)]
    (let [source (StreamSource. xslt-input)]
      (.newTemplates factory source))))

(defmethod make-templates Templates
  [xslt-input]
  xslt-input)

(defmethod make-templates :default
  [xslt-input]
  (let [factory (TransformerFactory/newInstance)]
    (with-open [input (input-stream xslt-input)]
      (let [source (StreamSource. input)]
        (.newTemplates factory source)))))


(defmulti make-transformer class)

(defmethod make-transformer ::stream
  [xslt-input]
  (let [factory (TransformerFactory/newInstance)]
    (let [source (StreamSource. xslt-input)]
      (.newTransformer factory source))))

(defmethod make-transformer Templates
  [xslt-input]
  (.newTransformer xslt-input))

(defmethod make-transformer Transformer
  [xslt-input]
  xslt-input)

(defmethod make-transformer :default
  [xslt-input]
  (let [factory (TransformerFactory/newInstance)]
    (with-open [input (input-stream xslt-input)]
      (let [source (StreamSource. input)]
        (.newTransformer factory source)))))


(defn transform
  ([xslt]
     (transform xslt *in* *out*))
  ([xslt in]
     (transform xslt in *out*))
  ([xslt in out]
     ;; Convert arguments to streams if necessary.
     (let [set-xsl-params (fn [transformer]
                            (doseq [[name value] *xsl-params*]
                              (.setParameter name value)))
           input-is-stream? (isa? (class in) ::stream)
           output-is-stream? (isa? (class out) ::stream)
           input (if input-is-stream? in (input-stream in))
           output (if output-is-stream? out (output-stream out))]
       (try
         (let [transformer (make-transformer xslt)
               input-source (StreamSource. input)
               output-result (StreamResult. output)]
           (set-xsl-params transformer)
           (.transform transformer input-source output-result))
         (finally
          ;; Close streams that this function created.
          (when (not input-is-stream?)
            (.close input))
          (when (not output-is-stream?)
            (.close output)))
         ))))


(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

