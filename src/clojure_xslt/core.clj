(ns clojure-xslt.core
  (:import [javax.xml.transform TransformerFactory Transformer Templates]
           [javax.xml.transform.stream StreamSource StreamResult])
  (:use [clojure.java.io :only (input-stream output-stream)]))

(derive java.io.InputStream ::stream)
(derive java.io.OutputStream ::stream)
(derive java.io.Reader ::stream)
(derive java.io.Writer ::stream)

(defn transformer-factory
  [& opts]
  (let [{class-name :class-name
         class-loader :class-loader
         features :features
         attributes :attributes
         uri-resolver :uri-resolver
         error-listener :error-listener}
        (when opts (apply hash-map opts))
        factory (if class-name
                  (TransformerFactory/newInstance class-name class-loader)
                  (TransformerFactory/newInstance))]
    (when uri-resolver
      (.setURIResolver factory uri-resolver))
    (when error-listener
      (.setErrorListener factory uri-resolver))
    (doseq [[name val] features]
      (.setFeature factory name val))
    (doseq [[name val] attributes]
      (.setAttribute factory name val))
    factory))

(defn transformer
  ([xslt-input]
     (transformer (transformer-factory) xslt-input))
  ([factory xslt-input]
     (let [source (StreamSource. xslt-input)]
       (.newTransformer factory source))))

(defn templates
  ([xslt-input]
     (templates (transformer-factory) xslt-input))
  ([factory xslt-input]
     (let [source (StreamSource. xslt-input)]
       (.newTemplates factory source))))

(defn transform
  [xslt in out & xsl-params]
  (let [source (StreamSource. in)
        result (StreamResult. out)]
    (doseq [[name val] xsl-params]
      (.setParameter xslt name val))
    (.transform xslt source result)))
