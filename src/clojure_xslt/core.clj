(ns clojure-xslt.core
  (:import [javax.xml.transform TransformerFactory Transformer Templates]
           [javax.xml.transform.stream StreamSource StreamResult]
           [javax.xml.xpath XPathFactory XPathConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.namespace NamespaceContext])
  (:use [clojure.java.io :only (input-stream output-stream)]))

(derive java.io.InputStream ::stream)
(derive java.io.OutputStream ::stream)
(derive java.io.Reader ::stream)
(derive java.io.Writer ::stream)


(defn document-builder-factory
  [& opts]
  (let [{features :features
         attributes :attributes
         class-name :class-name
         class-loader :class-loader}
        (when opts (apply hash-map opts))
        factory (if class-name
                  (DocumentBuilderFactory/newInstance class-name class-loader)
                  (DocumentBuilderFactory/newInstance))]
    (doseq [[name val] features]
      (.setFeature factory name val))
    (doseq [[name val] attributes]
      (.setAttribute factory name val))
    (.setNamespaceAware factory true)
    factory))

(defn document
  ([input]
     (document input (document-builder-factory)))
  ([input document-builder-factory]
     (.parse (.newDocumentBuilder document-builder-factory) input))
  ([input document-builder-factory system-id]
     (.parse (.newDocumentBuilder document-builder-factory) input system-id)))

(def xpath-constant-map
  {:string XPathConstants/STRING
   :boolean XPathConstants/BOOLEAN
   :dom-object-model XPathConstants/DOM_OBJECT_MODEL
   :node XPathConstants/NODE
   :nodeset XPathConstants/NODESET
   :number XPathConstants/NUMBER})

(defn namespace-context
  [ns-context-map]
  (proxy [NamespaceContext] []
    (getNamespaceURI [prefix] (ns-context-map prefix))))

(defn xpath-factory
  [& opts]
  (let [{features :features
         function-resolver :function-resolver
         variable-resolver :variable-resolver
         class-name :class-name
         class-loader :class-loader
         object-model-uri :object-model-uri}
        (when opts (apply hash-map opts))
        uri (or object-model-uri XPathFactory/DEFAULT_OBJECT_MODEL_URI)
        factory (if class-name
                  (XPathFactory/newInstance uri class-name class-loader)
                  (XPathFactory/newInstance uri))]
    (doseq [[name val] features]
      (.setFeature factory name val))
    (when function-resolver
      (.setXPathFunctionResolver factory function-resolver))
    (when variable-resolver
      (.setXPathVariableResolver factory variable-resolver))
    factory))

(defn xpath
  ([expression]
     (xpath expression nil))
  ([expression & opts]
     (let [{factory :factory
            ns :ns
            return-type :return-type
            preserve-whitespace :preserve-whitespace}
           (when opts (apply hash-map opts))
           xpath-return-type (get xpath-constant-map return-type XPathConstants/STRING)
           ns-context (if (map? ns)
                        (namespace-context ns)
                        ns)
           fact (if factory factory (xpath-factory))]
       (fn [obj]
         (let [xp (.newXPath fact)]
           (.setNamespaceContext xp ns-context)
           (let [result (.evaluate xp expression obj xpath-return-type)]
             (if (and (string? result) (not preserve-whitespace))
               (.trim result)
               result)))))))


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
