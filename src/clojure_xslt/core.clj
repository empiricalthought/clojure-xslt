(ns clojure-xslt.core
  "Core code for wrapping JAXP functionality in a Clojure API."
  (:import [javax.xml.transform TransformerFactory Transformer Templates]
           [javax.xml.transform.stream StreamSource StreamResult]
           [javax.xml.xpath XPathFactory XPathConstants]
           [javax.xml.parsers DocumentBuilderFactory]
           [javax.xml.namespace NamespaceContext])
  (:use [clojure.java.io :only (input-stream output-stream)]))


(defn document-builder-factory
  "Create a DocumentBuilderFactory instance.

`opts` are keyword/value pairs, all of which are optional.
Valid keywords are:

   :features       a map, name/values to set as XML parser features
   :attributes     a map, name/values to set as XML parser attributes
   :class-name     a string, name of the DocumentBuilderFactory class to use
   :class-loader   a ClassLoader, the classloader to use to load the factory"
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
  "Create an org.w3c.dom.Document from a given input.

`input` can be a File, InputStream, String (representing a URI), or
InputSource.  When present, the DocumentBuilderFactory `dbf` will be
used to create the parser.  If `system-id` is also present, the parser
will use its value as the base for relative URI resolution."
  ([input]
     (document input (document-builder-factory)))
  ([input dbf]
     (.parse (.newDocumentBuilder dbf) input))
  ([input dbf system-id]
     (.parse (.newDocumentBuilder dbf) input system-id)))


(def xpath-constant-map
  {:string XPathConstants/STRING
   :boolean XPathConstants/BOOLEAN
   :dom-object-model XPathConstants/DOM_OBJECT_MODEL
   :node XPathConstants/NODE
   :nodeset XPathConstants/NODESET
   :number XPathConstants/NUMBER})


(defn namespace-context
  "Generate a NamespaceContext object given a map of prefixes to URIs."
  [ns-context-map]
  (proxy [NamespaceContext] []
    (getNamespaceURI [prefix] (ns-context-map prefix))))


(defn xpath-factory
  "Create an javax.xml.xpath.XPathFactory instance.

`opts` are keyword/value pairs, all of which are optional.  Valid
keywords are:

   :features           a map, name/values to set as XPathFactory features
   :function-resolver  an XPathFunctionResolver
   :variable-resolver  an XPathVariableResolver
   :class-name         a String, name of the XPathFactory class to use
   :class-loader       a ClassLoader, the classloader to use to load
                       the factory
   :object-model-uri   a String that is the URI for the XPathFactory's
                       object model"
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
  "Create an XPath evaluation function from a given XPath expression.

The return value is a Clojure function.  This function takes a single
argument: the context in which to evaluate the XPath expression.

`opts` are keyword/value pairs, all of which are optional.  Valid
keywords are:

   :factory      an XPathFactory
   :ns           a map from XML namespace prefixes to the corresponding URIs
   :return-type  a keyword. The return type of the XPath evaluation.
                 One of :string, :boolean, :node, :nodeset, :number,
                 or :dom-object-model
   :preserve-whitespace  boolean; when true, string results will be returned
                         with leading and trailing whitespace if present."
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
  "Create a TransformerFactory instance.

`opts` are keyword/value pairs, all of which are optional.  Valid
keywords are:

   :features        a map, name/value pairs of TransformerFactory features
   :attributes      a map, name/value pairs of TransformerFactory attributes
   :uri-resolver    a URIResolver
   :error-listener  an ErrorListener
   :class-name      a String, name of the TransformerFactory class to use
   :class-loader    a ClassLoader, the class loader to use to load the factory"
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
  "Create a Transformer from a given Reader or InputStream.

When factory is present, it will be used as the TransformerFactory
instance that creates the Transformer."
  ([xslt-input]
     (transformer (transformer-factory) xslt-input))
  ([factory xslt-input]
     (let [source (StreamSource. xslt-input)]
       (.newTransformer factory source))))

(defn templates
  "Create a Templates from a given Reader or InputStream.

When factory is present, it will be used as the TransformerFactory
instance that creates the Templates."
  ([xslt-input]
     (templates (transformer-factory) xslt-input))
  ([factory xslt-input]
     (let [source (StreamSource. xslt-input)]
       (.newTemplates factory source))))

(defn transform
  "Apply the transformation from xslt to in, outputting the result to out.

`xslt` is an instance of javax.xml.transform.Transformer.  `in` is a
Reader or an InputStream.  `out` is a Writer or an OutputStream."
  [xslt in out & xsl-params]
  (let [source (StreamSource. in)
        result (StreamResult. out)]
    (doseq [[name val] xsl-params]
      (.setParameter xslt name val))
    (.transform xslt source result)))
