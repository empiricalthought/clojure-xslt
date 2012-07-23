(ns clojure-xslt.core-test
  (:import [java.io ByteArrayInputStream ByteArrayOutputStream])
  (:use clojure.test
        clojure-xslt.core
        clojure.java.io
        clojure.xml))

(deftest identity-transform
  (testing "Run the XSLT identity transform against itself."
    (let [xslt-identity-bytes (ByteArrayOutputStream.)]
      (with-open [xslt-identity-stream (input-stream "resources/identity.xsl")]
        (copy xslt-identity-stream xslt-identity-bytes))
      (let [data (.toByteArray xslt-identity-bytes)
            xslt (transformer (ByteArrayInputStream. data))
            document-input (ByteArrayInputStream. data)
            document-output (ByteArrayOutputStream.)]
        (transform xslt document-input document-output)
        (let [expected (clojure.xml/parse (ByteArrayInputStream. data))
              actual (clojure.xml/parse (ByteArrayInputStream.
                                         (.toByteArray document-output)))]
          (is (= actual expected)))))))

(deftest change-title-transform
  (testing "Run sample XSLT against sample XML."
    (let [result
          (with-out-str
            (with-open [xslt (input-stream "resources/change-title.xsl")
                        xml (input-stream "resources/dublin-core.xml")]
              (transform (transformer xslt) xml *out*)))]
      (is (.contains result "New Title")))))

(deftest change-title-templates
  (testing "Run sample XSLT against sample XML using javax.xml.transform.Templates"
    (let [result
          (with-out-str
            (with-open [xslt (input-stream "resources/change-title.xsl")
                        xml (input-stream "resources/dublin-core.xml")]
              (transform (.newTransformer (templates xslt)) xml *out*)))]
      (is (.contains result "New Title")))))
