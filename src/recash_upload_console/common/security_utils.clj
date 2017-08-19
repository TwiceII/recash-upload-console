(ns recash-upload-console.common.security-utils)

(defn md5 [s]
  (let [algorithm (java.security.MessageDigest/getInstance "MD5")
        raw (.digest algorithm (.getBytes s))]
    (format "%032x" (java.math.BigInteger. 1 raw))))
