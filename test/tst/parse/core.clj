(ns tst.parse.core
  (:use parse.core
        parse.transform
        tupelo.test
        clojure.test)
  (:require
    [clojure.string :as str]
    [instaparse.core :as insta]
    [clojure.test.check :as tc]
    [clojure.test.check.generators :as gen]
    [clojure.test.check.properties :as prop]
    [clojure.test.check.clojure-test :as tst]
    [schema.core :as s]
    [tupelo.core :as t]
    [tupelo.gen :as tgen]
    [tupelo.misc :as tm]
    [tupelo.string :as ts]
    [tupelo.schema :as tsk]
    [clojure.set :as set]
    ))
(t/refer-tupelo)


(dotest
  (check 99
    (prop/for-all [s (gen/not-empty gen/string-alphanumeric)]
      ;(< (count s) 3)
      (ts/alphanumeric? s))))

(def word gen/string-alphanumeric)
(def word-quoted-double (gen/fmap #(str \" % \") word))
(def word-quoted-single (gen/fmap #(str \' % \') word))
(def spaced-words (gen/fmap #(str/join \space %)
                    (gen/vector (gen/one-of [word word-quoted-double word-quoted-single]))))
(def eol-then-words (gen/fmap strcat (gen/tuple tgen/chars-eol+ tgen/words-alpha)))
(def words-then-eol (gen/fmap strcat (gen/tuple tgen/words-alpha+ tgen/chars-eol+)))
(def starts-with-cpp-comment
  (gen/let [words tgen/words-alphanumeric]
    (strcat "// " words)))
(def not-starts-with-cpp-comment
  (gen/let [words tgen/words-alphanumeric+]
    (strcat words " //")))

;(spyx (gen/sample spaced-words))
;(spyx (gen/sample gen/string-alphanumeric))
;(spyx (gen/sample tgen/word-alpha))
;(spyx (gen/sample tgen/word-alpha+))
;(spyx (gen/sample tgen/words-alpha))
;(spyx (gen/sample tgen/words-alpha+))
;(spyx (gen/sample eol-then-words))
;(spyx (gen/sample words-then-eol))
;(spyx (gen/sample not-starts-with-cpp-comment))

(dotest
  (check 99 (prop/for-all [str-val (tgen/maybe-vec eol-then-words)]
              (starts-with-eol str-val)))
  (check 99 (prop/for-all [str-val (tgen/maybe-vec words-then-eol)]
              (not (starts-with-eol str-val))))

  (let [cr-abc   (str \return \a \b \c)
        lf-abc   (str \newline \a \b \c)
        crlf-abc (str \return \newline \a \b \c)]
    (is= 4 (count cr-abc))
    (is= "abc" (str/trim cr-abc))
    (is (starts-with-eol cr-abc))
    (is (starts-with-eol (vec cr-abc)))

    (is= 4 (count lf-abc))
    (is= "abc" (str/trim lf-abc))
    (is (starts-with-eol lf-abc))
    (is (starts-with-eol (vec lf-abc)))

    (is= 5 (count crlf-abc))
    (is= "abc" (str/trim crlf-abc))
    (is (starts-with-eol crlf-abc))
    (is (starts-with-eol (vec crlf-abc)))
    )
  (isnt (starts-with-eol ""))
  (isnt (starts-with-eol "abc"))
  (isnt (starts-with-eol "abc \newline"))
  (isnt (starts-with-eol "abc \return"))

  (isnt (starts-with-eol (vec "")))
  (isnt (starts-with-eol (vec "abc")))
  (isnt (starts-with-eol (vec "abc \newline")))
  (isnt (starts-with-eol (vec "abc \return")))
  )

(dotest
  (check 99 (prop/for-all [str-val (tgen/maybe-vec starts-with-cpp-comment)]
              (found-comment-cpp-start str-val)))
  (check 99 (prop/for-all [str-val (tgen/maybe-vec not-starts-with-cpp-comment)]
              (not (found-comment-cpp-start str-val))))

  (is (found-comment-cpp-start "//"))
  (is (found-comment-cpp-start "// "))
  (is (found-comment-cpp-start "// Hello Comment!"))
  (isnt (found-comment-cpp-start ""))
  (isnt (found-comment-cpp-start "/"))
  (isnt (found-comment-cpp-start " //"))
  (isnt (found-comment-cpp-start "x//"))
  (isnt (found-comment-cpp-start "/*"))
  (isnt (found-comment-cpp-start "/* C comment */"))
  (check 99 (prop/for-all [str-val (tgen/maybe-vec eol-then-words)]
              (starts-with-eol str-val)))
  )

(deftest t-found-comment-c-start
  (is (found-comment-c-start "/*"))
  (is (found-comment-c-start "/* "))
  (is (found-comment-c-start "/* Hello Comment!"))
  (isnt (found-comment-c-start ""))
  (isnt (found-comment-c-start "/"))
  (isnt (found-comment-c-start " /*"))
  (isnt (found-comment-c-start "x/*"))
  (isnt (found-comment-c-start "//"))
  (isnt (found-comment-c-start "// C++ comment ")))

(deftest t-found-comment-c-stop
  (is (found-comment-c-stop "*/"))
  (is (found-comment-c-stop "*/ "))
  (is (found-comment-c-stop "*/ /* Hello Comment!"))
  (isnt (found-comment-c-stop ""))
  (isnt (found-comment-c-stop "/"))
  (isnt (found-comment-c-stop " */"))
  (isnt (found-comment-c-stop "x*/"))
  (isnt (found-comment-c-stop "\n"))
  (isnt (found-comment-c-stop "\n\n")))

(deftest t-consume-comment-cpp
  (is= [\return \a \b \c] (consume-comment-cpp (str "//" \return "abc")))
  (is= [\return \newline \a \b \c] (consume-comment-cpp (str "//" \return \newline "abc")))
  (is= [\newline \a \b \c] (consume-comment-cpp (str "//" \newline "abc")))
  (is= [\newline \return \a \b \c] (consume-comment-cpp (str "//" \newline \return "abc")))


  (is= [\return \a \b \c] (consume-comment-cpp (str "// some comment" \return "abc")))
  (is= [\newline \a \b \c] (consume-comment-cpp (str "// some comment" \newline "abc")))

  (is= [] (consume-comment-cpp "//"))
  (is= [] (consume-comment-cpp "// "))
  (is= [] (consume-comment-cpp "// some comment ")))

(deftest t-consume-comment-c
  (is= "" (str/join (consume-comment-c "/**/")))
  (is= "" (str/join (consume-comment-c "/* */")))
  (is= "" (str/join (consume-comment-c "/* x */")))
  (is= "" (str/join (consume-comment-c "/* some comment */")))
  (is= "a" (str/join (consume-comment-c "/* some comment */a")))
  (is= "ab" (str/join (consume-comment-c "/* some comment */ab")))
  (is= "abc" (str/join (consume-comment-c "/* some comment */abc")))

  (is= [] (consume-comment-c "/*"))
  (is= [] (consume-comment-c "/*x"))
  (is= [] (consume-comment-c "/**"))
  (is= [] (consume-comment-c "/* some comment "))
  (is= [] (consume-comment-c "/* some comment *"))
  (is= [] (consume-comment-c "/* some comment /"))
  (is= [] (consume-comment-c "/* some comment /*"))
  )

(deftest t-save-dquote-str
  (is= {:result [\" \"] :src []}
    (save-dquote-string {:result [] :src [\" \"]}))

  (is= {:result [\" \a \b \c \"] :src []}
    (save-dquote-string {:result [] :src [\" \a \b \c \"]}))

  (is= {:result [\" \"] :src [\a \b \c]}
    (save-dquote-string {:result [] :src [\" \" \a \b \c]}))

  (is= {:result (vec "\"this is a string\"")
        :src    (vec "abc")}
    (save-dquote-string
      {:result []
       :src    (vec "\"this is a string\"abc")}))

  (is= {:result (vec "xyz\"\"")
        :src    []}
    (save-dquote-string
      {:result (vec "xyz")
       :src    [\" \"]}))

  (is= {:result (vec " xyz \"abc\"")
        :src    [\9]}
    (save-dquote-string
      {:result (vec " xyz ")
       :src    (vec "\"abc\"9")}))

  (is= {:result [\x \y \z \" \"]
        :src    [\space \a \b \c \space]}
    (save-dquote-string
      {:result [\x \y \z]
       :src    [\" \" \space \a \b \c \space]}))

  (is= {:result (vec " xyz \"this is a string \"")
        :src    (vec "abc ")}
    (save-dquote-string
      {:result (vec " xyz ")
       :src    "\"this is a string \"abc "}))
  )

(deftest t-save-squote-str
  (is= {:result [\' \'] :src []}
    (save-squote-string {:result [] :src [\' \']}))

  (is= {:result [\' \a \b \c \'] :src []}
    (save-squote-string {:result [] :src [\' \a \b \c \']}))

  (is= {:result [\' \'] :src [\a \b \c]}
    (save-squote-string {:result [] :src [\' \' \a \b \c]}))

  (is= {:result (vec "'this is a string'")
        :src    (vec "abc")}
    (save-squote-string
      {:result []
       :src    (vec "'this is a string'abc")}))

  (is= {:result (vec "xyz''")
        :src    []}
    (save-squote-string
      {:result (vec "xyz")
       :src    [\' \']}))

  (is= {:result (vec " xyz 'abc'")
        :src    [\9]}
    (save-squote-string
      {:result (vec " xyz ")
       :src    (vec "'abc'9")}))

  (is= {:result [\x \y \z \' \']
        :src    [\space \a \b \c \space]}
    (save-squote-string
      {:result [\x \y \z]
       :src    [\' \' \space \a \b \c \space]}))

  (is= {:result (vec " xyz 'this is a string '")
        :src    (vec "abc ")}
    (save-squote-string
      {:result (vec " xyz ")
       :src    "'this is a string 'abc "}))
  )

; #todo add generative testing
(deftest t-remove-comments
  (is= (tm/collapse-whitespace " abc ")
    (tm/collapse-whitespace
      (str/join (remove-comments " abc // xxx "))))

  (is= (tm/collapse-whitespace " abc ")
    (tm/collapse-whitespace
      (str/join (remove-comments " abc /* xxx */ "))))

  (is= (tm/collapse-whitespace
         (str/join (remove-comments
                     " line 1;  // cpp-1
                           line 2; /* c-1 */
                           /* c-2 */
                      // cpp-2
                      line 3; ")))
    (tm/collapse-whitespace "line 1; line 2; line 3;"))

  (is= (tm/collapse-whitespace
         (str/join (remove-comments
                     "line 1;  // cpp-1
                      line 2 \"has a string\"; /* c-1 */
                      /* c-2 */
                      // cpp-2
                      line 3; ")))
    (tm/collapse-whitespace "line 1;
                                 line 2 \"has a string\";
                                 line 3;"))
  (is= (tm/collapse-whitespace
         (str/join (remove-comments
                     "line 1;  // cpp-1 'str-1'
                      line 2;  // \"str-2\" cpp-2
                      line 3 \"has a // dummy-com // string\";
                      line 4 \"has a /* dummy-com */ string\";
                      line 5 'has a // dummy-com // string';
                      line 6 'has a /* dummy-com */ string';
                      /* \" dummy-com \" */
                      /*  ' dummy-com  ' */
                      line 7; ")))
    (tm/collapse-whitespace
      "line 1;
       line 2;
       line 3 \"has a // dummy-com // string\";
       line 4 \"has a /* dummy-com */ string\";
       line 5 'has a // dummy-com // string';
       line 6 'has a /* dummy-com */ string';
       line 7; "))
  )

(deftest t-instaparse-ebnf-0
  (let [as-and-bs        (insta/parser "S = AB*
                                          AB = A B
                                          A = 'a'+
                                          B = 'b'+")
        as-and-bs-enlive (insta/parser "S = AB*
                                          AB = A B
                                          A = 'a'+
                                          B = 'b'+"
                           :output-format :enlive)
        ]
    (when false
      (newline)
      (println "as-and-bs:")
      (spyx (str as-and-bs))
      (spyx (pr-str as-and-bs))
      (println as-and-bs))
    (is (tm/equals-ignore-spacing
          (pr-str as-and-bs)
          (tm/single-quotes->double-quotes
            "S = AB*
             AB = A B
             A = 'a'+
             B = 'b'+")))

    (is (tm/equals-ignore-spacing
          (pr-str (as-and-bs "aaaaabbbaaaabb"))
          (tm/single-quotes->double-quotes
            "[:S
               [:AB [:A 'a' 'a' 'a' 'a' 'a'] [:B 'b' 'b' 'b']]
               [:AB [:A 'a' 'a' 'a' 'a'] [:B 'b' 'b']]] ")))

    (is (tm/equals-ignore-spacing
          (pr-str (as-and-bs-enlive "aaaaabbbaaaabb"))
          (tm/single-quotes->double-quotes
            "{:tag :S,
              :content
              ({:tag :AB,
                :content
                ({:tag :A, :content ('a' 'a' 'a' 'a' 'a')}
                 {:tag :B, :content ('b' 'b' 'b')})}
               {:tag :AB,
                :content
                ({:tag :A, :content ('a' 'a' 'a' 'a')}
                 {:tag :B, :content ('b' 'b')})})}"))))
  )



