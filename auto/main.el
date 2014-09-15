(TeX-add-style-hook "main"
 (lambda ()
    (TeX-run-style-hooks
     "preamble"
     "babel"
     ""
     "inputenx"
     "utf8"
     "fontenc"
     "T1"
     "latex2e"
     "scrbook10"
     "scrbook"
     "BCOR=5mm"
     "headinclude=true"
     "bibtotocnumbered"
     "english"
     "ngerman"
     "fleqn"
     "chapters/title-page"
     "chapters/preface"
     "chapters/introduction"
     "chapters/formal-concept-analysis"
     "chapters/description-logics"
     "chapters/axiomatizing-valid-gcis"
     "chapters/axiomatizing-confident-gcis"
     "chapters/exploration-by-confidence"
     "chapters/model-exploration-by-confidence"
     "chapters/conclusions"
     "chapters/affirmation")))

(TeX-add-style-hook "main"
 (lambda ()
   (setf (second reftex-insert-label-flags)
         (concatenate 'string (second reftex-insert-label-flags) "TLPDRCX"))
   (apply 'LaTeX-add-environments
          (mapcar (lambda (env) (list env 'LaTeX-env-label))
                  '("Theorem" "Lemma" "Proposition"
                    "Definition" "Remark" "Corollary" "Example")))
   (reftex-add-to-label-alist
    '(("Theorem"      ?T "thm:"  "~\\ref{%s}" t ("Theorem" "Thm."))
      ("Lemma"        ?L "lem:"  "~\\ref{%s}" t ("Lemma" "Lem."))
      ("Proposition"  ?P "prop:" "~\\ref{%s}" t ("Proposition" "Prop."))
      ("Definition"   ?D "def:"  "~\\ref{%s}" t ("Definition" "Def."))
      ("Remark"       ?R "rem:"  "~\\ref{%s}" t ("Remark" "Rem."))
      ("Corollary"    ?C "cor:"  "~\\ref{%s}" t ("Corollary" "Cor."))
      ("Example"      ?X "expl:" "~\\ref{%s}" t ("Example"))))))
