(TeX-add-style-hook "main"
 (lambda ()
    (TeX-run-style-hooks
     "babel"
     "english"
     "inputenc"
     "utf8"
     "fontenc"
     "T1"
     "latex2e"
     "scrbook10"
     "scrbook"
     "fleqn"
     "preamble"
     "chapters/title-page"
     "chapters/preface"
     "chapters/introduction"
     "chapters/formal-concept-analysis"
     "chapters/description-logics"
     "chapters/axiomatizing-valid-gcis"
     "chapters/axiomatizing-confident-gcis"
     "chapters/exploration-by-confidence"
     "chapters/model-exploration-by-confidence"
     "chapters/conclusions")))

(TeX-add-style-hook "main"
 (lambda ()
   (setf (second reftex-insert-label-flags)
         (concatenate 'string (second reftex-insert-label-flags) "T"))
   (apply 'LaTeX-add-environments
          (mapcar (lambda (env) (list env 'LaTeX-env-label))
                  '("Theorem" "Lemma" "Proposition" "Satz"
                    "Definition" "Remark" "Corollary" "Example")))
   (reftex-add-to-label-alist
    '(("Theorem"      ?T "thm:"  "~\\ref{%s}" t ("Theorem" "Thm."))
      ("Lemma"        ?T "lem:"  "~\\ref{%s}" t ("Lemma" "Lem."))
      ("Proposition"  ?T "prop:" "~\\ref{%s}" t ("Proposition" "Prop."))
      ("Satz"         ?T "thm:"  "~\\ref{%s}" t ("Satz"))
      ("Definition"   ?T "def:"  "~\\ref{%s}" t ("Definition" "Def."))
      ("Remark"       ?T "rem:"  "~\\ref{%s}" t ("Remark" "Rem."))
      ("Corollary"    ?T "cor:"  "~\\ref{%s}" t ("Corollary" "Cor."))
      ("Example"      ?T "expl:" "~\\ref{%s}" t ("Example"))))))
