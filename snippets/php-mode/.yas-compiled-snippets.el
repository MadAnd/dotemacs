;;; Compiled snippets and support files for `php-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
                     '(("yapp" "\\Yii::$app->params['${1:param_name}']$0" "\\Yii::$app->params[]" nil nil nil "/home/madand/.emacs/snippets/php-mode/yii-app-params" nil nil)
                       ("yap" "\\Yii::$app->" "\\Yii::$app->" nil nil nil "/home/madand/.emacs/snippets/php-mode/yii-app" nil nil)
                       ("t-" "$this->" "$this->" nil nil nil "/home/madand/.emacs/snippets/php-mode/this-field" nil nil)
                       ("ta" "$this->$1 = $0;" "$this->field" nil nil nil "/home/madand/.emacs/snippets/php-mode/this-assign" nil nil)
                       ("t" "$this" "$this" nil nil nil "/home/madand/.emacs/snippets/php-mode/this" nil nil)
                       ("nn" ". ' NOT NULL'" ". ' NOT NULL'" nil nil nil "/home/madand/.emacs/snippets/php-mode/sql-not-null" nil nil)
                       ("rt" "return $0" "return" nil nil nil "/home/madand/.emacs/snippets/php-mode/return" nil nil)
                       ("pubs" "public static function ${1}(${2})\n{\n  $0\n}" "public static function" nil nil nil "/home/madand/.emacs/snippets/php-mode/public-static-function" nil nil)
                       ("pub" "public function ${1}(${2})\n{\n  $0\n}" "public function" nil nil nil "/home/madand/.emacs/snippets/php-mode/public-function" nil nil)
                       ("pt" "<?php $0 ?>" "<?php ... ?>" nil nil nil "/home/madand/.emacs/snippets/php-mode/php-tags" nil nil)
                       ("pec" "<?= $0 ?>" "<?= ... ?>" nil nil nil "/home/madand/.emacs/snippets/php-mode/php-short-echo-tags" nil nil)
                       ("if" "if (${1:condition}) {\n    $0\n}" "if (...) { ... }" nil nil nil "/home/madand/.emacs/snippets/php-mode/if" nil nil)
                       ("fore" "foreach (${1:\\$var} as ${2:\\$key${2:$(identity \" => \")}}${3:\\$val}) {\n        $0\n}" "foreach" nil nil nil "/home/madand/.emacs/snippets/php-mode/foreach" nil nil)
                       ("el" "else {\n    $0\n}" "else { ... }" nil nil nil "/home/madand/.emacs/snippets/php-mode/else" nil nil)))


;;; Do not edit! File generated at Fri May 20 18:02:05 2016
