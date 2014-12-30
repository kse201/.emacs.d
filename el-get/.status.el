((anything status "installed" recipe
           (:name anything :website "http://www.emacswiki.org/emacs/Anything" :description "Open anything / QuickSilver-like candidate-selection framework" :type git :url "http://repo.or.cz/r/anything-config.git" :shallow nil :load-path
                  ("." "extensions" "contrib")
                  :features anything))
 (anything-config status "installed" recipe
                  (:name anything-config :auto-generated t :type emacswiki :description "Applications libary for `anything.el'" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/anything-config.el"))
 (anything-git-files status "installed" recipe
                     (:name anything-git-files :type github :pkgname "tarao/anything-git-files-el"))
 (auto-complete status "installed" recipe
                (:name auto-complete :type github :website "https://github.com/auto-complete/auto-complete" :description "[My Recipes] Auto Complete Mode renews an old completion interface and provides an environment that makes users could be more concentrate on their own works." :pkgname "auto-complete/auto-complete"))
 (auto-complete+ status "installed" recipe
                 (:name auto-complete+ :auto-generated t :type emacswiki :description "Auto complete plus" :website "https://raw.github.com/emacsmirror/emacswiki.org/master/auto-complete+.el"))
 (auto-complete-c-headers status "installed" recipe
                          (:name auto-complete-c-headers :website "https://github.com/mooz/auto-complete-c-headers" :description "An auto-complete source for C/C++ header files." :type github :pkgname "mooz/auto-complete-c-headers" :depends auto-complete :prepare
                                 (progn
                                   (defun ac--c-headers-init nil
                                     (require 'auto-complete-c-headers)
                                     (add-to-list 'ac-sources 'ac-source-c-headers))
                                   (add-hook 'c-mode-hook 'ac--c-headers-init)
                                   (add-hook 'c++-mode-hook 'ac--c-headers-init))))
 (auto-complete-clang status "installed" recipe
                      (:name auto-complete-clang :website "https://github.com/brianjcj/auto-complete-clang" :description "Auto-complete sources for Clang. Combine the power of AC, Clang and Yasnippet." :type github :pkgname "brianjcj/auto-complete-clang" :depends auto-complete))
 (auto-complete-clang-extension status "installed" recipe
                                (:name auto-complete-clang-extension :auto-generated t :type emacswiki :description "Extension for Clang auto completion." :website "https://raw.github.com/emacsmirror/emacswiki.org/master/auto-complete-clang-extension.el"))
 (auto-complete-emacs-lisp status "installed" recipe
                           (:name auto-complete-emacs-lisp :description "Auto-complete sources for emacs lisp" :type http :url "http://www.cx4a.org/pub/auto-complete-emacs-lisp.el" :depends auto-complete))
 (auto-complete-etags status "installed" recipe
                      (:name auto-complete-etags :type emacswiki :description "Auto-complete sources for etags" :depends auto-complete))
 (auto-complete-extension status "installed" recipe
                          (:name auto-complete-extension :type emacswiki :description "Some extension for auto-complete-mode" :depends auto-complete))
 (auto-complete-latex status "installed" recipe
                      (:name auto-complete-latex :description "A LaTeX extention for auto-complete-mode" :website "https://bitbucket.org/tequilasunset/auto-complete-latex" :url "https://bitbucket.org/tequilasunset/auto-complete-latex" :type hg :depends auto-complete))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (el-get status "installed" recipe
         (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "master" :pkgname "dimitri/el-get" :info "." :compile
                ("el-get.*\\.el$" "methods/")
                :load "el-get.el" :post-init
                (when
                    (memq 'el-get
                          (bound-and-true-p package-activated-list))
                  (message "Deleting melpa bootstrap el-get")
                  (unless package--initialized
                    (package-initialize t))
                  (when
                      (package-installed-p 'el-get)
                    (let
                        ((feats
                          (delete-dups
                           (el-get-package-features
                            (el-get-elpa-package-directory 'el-get)))))
                      (el-get-elpa-delete-package 'el-get)
                      (dolist
                          (feat feats)
                        (unload-feature feat t))))
                  (require 'el-get))))
 (open-github-from-here status "installed" recipe
                        (:name open-github-from-here :type github :description "open github from here" :pkgname "shibayu36/emacs-open-github-from-here" :branch "development"))
 (popup status "installed" recipe
        (:name popup :type github :website "https://github.com/auto-complete/popup-el" :description "[My Recipes] This section describes the basic data structures and operations of popups." :pkgname "auto-complete/popup-el"))
 (powerline status "installed" recipe
            (:name powerline :website "https://github.com/milkypostman/powerline" :depends
                   (cl-lib)
                   :description "Powerline for Emacs" :type github :pkgname "milkypostman/powerline" :load-path "." :features powerline))
 (twittering-mode status "installed" recipe
                  (:name twittering-mode :description "Major mode for Twitter" :type github :pkgname "hayamiz/twittering-mode" :features twittering-mode :compile "twittering-mode.el")))
