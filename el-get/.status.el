((anything status "installed" recipe
           (:name anything :website "http://www.emacswiki.org/emacs/Anything" :description "Open anything / QuickSilver-like candidate-selection framework" :type git :url "http://repo.or.cz/r/anything-config.git" :shallow nil :load-path
                  ("." "extensions" "contrib")
                  :features anything))
 (anything-git-files status "installed" recipe
                     (:name anything-git-files :type github :pkgname "tarao/anything-git-files-el" :depends anything))
 (apel status "installed" recipe
       (:name apel :website "http://www.kanji.zinbun.kyoto-u.ac.jp/~tomo/elisp/APEL/" :description "APEL (A Portable Emacs Library) is a library to support to write portable Emacs Lisp programs." :type github :pkgname "wanderlust/apel" :build
              (mapcar
               (lambda
                 (target)
                 (list el-get-emacs
                       (split-string "-batch -q -no-site-file -l APEL-MK -f")
                       target "prefix" "site-lisp" "site-lisp"))
               '("compile-apel" "install-apel"))
              :load-path
              ("site-lisp/apel" "site-lisp/emu")))
 (auto-complete status "installed" recipe
                (:name auto-complete :type github :website "https://github.com/auto-complete/auto-complete" :description "[My Recipes] Auto Complete Mode renews an old completion interface and provides an environment that makes users could be more concentrate on their own works." :pkgname "auto-complete/auto-complete"))
 (auto-complete-rst status "installed" recipe
                    (:name auto-complete-rst :description "Auto-complete extension for ReST and Sphinx" :type github :pkgname "tkf/auto-complete-rst" :depends auto-complete :prepare
                           (progn
                             (autoload 'auto-complete-rst-init "auto-complete-rst"))))
 (cl-lib status "installed" recipe
         (:name cl-lib :builtin "24.3" :type elpa :description "Properly prefixed CL functions and macros" :url "http://elpa.gnu.org/packages/cl-lib.html"))
 (ddskk status "installed" recipe
        (:name ddskk :website "http://openlab.ring.gr.jp/skk/ddskk.html" :description "A Japanese input method on Emacs." :type github :pkgname "skk-dev/ddskk" :autoloads nil :info "doc/skk.info" :features
               ("skk-setup")
               :build
               `((,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile")
                 (,el-get-emacs "-batch" "-q" "-no-site-file" "-l" "SKK-MK" "-f" "SKK-MK-compile-info")
                 ("mv" "skk-setup.el.in" "skk-setup.el"))))
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
 (elscreen status "installed" recipe
           (:name elscreen :description "Screen Manager for Emacsen" :website "https://github.com/emacs-jp/elscreen" :depends apel :type github :pkgname "emacs-jp/elscreen"))
 (open-github-from-here status "installed" recipe
                        (:name open-github-from-here :type github :description "open github from here" :pkgname "shibayu36/emacs-open-github-from-here" :branch "development"))
 (open-junk-file status "required" recipe nil)
 (popup status "installed" recipe
        (:name popup :type github :website "https://github.com/auto-complete/popup-el" :description "[My Recipes] This section describes the basic data structures and operations of popups." :pkgname "auto-complete/popup-el"))
 (powerline status "installed" recipe
            (:name powerline :website "https://github.com/milkypostman/powerline" :depends
                   (cl-lib)
                   :description "Powerline for Emacs" :type github :pkgname "milkypostman/powerline" :load-path "." :features powerline))
 (vagrant status "installed" recipe
          (:name vagrant :description "Manage a vagrant box from emacs" :type github :pkgname "ottbot/vagrant.el" :features vagrant)))
