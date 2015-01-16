;;; @ dired
(setq dired-dwim-target t)
(defun dired-dwim-find-alternate-file ()
  "��ʕ����ɓK���� `dired-find-alternate-file'�D
�ʏ�� `dired-find-alternate-file' ���s�����C��ʕ�������Ă���
�����̃E�B���h�E�ɓ����o�b�t�@���\������Ă���� `dired-find-file'�D"
  (interactive)
  (cond
   ;; �����o�b�t�@������window�ɂ���ꍇ
   ((delq (selected-window) (get-buffer-window-list))
    (dired-find-file))
   ;; �����o�b�t�@������window�ɂȂ��ꍇ
   (t
    (dired-find-alternate-file))))

(defun dired-up-alternate-directory ()
  "�o�b�t�@�𑝂₳����̃f�B���N�g���Ɉړ��D"
  (interactive)
  (let* ((dir (dired-current-directory))
         (up (file-name-directory (directory-file-name dir))))
    (or (dired-goto-file (directory-file-name dir))
        ;; Only try dired-goto-subdir if buffer has more than one dir.
        (and (cdr dired-subdir-alist)
             (dired-goto-subdir up))
        (progn
          (find-alternate-file up)
          (dired-goto-file dir)))))
(defun dired-dwim-up-alternate-directory ()
  "��ʕ����ɓK���� `dired-up-alternate-directory'�D"
  (interactive)
  (cond
   ;; �����o�b�t�@������window�ɂ���ꍇ
   ((delq (selected-window) (get-buffer-window-list))
    (dired-up-directory))
   ;; �����o�b�t�@������window�ɂȂ��ꍇ
   (t
    (dired-up-alternate-directory))))


(defun dired-dwim-quit-window ()
  "��ʕ����ɓK���� `quit-window'�D"
  (interactive)
  (quit-window (not (delq (selected-window) (get-buffer-window-list)))))
;; userful dired
(require 'dired-x nil t)
(require 'wdired nil t)
(define-key dired-mode-map "r" 'wdir3ed-change-to-wdired-mode)

