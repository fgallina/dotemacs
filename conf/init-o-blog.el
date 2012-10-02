(require 'org)
(require 'o-blog)

(add-to-list
 'o-blog-i18n
 '("es"
   :posted-on "Publicado en"
   :post-timestamp "%A %B, %d %Y a las %H:%M:%S"
   :post "Artículo"
   :posts "Artículos"
   :related-tags "tags relacionados"
   :home "Home"
   :tags "Tags"
   :archives "Archivos"
   :rss "RSS"
   :about "Acerca"
   :links "Enlaces"
   :redirect-header "Redirigiendo"
   :redirect-text "Oups! no deberías estar aqui. Por favor esperá la redirección."
   :redirect-link "Click aqui para ir al home."
   :powered-by "Generado con"
   :debug-blog "Volcado de información para el blog"
   :debug-post "Volcado de información para el artículo"
   :debug-tag "Volcado de información para el tag"
   :debug-property "Propiedad"
   :debug-value "Valor"
   :debug-blog-header "Blog"
   :debug-posts-header "Artículos"
   :debug-static-pages-header "Páginas estáticas"
   :debug-snippet-header "Snippets"
   :debug-tags-header "Tags"
   :category "Categoría"
   :categories "Categorías"
   :license-terms "Publicado bajo los terminos"))

(let ((en (assoc-string "en" o-blog-i18n)))
  (setf
   (cdr en)
   (cdr
    (append en
            (list
             :post "Post"
             :posts "Posts"
             :category "Category"
             :categories "Categories"
             :license-terms "Published under the terms of the")))))

(defun o-blog-update-last-modified ()
  (let ((time-stamp-line-limit 5)
        (time-stamp-format "%04y-%02m-%02d %02H:%02M:%02S")
        (time-stamp-start "^#\\+DATE:\\S+")
        (time-stamp-end "$"))
    (and (string= org-state "DONE")
         (time-stamp))))

(add-hook 'org-after-todo-state-change-hook 'o-blog-update-last-modified)
