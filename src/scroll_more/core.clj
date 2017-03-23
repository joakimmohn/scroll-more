(ns scroll-more.core
  (:require
   [reagent.core :as r]))

(defn- get-scroll-top []
  (if (exists? (.-pageYOffset js/window))
    (.-pageYOffset js/window)
    (.-scrollTop (or (.-documentElement js/document)
                     (.-parentNode (.-body js/document))
                     (.-body js/document)))))

(defn- get-el-top-position [node]
  (if (not node)
    0
    (+ (.-offsetTop node)
       (get-el-top-position
       (.-offsetParent node)))))

(defn- safe-component-mounted? [component]
  (try (boolean (r/dom-node component)) (catch js/Object _ false)))

(defn- debounce
  ([threshold f] (debounce threshold f (constantly nil)))
  ([threshold f prep-fn]
   (let [t (atom nil)]
    (fn [& args]
      (when @t (js/clearTimeout @t))
      (apply prep-fn args)
      (reset! t (js/setTimeout #(do
                                  (reset! t nil)
                                  (apply f args))
                               threshold))))))

(defn scroll-more [props]
  (let [listener-fn (atom nil)
        detach-scroll-listener  (fn []
                                  (when @listener-fn
                                   (.removeEventListener js/window "scroll" @listener-fn)
                                   (.removeEventListener js/window "resize" @listener-fn)
                                   (reset! listener-fn nil)))
        scroll-listener (fn [this]
                          (when (safe-component-mounted? this)
                            (let [{:keys [load-fn]} (r/props this)
                                  node (r/dom-node this)
                                  scroll-top (get-scroll-top)
                                  my-top (get-el-top-position node) 
                                  threshold 5
                                  should-load-more? (< (- (+ my-top (.-offsetHeight node))
                                                          scroll-top
                                                          (.-innerHeight js/window))
                                                       threshold)]
                              (when should-load-more?
                                (detach-scroll-listener)
                                (load-fn)))))
        debounced-scroll-listener (debounce 200 scroll-listener)
        attach-scroll-listener (fn [this]
                                 (let [{:keys [can-show-more?]} (r/props this)]
                                   (when can-show-more?
                                     (when-not @listener-fn
                                       (reset! listener-fn (partial debounced-scroll-listener this))
                                       (.addEventListener js/window "scroll" @listener-fn)
                                       (.addEventListener js/window "resize" @listener-fn))
                                     (scroll-listener this))) 500)]
    (r/create-class
      {:component-did-mount
       (fn [this]
         (attach-scroll-listener this))
       :component-did-update
       (fn [this _]
         (attach-scroll-listener this))
       :component-will-unmount
       (fn []
         (detach-scroll-listener))
       :reagent-render
       (fn [props]
         [:div])})))
