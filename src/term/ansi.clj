(ns term.ansi
  "ANSI escape sequences"
  (:require [clojure.string :as string]))

(def ^:const unicode-escape "\u001b")

(defn escaped [code]
  (str unicode-escape code))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Top level commands

(def ^:const single-shift-two (escaped "N"))

(def ^:const single-shift-three (escaped "O"))

(def ^:const string-terminator (escaped "\\"))

(def ^:const reset-to-initial-state (escaped "c"))

(defn device-control-string
  [cmd]
  (escaped (str "P" cmd string-terminator)))

(defn control-sequence-introducer
  [cmd]
  (escaped (str "[" cmd)))

(defn operating-system-command
  [cmd]
  (escaped (str "]" cmd string-terminator)))

(defn start-of-string
  [text]
  (escaped (str "X" text string-terminator)))

(defn privacy-message
  [text]
  (escaped (str "^" text string-terminator)))

(defn application-program-command
  [text]
  (escaped (str "_" text string-terminator)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Control Sequences

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Cursor Movement

(defn cursor-up
  "Moves the cursor n cells up. If the cursor is already at
  the edge of the screen, this has no effect. "
  [& [n]]
  (control-sequence-introducer (str (or n 1) "A")))

(defn cursor-down
  "Moves the cursor n cells down. If the cursor is already at
  the edge of the screen, this has no effect. "
  [& [n]]
  (control-sequence-introducer (str (or n 1) "B")))

(defn cursor-forward
  "Moves the cursor n cells forward/right. If the cursor is already at
  the edge of the screen, this has no effect. "
  [& [n]]
  (control-sequence-introducer (str (or n 1) "C")))

(defn cursor-back
  "Moves the cursor n cells backward/left. If the cursor is already at
  the edge of the screen, this has no effect. "
  [& [n]]
  (control-sequence-introducer (str (or n 1) "D")))

(defn cursor-next-line
  "Moves cursor to beginning of the line n lines down."
  [& [n]]
  (control-sequence-introducer (str (or n 1) "E")))

(defn cursor-previous-line
  "Moves cursor to beginning of the line n lines up."
  [& [n]]
  (control-sequence-introducer (str (or n 1) "F")))

(defn cursor-horizontal-absolute
  "Moves the cursor to column n"
  [& [n]]
  (control-sequence-introducer (str (or n 1) "G")))

(defn cursor-position
  "Moves the cursor to row n and column m"
  [n m]
  (control-sequence-introducer (str n ";" m "H")))

(def ^:const device-status-report
  "Reports the cursor position (CPR) to the application as (as though typed at the keyboard)
  ESC[n;mR, where n is the row and m is the column.) "
  (control-sequence-introducer "6n"))

(def ^:const save-cursor-position
  "Save the cursor position"
  (control-sequence-introducer "s"))

(def ^:const restore-cursor-position
  "Restore the cursor position"
  (control-sequence-introducer "u"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Clearing Characters

(defn erase-in-display
  "Clears part of the screen.
  If n is 0 or missing, clear from cursor to end of screen.
  If n is 1, clear from cursor to beginning of the screen.
  If n is 2, clear entire screen.
  If n is 3, clear entire screen and delete all lines saved in the scrollback buffer"
  [& [n]]
  (control-sequence-introducer (str (or n 1) "J")))

(defn erase-in-line
  "Erases part of the line.
  If n is 0 or missing, clear from cursor to the end of the line.
  If n is 1, clear from cursor to beginning of the line.
  If n is 2, clear entire line.
  Cursor position does not change."
  [& [n]]
  (control-sequence-introducer (str (or n 0) "K")))

(def ^:const clear-screen
  "clear entire screen"
  (erase-in-display 2))

(def ^:const clear-line
  "clear entire line"
  (erase-in-line 2))

(def ^:const clear-line-left
  "clear from the beginning of the line to the cursor"
  (erase-in-line 1))

(def ^:const clear-line-right
  "clear from the beginning of the line to the cursor"
  (erase-in-line 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Scrolling

(defn scroll-up
  "Scroll whole page up by n lines. New lines are added at the bottom"
  ([& [n]]
   (control-sequence-introducer (str (or n 1) "S"))))

(defn scroll-down
  "Scroll whole page down by n lines. New lines are added at the top"
  ([& [n]]
   (control-sequence-introducer (str (or n 1) "T"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Select Graphic Rendition

(defn select-graphic-rendition
  "Apply appearance changes to the output text"
  [& codes]
  (control-sequence-introducer (str (if (empty? codes)
                                      0
                                      (string/join ";" codes))
                                    "m")))

(defmacro def-sgr [name code-seq & [docstring?]]
  (let [name (with-meta name
               (cond-> {}
                 (string? docstring?) (assoc :doc docstring?)))]
    `(def ^:const ~name (select-graphic-rendition ~code-seq))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Text effects

(def-sgr normal      0 "all attributes off")
(def-sgr bold        1)
(def-sgr faint       2 "Not widely supported")
(def-sgr italic      3 "Not widely supported. sometimes treated as inverse")
(def-sgr underlined  4)
(def-sgr blink       5)
(def-sgr fast-blink  6 "Not widely supported")
(def-sgr inverse     7 "reverse foreground and background colours")
(def-sgr conceal     8 "Not widely supported")
(def-sgr crossed-out 9 "Not widely supported")
(def strikethrough crossed-out)

(def-sgr fraktur 20 "hardly ever supported") ;; I wish my terminal were supported

(def-sgr not-bold        "21;22" "turn off bold and/or faint")
(def-sgr not-faint       22)
(def-sgr not-italic      23)
(def-sgr not-underlined  24)
(def-sgr not-blink       25)
(def-sgr not-inverse     27) ;; Isn't that just inverse?
(def-sgr reveal          28)
(def-sgr not-crossed-out 29)
(def not-conceal reveal)
(def not-strikethrough not-crossed-out)

(def-sgr framed                  51)
(def-sgr encircled               52)
(def-sgr overlined               53)
(def-sgr not-framed-or-encircled 54)
(def-sgr not-overlined           55)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Fonts

(def-sgr primary-font 10)

(defn font
  "if n is 0, default font.
  if n is between 1 and 9 inclusively switch to
  the corresponding alternate font"
  [n]
  (assert (<= 0 n 9) "expected font number between 0 and 9 inclusively") ;; Assert here to avoid ruining the sgr state
  (select-graphic-rendition (+ 10 n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;; Colours

(def-sgr default-foreground 39)
(def-sgr default-background 49)

;;TODO bright colours
(let [colour-key?         #{:black :red :green :blue :yellow
                            :magenta :cyan :white}
      colour-type-chooser (fn [& [a b c]]
                            (cond
                              (not a)         ::reset
                              (colour-key? a) ::colour-key
                              (int? a)        (if (and (int? b)
                                                       (int? c))
                                                ::rgb
                                                ::n)
                              :else           (type a)))]
  (defmulti foreground
    "Set the foreground color to one of:
  the 8 colour keywords,
  a single number for 8bit colour,
  three numbers for rgb values,
  or without argument to reset the colour"
    colour-type-chooser)

  (defmulti background
    "Set the background color to one of:
  the 8 colour keywords,
  a single number for 8-bit colour,
  three numbers for 24-bit rgb values,
  or without arguments to reset the colour"
      colour-type-chooser))

(defmethod foreground ::reset
  []
  default-foreground)

(let [foreground-colour-keys {:black   30
                              :red     31
                              :green   32
                              :yellow  33
                              :blue    34
                              :magenta 35
                              :cyan    36
                              :white   37}]
  (defmethod foreground ::colour-key
    [k]
    (select-graphic-rendition
     (get foreground-colour-keys k))))

(defmethod foreground ::n
  [n]
  (select-graphic-rendition (str "38;5;" n)))

(defmethod foreground ::rgb
  [r g b]
  (select-graphic-rendition (format "38;2;%s;%s;%s" r g b)))

(defmethod foreground java.awt.Color
  [^java.awt.Color colour]
  (foreground (.getRed colour) (.getGreen colour) (.getBlue colour)))

(defmethod background ::reset
  []
  default-background)

(let [background-colour-keys {:black   40
                              :red     41
                              :green   42
                              :yellow  43
                              :blue    44
                              :magenta 45
                              :cyan    46
                              :white   47}]
  (defmethod background ::colour-key
    [k]
    (select-graphic-rendition
     (get background-colour-keys k))))

(defmethod background ::n
  [n]
  (select-graphic-rendition (str "48;5;" n)))

(defmethod background ::rgb
  [r g b]
  (select-graphic-rendition (format "48;2;%s;%s;%s" r g b)))

(defmethod background java.awt.Color
  [^java.awt.Color colour]
  (background (.getRed colour) (.getGreen colour) (.getBlue colour)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; AUX Serial Port

(def ^:const aux-port-on
  "Enable aux serial port usually for local serial printer"
  (control-sequence-introducer "5i"))

(def ^:const aux-port-off
  "Disable aux serial port usually for local serial printer"
  (control-sequence-introducer "4i"))
