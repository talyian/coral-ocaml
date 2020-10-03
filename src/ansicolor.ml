open Printf

let tee f x =
  f x;
  x

type indexedColor =
  | BLACK
  | RED
  | GREEN
  | YELLOW
  | BLUE
  | MAGENTA
  | CYAN
  | WHITE

type color =
  | Clear
  | Color of indexedColor
  | Bold of indexedColor
  | Underline of indexedColor
  | RGB of (int * int * int)

let color_value = function
  | BLACK -> 30
  | RED -> 31
  | GREEN -> 32
  | YELLOW -> 33
  | BLUE -> 34
  | MAGENTA -> 35
  | CYAN -> 36
  | WHITE -> 37

let code = function
  | Clear -> "\027[0m"
  | Color c -> sprintf "\027[0;%dm" (color_value c)
  | Bold c -> sprintf "\027[1;%dm" (color_value c)
  | Underline c -> sprintf "\027[2;%dm" (color_value c)
  | RGB (a, b, c) -> sprintf "\027[38;5;%dm" (16 + c + (b * 6) + (a * 6 * 6))

let as_color c s = code c ^ s ^ code Clear

let as_rgb c s = as_color (RGB c) s

let flip f a b = f b a
