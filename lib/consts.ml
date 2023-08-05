let log_progress = ref false
let log string = if !log_progress then print_endline string
let init_infty = ref 20
let infty_step = ref 20
let minify = ref true

let time f t =
  let init = Sys.time () in
  let ans = f () in
  t := !t +. (Sys.time () -. init);
  ans
;;
