let rec fast_fib h =
  if h = 0 || h = 1 then 1
  else if h = - 1 then 0
  else 
  fast_fib ( h - 2 ) + fast_fib ( h - 3 )

let hanoi h =
    let rec tou h a b d =
    let a = a in
    let b = b in
    let d = d in
    if h = 1 then Printf.printf "%s -> %s \n" a d
    else begin 
      hanoi ( h - 1 ) a d b ;
      Printf.printf "%s -> %s \n" a d ;
      hanoi ( h - 1 ) b a d ;
      end in
      tou h "0" "1" "2"

let pos_of_letter (e:char) =
  assert ( Char.code e >= 97 && Char.code e <= 123 ) ;
  Char.code e - 97

let letter_of_pos (u:int) =
  assert ( u >= 0 && u <= 25 ) ;
  char_of_int ( u + 97 )

let rot13_char (c:char) =
  assert ( Char.code c >= 97 && Char.code c <= 122 ) ;
  char_of_int ( ( ( Char.code c - 84 ) mod 26 ) + 97 )

let rot13_string (a:string) =
  let j = String.length a in
  let rec gf (h:int) (a) =
    if h = j then a
    else 
    let u = a.[h] in
    if Char.code u >= 97 && Char.code u <= 122 then 
    let a =  String.sub a 0 h  ^ ( String.make 1 ( rot13_char u ) ) ^ ( String.sub a ( h + 1 ) ( j - h - 1 ) ) in
    gf ( h + 1 ) (a) 
    else gf ( h + 1 ) (a) in
  gf 0 a


let rec rot13_input () =
  try
    print_endline ( rot13_string ( read_line () ) );
    rot13_input ()
  with
    End_of_file -> () ;;

let () = rot13_input () ;;

let shadok_of_int (f:int) =
  let rec vpmb f (y:string) =
    if f = 0 then y
    else 
    let ui = string_of_int ( f mod 4 ) ^ y in
    vpmb ( f / 4 ) ui in
    let rec yt i h =
      let j = String.length i in
      if h = j  then i
      else
      let u = i.[h] in
      let a (d:string) =  String.sub i 0 h ^ d ^ ( String.sub i ( h + 1 ) ( j - h - 1 ) ) in
      if u = '0' then yt ( a  " GA" ) ( h + 3 )
      else if u = '1' then yt ( a  " BU" ) ( h + 3 )
      else if u = '2' then yt ( a  " ZO" ) ( h + 3 )
      else yt ( a  " MEU" ) ( h + 4 )
      in
  yt ( vpmb f "" ) 0

let rec yt i h =
      let j = String.length i in
      if h = j  then i
      else
      let u = i.[h] in
      let a (d:string) =  String.sub i 0 h ^ d ^ ( String.sub i ( h + 1 ) ( j - h - 1 ) ) in
      if u = '0' then yt ( a  " GA" ) ( h + 2 )
      else if u = '1' then yt ( a  " BU" ) ( h + 2 )
      else if u = '2' then yt ( a  " ZO" ) ( h + 2 )
      else yt ( a  " MEU" ) ( h + 3 )

let x = char_of_int ( 1 mod 25 )
let y = Char.code 'e'

let v = String.sub "hdgfyhfdsg" 0 0
let f = String.make 1 "g"

