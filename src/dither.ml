open Core

let black = 0
let white img = Image.max_val img

let set_black_or_white img ~x ~y : int =
  match Image.get img ~x ~y with
  | pix when Pixel.red pix > white img / 2 ->
    let old_val = Pixel.red pix in
    Image.set img ~x ~y (Pixel.of_int (white img));
    old_val - white img
  | pix ->
    let old_val = Pixel.red pix in
    Image.set img ~x ~y Pixel.zero;
    old_val - black
;;

let check_inbounds ~(x : int) ~(y : int) (img : Image.t) : bool =
  x >= 0 && x < Image.width img && y >= 0 && y < Image.height img
;;

let _bounded_add (pix1 : Pixel.t) (pix2 : Pixel.t) (img : Image.t) : Pixel.t =
  match Pixel.( + ) pix1 pix2 with
  | pix when Pixel.red pix < 0 -> Pixel.zero
  | pix when Pixel.red pix > Image.max_val img -> Pixel.of_int (white img)
  | pix -> pix
;;

let calc_err_pix (pix : Pixel.t) ~(err : int) ~(adjust_by : float) _img
  : Pixel.t
  =
  (* bounded_add pix (Pixel.of_int (int_of_float (float_of_int err *.
     adjust_by))) img *)
  Pixel.( + )
    pix
    (Pixel.of_int (int_of_float (float_of_int err *. adjust_by)))
;;

let set_neighbor
  ~(x : int)
  ~(y : int)
  (img : Image.t)
  (err : int)
  ~(adjust_by : float)
  : unit
  =
  if check_inbounds ~x ~y img
  then (
    let new_pix = calc_err_pix ~err ~adjust_by (Image.get ~x ~y img) img in
    Image.set ~x ~y img new_pix)
  else ()
;;

let edit_neighbors
  (img : Image.t)
  (error : int)
  ~(x_mid : int)
  ~(y_mid : int)
  : unit
  =
  set_neighbor ~x:(x_mid + 1) ~y:y_mid img error ~adjust_by:(7.0 /. 16.0);
  set_neighbor ~x:(x_mid - 1) ~y:(y_mid + 1) img error ~adjust_by:(3. /. 16.);
  set_neighbor ~x:x_mid ~y:(y_mid + 1) img error ~adjust_by:(5.0 /. 16.0);
  set_neighbor ~x:(x_mid + 1) ~y:(y_mid + 1) img error ~adjust_by:(1. /. 16.)
;;

(* This should look familiar by now! *)
let transform (image : Image.t) : Image.t =
  let gray_img = Grayscale.transform image in
  let _ =
    Image.mapi gray_img ~f:(fun ~x ~y _pix ->
      let error = set_black_or_white ~x ~y gray_img in
      edit_neighbors gray_img error ~x_mid:x ~y_mid:y;
      Pixel.zero)
  in
  gray_img
;;

let%expect_test "Dither image" =
  let transformed =
    transform
      (Image.load_ppm
         ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm")
  in
  let reference_img =
    Image.load_ppm
      ~filename:
        "/home/ubuntu/raster/images/reference-beach_portrait_dither.ppm"
  in
  let _ =
    Image.mapi reference_img ~f:(fun ~x ~y pix ->
      if not (Pixel.equal pix (Image.get ~x ~y transformed))
      then (
        print_s
          [%sexp
            ((Pixel.red pix, Pixel.red (Image.get ~x ~y transformed))
             : int * int)];
        Pixel.zero)
      else Pixel.zero)
  in
  ();
  [%expect ""]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
