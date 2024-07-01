open Core

(* You need to change the implementation of this function so that it does
   something to the image instead of just leaving it untouched. *)
let transform image =
  Image.map image ~f:(fun pix ->
    let av = (Pixel.red pix + Pixel.blue pix + Pixel.green pix) / 3 in
    Pixel.of_int av)
;;

let%expect_test "Grayscale image" =
  let transformed =
    transform
      (Image.load_ppm
         ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm")
  in
  print_s
    [%sexp
      (Image.equal
         transformed
         (Image.load_ppm
            ~filename:
              "/home/ubuntu/raster/images/reference-beach_portrait_gray.ppm")
       : bool)];
  [%expect "true"]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
