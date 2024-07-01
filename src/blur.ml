open Core

let get_lower_bound offset = if offset < 0 then 0 else offset
let upp_bound offset max = if offset > max - 1 then max - 1 else offset

let get_sub_image ~radius ~x_mid ~y_mid ~original =
  let x_start = get_lower_bound (x_mid - radius) in
  let y_start = get_lower_bound (y_mid - radius) in
  let x_end = upp_bound (x_mid + radius) (Image.width original) in
  let y_end = upp_bound (y_mid + radius) (Image.height original) in
  Image.slice original ~x_start ~y_start ~x_end ~y_end
;;

let blurred_pix split_img = Image.mean_pixel split_img

(* You need to modify this function to blur the input image based on the
   provided radius instead of ignoring it. *)
let transform image ~radius : Image.t =
  Image.mapi image ~f:(fun ~x ~y _pix ->
    blurred_pix (get_sub_image ~radius ~x_mid:x ~y_mid:y ~original:image))
;;

let%expect_test "Bluescreen Image" =
  let transformed =
    transform
      (Image.load_ppm
         ~filename:"/home/ubuntu/raster/images/beach_portrait.ppm")
      ~radius:3
  in
  print_s
    [%sexp
      (Image.equal
         transformed
         (Image.load_ppm
            ~filename:
              "/home/ubuntu/raster/images/reference-beach_portrait_blur.ppm")
       : bool)];
  [%expect "true"]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
