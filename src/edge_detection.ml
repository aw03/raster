open! Core

let get_mirror_pix ~x ~y img : Pixel.t =
  Image.get
    img
    ~x:(max (min x (Image.width img - 1)) 0)
    ~y:(max (min y (Image.height img - 1)) 0)
;;

let kern_val ~(x : int) ~(y : int) (img : Image.t) (i : int) : int =
  Pixel.red (get_mirror_pix ~x ~y img) * i
;;

let calc_gx ~x ~y img =
  kern_val ~x:(x - 1) ~y:(y - 1) img (-1)
  + kern_val ~x:(x + 1) ~y:(y - 1) img 1
  + kern_val ~x:(x - 1) ~y img (-2)
  + kern_val ~x:(x + 1) ~y img 2
  + kern_val ~x:(x - 1) ~y:(y + 1) img (-1)
  + kern_val ~x:(x + 1) ~y:(y + 1) img 1
;;

let calc_gy ~x ~y img =
  kern_val ~x:(x - 1) ~y:(y - 1) img (-1)
  + kern_val ~x:(x + 1) ~y:(y - 1) img (-1)
  + kern_val ~x ~y:(y - 1) img (-2)
  + kern_val ~x ~y:(y + 1) img 2
  + kern_val ~x:(x - 1) ~y:(y + 1) img 1
  + kern_val ~x:(x + 1) ~y:(y + 1) img 1
;;

let calc_g ~x ~y img =
  int_of_float
    (sqrt
       ((float_of_int (calc_gx ~x ~y img) ** 2.)
        +. (float_of_int (calc_gy ~x ~y img) ** 2.)))
;;

let create_edge ~x ~y (img : Image.t) (threshold : int) : Pixel.t =
  let edge_val = calc_g ~x ~y img in
  if edge_val > threshold
  then Pixel.of_int (Image.max_val img)
  else Pixel.zero
;;

let transform ?(radius = 2) (image : Image.t) (threshold_percent : float)
  : Image.t
  =
  let threshold =
    int_of_float (threshold_percent *. float_of_int (Image.max_val image))
  in
  let blurred_grey_img =
    Blur.transform (Grayscale.transform image) ~radius
  in
  let edge_img =
    Image.mapi blurred_grey_img ~f:(fun ~x ~y _pix ->
      create_edge ~x ~y blurred_grey_img threshold)
  in
  edge_img
;;

let command =
  Command.basic
    ~summary:"Make edge image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and threshold =
        flag
          "threshold"
          (required Command.Param.float)
          ~doc:"The threshold for the edges, a float from 0 to 1"
      and radius =
        flag
          "radius"
          (optional Command.Param.int)
          ~doc:"the radius for the blur applied to image. Default 2"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        match radius with
        | None ->
          let image' = transform image threshold in
          Image.save_ppm
            image'
            ~filename:
              (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edges.ppm")
        | Some rad ->
          let image' = transform image threshold ~radius:rad in
          Image.save_ppm
            image'
            ~filename:
              (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_edges.ppm")]
;;
