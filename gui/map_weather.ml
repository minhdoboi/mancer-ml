(* The weather over the campaign map: clouds pass over the painted valley and
   throw soft shadows across it, sunlight comes through the gaps, and dust
   drifts in the light.

   It is a day effect.  The second chapter fades it out together with its own
   cold tint, so the shader carries the night maths too and the two cross
   over in one pass; that maths is the one of assets/shaders/night.fs and has
   to follow it. *)

open Mancer
module R = Raylib

(* The sun sits over the mountains, in the upper left of the visible map. *)
let sun_u = 0.16
let sun_v = 0.10

(* ------------------------------------------------------------- the shader *)

(* The map is drawn through this: the cloud shadows and the sunlight are part
   of the picture, and the night tint of the second chapter rides on the same
   pass (the maths of assets/shaders/night.fs). *)
let weather_vs =
  {|#version 330

in vec3 vertexPosition;
in vec2 vertexTexCoord;
in vec4 vertexColor;

out vec2 fragTexCoord;
out vec4 fragColor;

uniform mat4 mvp;

void main() {
  fragTexCoord = vertexTexCoord;
  fragColor = vertexColor;
  gl_Position = mvp * vec4(vertexPosition, 1.0);
}
|}

let weather_fs =
  {|#version 330

in vec2 fragTexCoord;
in vec4 fragColor;

uniform sampler2D texture0;
uniform vec4 colDiffuse;
uniform float u_time;
uniform float u_night;
uniform vec2 u_sun;

out vec4 finalColor;

float hash(vec2 p) {
  vec3 p3 = fract(vec3(p.xyx) * 0.1031);
  p3 += dot(p3, p3.yzx + 33.33);
  return fract((p3.x + p3.y) * p3.z);
}

float vnoise(vec2 p) {
  vec2 i = floor(p);
  vec2 f = fract(p);
  vec2 u = f * f * (3.0 - 2.0 * f);
  float a = hash(i);
  float b = hash(i + vec2(1.0, 0.0));
  float c = hash(i + vec2(0.0, 1.0));
  float d = hash(i + vec2(1.0, 1.0));
  return mix(mix(a, b, u.x), mix(c, d, u.x), u.y);
}

float fbm(vec2 p) {
  float v = 0.0;
  float a = 0.5;
  for (int i = 0; i < 4; i++) {
    v += a * vnoise(p);
    p = p * 2.11 + vec2(3.7, 9.1);
    a *= 0.5;
  }
  return v;
}

float fbm3(vec2 p) {
  float v = 0.0;
  float a = 0.5;
  for (int i = 0; i < 3; i++) {
    v += a * vnoise(p);
    p = p * 2.11 + vec2(3.7, 9.1);
    a *= 0.5;
  }
  return v;
}

void main() {
  vec4 tex = texture(texture0, fragTexCoord) * fragColor * colDiffuse;
  vec3 day = tex.rgb;

  // The clouds drift east over the valley, a slow body with quicker wisps.
  vec2 q = fragTexCoord * vec2(1.25, 0.95) + vec2(u_time * 0.010, u_time * 0.004);
  float body = fbm3(q);
  float wisp = fbm3(q * 2.6 + vec2(u_time * 0.03, u_time * 0.01));
  float cloud = body * 0.82 + wisp * 0.18;
  float shade = smoothstep(0.50, 0.66, cloud);

  // Sunlight through the gaps: rays fanning out of the sun, cut by the clouds.
  vec2 d = fragTexCoord - u_sun;
  float dist = length(d);
  float ang = atan(d.y, d.x);
  float ray = fbm3(vec2(ang * 1.9, dist * 1.6 - u_time * 0.02));
  ray = smoothstep(0.52, 0.88, ray) * (1.0 - smoothstep(0.05, 0.9, dist));
  float light = ray * (1.0 - shade);

  vec3 col = day * (1.0 - 0.38 * shade);
  col = mix(col, col * vec3(0.80, 0.89, 1.10), shade * 0.6);
  col += vec3(1.0, 0.90, 0.70) * (1.0 - shade) * 0.04;
  col += vec3(1.0, 0.80, 0.48) * light * 0.16;
  col += vec3(1.0, 0.88, 0.62) * (1.0 - smoothstep(0.0, 0.32, dist)) * 0.05;

  // The night chapter: the same map under a cold moon.
  float grey = dot(day, vec3(0.30, 0.59, 0.11));
  vec3 moon = mix(day, vec3(grey), 0.38) * vec3(0.26, 0.36, 0.70);
  moon += vec3(0.03, 0.05, 0.12);
  vec2 dv = fragTexCoord - vec2(0.5);
  float vig = smoothstep(0.92, 0.18, dot(dv, dv) * 2.15);
  moon *= mix(0.52, 1.0, vig);
  col = mix(col, moon, clamp(u_night, 0.0, 1.0));

  finalColor = vec4(col, tex.a);
}
|}

type weather_shader = {
  sh : R.Shader.t;
  time : R.ShaderLoc.t;
  night : R.ShaderLoc.t;
  sun : R.ShaderLoc.t;
}

let shader : weather_shader option ref = ref None

let loc_ok l = R.ShaderLoc.to_int l >= 0

let set_float sh l v =
  if loc_ok l then (
    let p = Ctypes.(allocate float v) in
    R.set_shader_value sh l (Ctypes.to_voidp p) R.ShaderUniformDataType.Float)

let set_vec2 sh l x y =
  if loc_ok l then (
    let p = Ctypes.(allocate_n float ~count:2) in
    Ctypes.(p <-@ x);
    Ctypes.(p +@ 1 <-@ y);
    R.set_shader_value sh l (Ctypes.to_voidp p) R.ShaderUniformDataType.Vec2)

let get_shader () =
  match !shader with
  | Some s -> Some s
  | None ->
      let sh = R.load_shader_from_memory weather_vs weather_fs in
      let s =
        if R.is_shader_valid sh then
          Some
            {
              sh;
              time = R.get_shader_location sh "u_time";
              night = R.get_shader_location sh "u_night";
              sun = R.get_shader_location sh "u_sun";
            }
        else None
      in
      shader := s;
      s

(* ---------------------------------------------------------- dust in the light *)

let mote_count = 60

let rnd n =
  let x = sin ((n *. 12.9898) +. 78.233) *. 43758.5453 in
  x -. floor x

let draw_motes ~night =
  let strength = 1. -. night in
  if strength > 0.02 then (
    let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
    let t = R.get_time () in
    R.begin_blend_mode R.BlendMode.Additive;
    for i = 0 to mote_count - 1 do
      let seed = float i in
      let bx = rnd ((seed *. 3.) +. 1.) in
      let by = rnd ((seed *. 3.) +. 2.) in
      let speed = 0.006 +. (rnd ((seed *. 7.) +. 3.) *. 0.012) in
      let phase = rnd ((seed *. 13.) +. 5.) *. 6.28318 in
      let radius = 0.9 +. (rnd ((seed *. 11.) +. 7.) *. 1.7) in
      let drift = bx +. (t *. speed) in
      let drift = drift -. floor drift in
      let bob = by +. (sin ((t *. 0.35) +. phase) *. 0.03) in
      let y = bob -. floor bob in
      (* Motes only catch the light near the sun. *)
      let dx = drift -. sun_u and dy = y -. sun_v in
      let near = 1. -. Float.min 1. (sqrt ((dx *. dx) +. (dy *. dy)) /. 1.2) in
      let twinkle = 0.5 +. (0.5 *. sin ((t *. 1.7) +. phase)) in
      let a = near *. near *. 180. *. twinkle *. strength in
      if a > 3. then
        R.draw_circle_v (R.Vector2.create (drift *. sw) (y *. sh)) (radius *. strength)
          (R.Color.create 255 236 196 (int_of_float (Float.min 255. a)))
    done;
    R.end_blend_mode ())

(* --------------------------------------------------------------- the map *)

(* [night] is the night chapter's fade, 0 by day and 1 at night. *)
let draw_map ~night (tex : R.Texture.t) =
  let sw = float (R.get_screen_width ()) and sh = float (R.get_screen_height ()) in
  let crop = Gui_draw.cover_src tex in
  match get_shader () with
  | None -> Fx.with_night night (fun () -> Gui_draw.cover tex)
  | Some s ->
      set_float s.sh s.time (R.get_time ());
      set_float s.sh s.night night;
      set_vec2 s.sh s.sun sun_u sun_v;
      R.begin_shader_mode s.sh;
      R.draw_texture_pro tex crop (R.Rectangle.create 0. 0. sw sh) (R.Vector2.create 0. 0.) 0.
        (Gui_draw.alpha_color R.Color.white);
      R.end_shader_mode ();
      draw_motes ~night
