open Vec

let size, width, height = 300, 300, 200
let sizef, widthf, heightf = float_of_int size, float_of_int width, float_of_int height

let pi = 3.14
let o, theta, phi, fov_theta, fov_phi = ref (vec3 0. 0. 0.), ref (pi /. 4.), ref 0., pi /. 2., pi /. 2. *. heightf /. widthf
