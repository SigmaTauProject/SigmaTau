/**
 * Creates a new, empty vec2
 *
 * @returns {vec2} a new 2D vector
 */
export default function create(x=0,y=0) {
    var out = new Float32Array(2)
    out[0] = x
    out[1] = y
    return out
}