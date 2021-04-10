# Mandelbrot fractal
# Original code from https://code.activestate.com/recipes/577111-mandelbrot-fractal-using-pil/

from PIL import Image

if __name__ == "__main__":
    print("# running mandelbrot.py")

    # drawing area
    xa = -2.0
    xb = 1.0
    ya = -1.5
    yb = 1.5

    # max iterations allowed
    maxIt = 255

    # image size
    imgx = 768
    imgy = 768
    image = Image.new("RGB", (imgx, imgy))

    for y in range(imgy):
        zy = y * (yb - ya) / (imgy - 1) + ya
        for x in range(imgx):
            zx = x * (xb - xa) / (imgx - 1) + xa
            z = zx + zy * 1j
            c = z
            for i in range(maxIt):
                if abs(z) > 2.0: break
                z = z * z + c
            image.putpixel((x, y), (i % 4 * 64, i % 8 * 32, i % 16 * 16))

    image.save("mandelbrot.png")
    print("# saving mandelbrot.png")
    print("")
