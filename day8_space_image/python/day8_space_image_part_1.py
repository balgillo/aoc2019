import sys
import io

class SpaceImageLayer:
    def __init__(self, width, height, input):
        self.width = width
        self.height = height
        self.rows = []
        row_index = 0
        next_row_index = 1
        while next_row_index * width <= len(input):
            self.rows.append( 
                input[row_index * width : next_row_index * width])
            row_index = next_row_index
            next_row_index += 1

    def count_of(self, character):
        ret = 0
        for r in self.rows:
            for c in r:
                if c == character:
                    ret += 1
        return ret


class SpaceImage:
    def __init__(self, width, height, input):
        self.width = width
        self.height = height
        pixels_per_layer = width * height
        self.layers = []
        layer_index = 0
        next_layer_index = 1
        while next_layer_index * pixels_per_layer <= len(input):
            self.layers.append(SpaceImageLayer(width, height, 
                input[layer_index * pixels_per_layer : next_layer_index * pixels_per_layer]))
            layer_index = next_layer_index
            next_layer_index += 1

    def checksum(self):
        min_layer = self.layers[0]
        min_layer_zero_count = min_layer.count_of("0")
        for l in self.layers[1:]:
            l_zero_count = l.count_of("0")
            if l_zero_count < min_layer_zero_count:
                min_layer = l
                min_layer_zero_count = l_zero_count
        return min_layer.count_of("1") * min_layer.count_of("2")


image_width = int(sys.argv[1])
image_height = int(sys.argv[2])
file_path = sys.argv[3]
image = None
with io.open(file_path, "r") as f:
    line = f.readline()
    if line:
        image = SpaceImage(image_width, image_height, line)
    else:
        raise Exception("No image in file!")
print(image.checksum())
