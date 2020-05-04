use std::clone::Clone;
use std::env;
use std::fmt;
use std::fs::File;
use std::io::Read;
use std::mem::swap;

fn main() {
    let args: Vec<String> = env::args().collect();
    let image_width = usize::from_str_radix(&args[1], 10).expect("Can't parse first param as width");
    let image_height = usize::from_str_radix(&args[2], 10).expect("Can't parse second param as height");
    let file_path = &args[3];
    let image = load_image(image_width, image_height, &file_path);
    println!("{}", image.calc_checksum());
}


fn load_image(width: usize, height: usize, file_path: &str) -> SpaceImage {
    let mut builder = SpaceImageBuilder::new(width, height);
    let mut f = File::open(file_path).expect("Unable to open file");
    let mut buf: [u8; 1] = [0];

    loop {
        match f.read(&mut buf) {
            Ok(1) => {
                match buf[0] {
                    b'0' => builder.append_pixel(PixelColour::Black),
                    b'1' => builder.append_pixel(PixelColour::White),
                    b'2' => builder.append_pixel(PixelColour::Transparent),
                    b'3' | b'4' | b'5' | b'6' | b'7' | b'8' | b'9' => builder.append_pixel(PixelColour::Other),
                    _ => ()  // ignore line breaks, whitespace etc.
                };
            },
            Ok(0) => {
                break;
            }
            Ok(_) | Err(_) => {
                panic!("File read failed");
            }
        }
    }
    match builder.build() {
        Ok(image) => {
            image
        }
        Err(s) => {
            panic!(format!("Bad image! {}", s));
        }
    }
}


struct SpaceImageBuilder {
    width: usize,
    height: usize,
    complete_layers: Vec<SpaceImageLayer>,
    wip_layer: Vec<Vec<PixelColour>>,
    wip_row: Vec<PixelColour>
}

impl SpaceImageBuilder {
    fn new(width: usize, height: usize) -> SpaceImageBuilder {
        SpaceImageBuilder{width: width, height: height,
            complete_layers: Vec::new(), wip_layer: Vec::new(),
            wip_row: Vec::new()}
    }

    fn append_pixel(&mut self, pixel: PixelColour) {
        self.wip_row.push(pixel);
        if self.wip_row.len() == self.width {
            let mut r = Vec::new();
            swap(&mut self.wip_row, &mut r);
            self.wip_layer.push(r);
            if self.wip_layer.len() == self.height {
                self.complete_layers.push(SpaceImageLayer::new(&self.wip_layer));
                self.wip_layer.clear();
            }
        }
    }

    fn build(&mut self) -> Result::<SpaceImage, String> {
        if self.wip_layer.is_empty() && self.wip_row.is_empty() {
            let mut layers = Vec::new();
            swap(&mut self.complete_layers, &mut layers);
            Ok(SpaceImage::new(self.width, self.height, layers))
        } else {
            Err(format!("Partial layer! {:?}", self.wip_row))
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
enum PixelColour {
    Black,
    White,
    Transparent,
    Other
}

impl fmt::Display for PixelColour {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            PixelColour::Black => "0",
            PixelColour::White => "1",
            PixelColour::Transparent => "2",
            PixelColour::Other => "x",
        })
    }    
}


struct SpaceImage {
    width: usize,
    height: usize,
    layers: Vec<SpaceImageLayer>,
}

impl SpaceImage {
    fn new(width: usize, height: usize, mut layers: Vec<SpaceImageLayer>) -> SpaceImage {
        layers.shrink_to_fit();
        SpaceImage{width: width, height: height, layers: layers}
    }

    fn calc_checksum(&self) -> i32 {
        let mut layer_index_with_least_zeroes = 0;
        let mut least_zeroes = self.layers[layer_index_with_least_zeroes].count(PixelColour::Black);
        for layer_index in 1..self.layers.len() {
            let zeroes_in_layer = self.layers[layer_index].count(PixelColour::Black);
            if zeroes_in_layer < least_zeroes {
                layer_index_with_least_zeroes = layer_index;
                least_zeroes = zeroes_in_layer;
            }
        }
        let layer_with_least_zeroes = &self.layers[layer_index_with_least_zeroes];
        layer_with_least_zeroes.count(PixelColour::White) *
            layer_with_least_zeroes.count(PixelColour::Transparent)
    }
}

impl fmt::Display for SpaceImage {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} x {}", self.width, self.height)
    }
}


struct SpaceImageLayer {
    rows: Vec<Vec<PixelColour>>,
}

impl SpaceImageLayer {
    fn new(rows: &Vec<Vec<PixelColour>>) -> SpaceImageLayer {
        let mut myrows = Vec::with_capacity(rows.len());
        for row in rows {
            let mut myrow = row.clone();
            myrow.shrink_to_fit();
            myrows.push(myrow);
        }
        SpaceImageLayer{rows: myrows}
    }

    fn count(&self, colour: PixelColour) -> i32 {
        let mut ret = 0;
        for row in &self.rows {
            ret = row.iter().fold(ret, |ret, x| if *x == colour {ret + 1} else {ret});
        }
        ret
    }
}