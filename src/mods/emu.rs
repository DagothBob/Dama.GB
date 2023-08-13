

use sdl2::{Sdl, VideoSubsystem, video::Window, render::Canvas, pixels::Color};

use crate::gb::GB;

pub struct GBEmu {
    gb: GB,
    sdl: Sdl,
    video: VideoSubsystem,
    canvas: Canvas<Window>
}

impl GBEmu {
    pub fn init() -> Self {
        let sdl: Sdl = sdl2::init().unwrap();
        let video: VideoSubsystem = sdl.video().unwrap();
        let canvas: Canvas<Window> = video.window("Dama.GB", 160, 144)
                             .position_centered()
                             .build()
                             .unwrap()
                             .into_canvas().build().unwrap();

        GBEmu { 
            gb: GB::init(),
            sdl,
            video,
            canvas
        }
    }

    pub fn run(&mut self) {
        self.canvas.set_draw_color(Color::RGB(255, 0, 255));
        self.canvas.clear();
        self.canvas.present();

        loop {
            
        }
    }
}