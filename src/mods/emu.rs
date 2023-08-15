

use std::{time::{Instant, Duration}, thread};

use sdl2::{Sdl, VideoSubsystem, video::{Window, WindowContext}, render::{Canvas, Texture, TextureAccess, TextureCreator}, pixels::Color, EventPump, event::Event, keyboard::Keycode};

use crate::gb::GB;

const FRAME_TIME: Duration = Duration::from_nanos(16_750_419); // 59.7 FPS
const CYCLE_TIME: Duration = Duration::from_nanos(61); // 16384 Hz

pub struct GBEmu {
    gb: GB,
    sdl: Sdl,
    video: VideoSubsystem,
    canvas: Canvas<Window>,
    texture_creator: TextureCreator<WindowContext>,
    events: EventPump
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
        let events: EventPump = sdl.event_pump().unwrap();
        let texture_creator: TextureCreator<WindowContext> = canvas.texture_creator();

        GBEmu { 
            gb: GB::init(),
            sdl,
            video,
            canvas,
            texture_creator,
            events
        }
    }

    pub fn run(&mut self) {
        let mut screen_texture: Texture = self.texture_creator.create_texture_streaming(None, 
                                                                                  160, 
                                                                                 144)
                                                              .unwrap();
        self.canvas.set_draw_color(Color::RGB(255, 0, 255));
        self.canvas.clear();
        self.canvas.present();

        let mut i = 0;
        let mut frame_start: Instant;

        // Per-frame loop (59.7 FPS)
        'frame: loop {
            frame_start = Instant::now();
            i = (i + 1) % 255;

            for event in self.events.poll_iter() {
                match event {
                    Event::Quit {..} | Event::KeyDown { keycode: Some(Keycode::Escape), .. } => {
                        break 'frame
                    },
                    _ => {}
                }
            }

            // Per-cycle loop (16384Hz)
            'cycle: loop {
                // Enough cycles have elapsed -> Go to end of frame
                if frame_start.elapsed() >= CYCLE_TIME {
                    break 'cycle
                }
            }

            let pixel_data: [u8; 69120] = [i; 69120];
            screen_texture.update(None, &pixel_data, 480).unwrap();
            self.canvas.copy(&screen_texture, None, None).unwrap();
            self.canvas.present();
            thread::sleep(FRAME_TIME.saturating_sub(frame_start.elapsed()));
        }
    }
}