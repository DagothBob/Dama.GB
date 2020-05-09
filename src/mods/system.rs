#![allow(non_camel_case_types)]
#![allow(clippy::single_match)]

extern crate sdl2;

use sdl2::pixels::Color;
use sdl2::event::Event;

use crate::cpu;
use crate::memory;

pub struct System {
    gb_cpu:cpu::CPU,
    gb_memory:memory::MemMap,
    sdl_context:sdl2::Sdl,
    quit:bool
}

impl System {
    pub fn init() -> System {
        System {
            gb_cpu:cpu::CPU::init(),
            gb_memory:memory::MemMap::init(),
            sdl_context:sdl2::init().unwrap(),
            quit:false
        }
    }

    pub fn system_loop(&mut self) {
        let video_subsystem = self.sdl_context.video().unwrap();
        let window = video_subsystem.window("Dama-GB", 640, 576).position_centered().build().unwrap();
        let mut canvas = window.into_canvas().build().unwrap();

        canvas.set_draw_color(Color::RGB(0, 0, 0));
        canvas.clear();
        canvas.present();

        while !self.quit {
            self.handle_sdl_events(self.sdl_context.event_pump().unwrap());
            self.gb_cpu.cpu_cycle();
        }
    }

    pub fn handle_sdl_events(&mut self, mut event_pump:sdl2::EventPump) {
        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} => {
                    self.quit = true
                },
                _ => {

                }
            }
        }
    }
}
