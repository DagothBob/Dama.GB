#![allow(non_camel_case_types)]

extern crate sdl2;

use sdl2::pixels::Color;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;

use crate::cpu;
use crate::memory;

pub struct System {
    pub gb_cpu:cpu::CPU,
    pub gb_memory:memory::MemMap,
    pub sdl_context:sdl2::Sdl,
    pub quit:bool
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

            // Halted/stopped and interrupts enabled means CPU will halt instruction flow
            // Else, turn halt/stop flag off
            if !self.gb_cpu.halt && !self.gb_cpu.stop && self.gb_cpu.ime {
                self.gb_cpu.cpu_cycle();
            }
            else if (self.gb_cpu.halt || self.gb_cpu.stop) && !self.gb_cpu.ime {
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
            }

            self.interrupt_handler();
        }
    }

    pub fn interrupt_handler(&mut self) -> u8 {
        let int_e = self.gb_memory.get_byte(memory::IE as usize);
        let int_f = self.gb_memory.get_byte(memory::IF as usize);

        if self.gb_cpu.ime {
            self.gb_cpu.op_call(); // Push PC onto stack

            // Check that interrupt-enable and appropriate interrupt flag is set
            // Ordered how they are precedented on GB hardware
            if (int_e & memory::IE_VBLNK) == memory::IE_VBLNK &&
               (int_f & memory::IF_VBLNK) == memory::IF_VBLNK {
                self.gb_memory.set_memory(memory::IF as usize, int_f & !memory::IF_VBLNK);
                self.gb_cpu.ime = false;
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
                self.gb_cpu.registers.pc = memory::VBLN;
                memory::IF_VBLNK
            }
            else if (int_e & memory::IE_LSTAT) == memory::IE_LSTAT &&
                    (int_f & memory::IF_LSTAT) == memory::IF_LSTAT {
                self.gb_memory.set_memory(memory::IF as usize, int_f & !memory::IF_LSTAT);
                self.gb_cpu.ime = false;
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
                self.gb_cpu.registers.pc = memory::LSTT;
                memory::IF_LSTAT
            }
            else if (int_e & memory::IE_TIMER) == memory::IE_TIMER &&
                    (int_f & memory::IF_TIMER) == memory::IF_TIMER {
                self.gb_memory.set_memory(memory::IF as usize, int_f & !memory::IF_TIMER);
                self.gb_cpu.ime = false;
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
                self.gb_cpu.registers.pc = memory::TIMR;
                memory::IF_TIMER
            }
            else if (int_e & memory::IE_SRIAL) == memory::IE_SRIAL &&
                    (int_f & memory::IF_SRIAL) == memory::IF_SRIAL {
                self.gb_memory.set_memory(memory::IF as usize, int_f & !memory::IF_SRIAL);
                self.gb_cpu.ime = false;
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
                self.gb_cpu.registers.pc = memory::SRAL;
                memory::IF_SRIAL
            }
            else if (int_e & memory::IE_JYPAD) == memory::IE_JYPAD &&
                    (int_f & memory::IF_JYPAD) == memory::IF_JYPAD {
                self.gb_memory.set_memory(memory::IF as usize, int_f & !memory::IF_JYPAD);
                self.gb_cpu.ime = false;
                self.gb_cpu.halt = false;
                self.gb_cpu.stop = false;
                self.gb_cpu.registers.pc = memory::JPAD;
                memory::IF_JYPAD
            }
            else {
                0
            }
        }
        else {
            0
        }
    }

    pub fn handle_sdl_events(&mut self, mut event_pump:sdl2::EventPump) {
        let pad_state = self.gb_memory.get_byte(memory::P1 as usize);
        let int_f = self.gb_memory.get_byte(memory::IF as usize);

        for event in event_pump.poll_iter() {
            match event {
                Event::Quit {..} => {
                    self.quit = true
                },
                Event::KeyDown { keycode: Some(Keycode::Right), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_RIGHT);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::Left), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_LEFT);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::Up), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_UP);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::Down), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_DOWN);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::Z), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_ABUTT);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::X), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_BBUTT);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::C), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_SELEC);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                Event::KeyDown { keycode: Some(Keycode::V), .. } => {
                    self.gb_memory.set_memory(memory::P1 as usize, pad_state | memory::P1_START);
                    self.gb_memory.set_memory(memory::IF as usize, int_f | memory::IF_JYPAD)
                },
                _ => {

                }
            }
        }
    }
}
