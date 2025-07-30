use std::sync::{Condvar, Mutex};

pub struct Semaphore {
    max_count: usize,
    count: Mutex<usize>,
    cvar: Condvar,
}

impl Semaphore {
    pub fn new(max_count: usize) -> Self {
        Self {
            max_count,
            count: Mutex::new(max_count),
            cvar: Condvar::new(),
        }
    }

    pub fn acquire(&self) -> SemaphoreGuard {
        self._acquire();
        SemaphoreGuard { semaphore: self }
    }

    pub fn try_acquire(&self) -> Option<SemaphoreGuard> {
        let mut count = self.count.lock().unwrap();
        if *count > 0 {
            *count -= 1;
            Some(SemaphoreGuard { semaphore: self })
        } else {
            None
        }
    }

    fn _acquire(&self) {
        let mut count = self.count.lock().unwrap();
        while *count == 0 {
            count = self.cvar.wait(count).unwrap();
        }
        *count -= 1;
    }

    fn _release(&self) {
        let mut count = self.count.lock().unwrap();
        assert!(*count < self.max_count, "Semaphore released too many times");
        *count += 1;
        self.cvar.notify_one();
    }
}

pub struct SemaphoreGuard<'a> {
    semaphore: &'a Semaphore,
}

impl<'a> Drop for SemaphoreGuard<'a> {
    fn drop(&mut self) {
        self.semaphore._release();
    }
}

impl<'a> SemaphoreGuard<'a> {
    pub fn with_released<R, F: FnOnce() -> R>(&self, func: F) -> R {
        self.semaphore._release();

        // If func() panics, we need to handle reacquisition carefully
        let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(func));

        self.semaphore._acquire(); // Always reacquire

        match result {
            Ok(value) => value,
            Err(panic) => std::panic::resume_unwind(panic),
        }
    }
}
