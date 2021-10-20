from threading import Timer
from threading import Event, Lock, Thread
from subprocess import Popen, PIPE, STDOUT
from time import monotonic

"""

# Usage if you want to make sure function finishes in less than x seconds:

watchdog = Watchdog(x)
try:
  # do something that might take too long
except Watchdog:
  # handle watchdog error
watchdog.stop()

"""

class Watchdog(Exception):
    def __init__(self, timeout, userHandler=None):  # timeout in seconds
        self.timeout = timeout
        self.handler = userHandler if userHandler is not None else self.defaultHandler
        self.timer = Timer(self.timeout, self.handler)
        self.timer.start()

    def reset(self):
        self.timer.cancel()
        self.timer = Timer(self.timeout, self.handler)
        self.timer.start()

    def stop(self):
        self.timer.cancel()

    def defaultHandler(self):
        raise self

"""

# A portable solution is to use a thread to kill the child process if reading a line takes too long:

#!/usr/bin/env python3
from subprocess import Popen, PIPE, STDOUT

timeout = 10
with Popen(command, stdout=PIPE, stderr=STDOUT,
           universal_newlines=True) as process:  # text mode
    # kill process in timeout seconds unless the timer is restarted
    watchdog = WatchdogTimer(timeout, callback=process.kill, daemon=True)
    watchdog.start()
    for line in process.stdout:
        # don't invoke the watcthdog callback if do_something() takes too long
        with watchdog.blocked:
            if not do_something(line):  # some criterium is not satisfied
                process.kill()
                break
            watchdog.restart()  # restart timer just before reading the next line
    watchdog.cancel()

"""

class WatchdogTimer(Thread):
    """Run *callback* in *timeout* seconds unless the timer is restarted."""

    def __init__(self, timeout, callback, *args, timer=monotonic, **kwargs):
        super().__init__(**kwargs)
        self.timeout = timeout
        self.callback = callback
        self.args = args
        self.timer = timer
        self.cancelled = Event()
        self.blocked = Lock()

    def run(self):
        self.restart() # don't start timer until `.start()` is called
        # wait until timeout happens or the timer is canceled
        while not self.cancelled.wait(self.deadline - self.timer()):
            # don't test the timeout while something else holds the lock
            # allow the timer to be restarted while blocked
            with self.blocked:
                if self.deadline <= self.timer() and not self.cancelled.is_set():
                    return self.callback(*self.args)  # on timeout

    def restart(self):
        """Restart the watchdog timer."""
        self.deadline = self.timer() + self.timeout

    def cancel(self):
        self.cancelled.set()