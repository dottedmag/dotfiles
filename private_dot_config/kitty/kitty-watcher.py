from typing import Any

from kitty.boss import Boss
from kitty.window import Window

from kitty.fast_data_types import current_focused_os_window_id
from pathlib import Path
from time import time

fh = None

# Keep cwds per window, so that we can report correct cwd for running programs from shells
slast_window_report = None

def on_load(boss: Boss, data: dict[str, Any]) -> None:
    db_dir = Path.home() / ".local" / "share" / "aw-watcher-kitty"
    db_dir.mkdir(mode=0o755, parents=True, exist_ok=True)

    global fh
    fh = open(db_dir / "data.txt", "w")

    print("Opened output file")

def on_focus_change(boss: Boss, window: Window, data: dict[str, Any])-> None:
    if data['focused']:
        if window.at_prompt:
            log_event(window, "<shell>")
        else:
            log_event(window, window.last_cmd_cmdline)
    else:
        if last_window_report == window.id:
            log_event(None, "")

def on_close(boss: Boss, window: Window, data: dict[str, Any])-> None:
    if last_window_report == window.id:
        log_event(None, "")

def on_cmd_startstop(boss: Boss, window: Window, data: dict[str, Any]) -> None:
    if data['is_start']:
        if is_window_focused(window):
            log_event(window, data['cmdline'])
    else:
        if last_window_report == window.id:
            log_event(window, "<shell>")

def cwd(w):
    return w.child.foreground_processes[0]["cwd"] or "<unknown>"

def is_window_focused(w):
    return w.os_window_id == current_focused_os_window_id() and w is w.tabref().active_window

def csv_escape(s):
    if "," not in s:
        return s
    return '"'+s.replace('"', '""')+'"'

def log_event(w, cmd):
    ts = int(1000*time())

    global last_window_report
    if w is None:
        last_window_report = None
        fh.write(f"{ts},,\n")
    else:
        last_window_report = w.id
        fh.write(f"{ts},{csv_escape(cwd(w))},{csv_escape(cmd)}\n")

    fh.flush()
