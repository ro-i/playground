#!/usr/bin/python3
# coding=utf-8

from collections import OrderedDict
import datetime
import json
import re
import select
import signal
import subprocess
import sys
import time
import threading


global colors
global lineitems
global statusline
global update
statusline = OrderedDict()


###################
## BEGIN: config ##
###################

# colors to use in bar
colors = {
        "default": "#bdbdbd",
        "blue": "#46aede",
        "green": "#94e76b",
        "red": "#eb4509",
        "yellow": "#ffac18"
        }

#################
## END: config ##
#################


# set text for lineitem field
def __format_json_output(lineitem, text, color=colors["default"]):
    return {
            "name": lineitem,
            "full_text": text,
            "color": color,
            "separator_block_width": 19
            }


def __get_file_content(file):
    try:
        f = open(file)
        content = f.readlines()
        f.close()
    except:
        content = None
    finally:
        return content


# get single line from file
def __get_file_line(file):
    try:
        f = open(file)
        line = f.readline().rstrip('\n')
        f.close()
    except:
        line = None
    finally:
        return line


# get the output of a command
def __get_output(cmd):
    try:
        return subprocess.run(cmd, check=True, stdout=subprocess.PIPE,
                stderr=subprocess.STDOUT, text=True).stdout
    except:
        return None


# run command
def __run(cmd):
    subprocess.run(cmd, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)


# run command in shell and detach it
def __run_detached_in_shell(cmd):
    subprocess.run(cmd + " > /dev/null 2<&1 &", stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL, shell=True)


def backlight(button = 0):
    def num():
        digits = __get_file_line("/sys/class/backlight/intel_backlight/brightness")
        if digits is None:
            return None
        return int(digits)

    number = num()
    if number is None:
        return err()

    if button == 4 and number+1000 <= 120000:
        __run(["sudo", "/opt/sandy/setbacklight.sh", "+1000"])
    elif button == 5 and number-1000 >= 0:
        __run(["sudo", "/opt/sandy/setbacklight.sh", "-1000"])

    number = num()
    if number is None:
        return err()
    shown = str(int(number*100/120000))

    result = "☀ " + shown + "%"

    return __format_json_output("backlight", result)


def battery(button = 0):
    state = __get_file_line("/sys/class/power_supply/BAT0/status")
    if state is None:
        return err()
    cap = __get_file_line("/sys/class/power_supply/BAT0/capacity")
    if cap is None:
        return err()

    if state == "Full":
        result = cap + "% (=)"
        color = colors["blue"]
    elif state == "Charging":
        result = cap + "% (+)"
        color = colors["green"]
    elif state == "Discharging":
        result = cap + "% (-)"
        color = colors["red"]
    elif state == "Unknown":
        result = cap + "% (~)"
        color = colors["blue"]
    else:
        result = cap + "% (err)"
        color = colors["red"]

    return __format_json_output("battery", result, color)


def ethernet(button = 0):
    state = __get_file_line("/sys/class/net/eno2/operstate")
    if state is None:
        return err()

    if state == "down":
        result = "E"
        color = colors["red"]
    elif state == "up":
        speed = __get_file_line("/sys/class/net/eno2/speed")
        if speed is None:
            err()
        result = "E (" + speed + " MBit/s)"
        color = colors["green"]
    else:
        result = "error"
        color = colors["red"]

    return __format_json_output("ethernet", result, color)


def err():
    return __format_json_output("err", "error", colors["red"])


def time(button = 0):
    if button == 3:
        __run_detached_in_shell("gnome-terminal -- bash -c 'ncal -b -M -y | less'")

    now = datetime.datetime.now()
    result = now.strftime("%H:%M - %d.%m")
    return __format_json_output("time", result)


def volume(button = 0):
    if button == 2:
        # middle click to open pavucontrol-qt
        __run_detached_in_shell("pavucontrol-qt")
        return
    elif button == 3:
        # right klick to toggle mute/unmute
        output = __get_output(["amixer", "set", "Master", "toggle"])
    elif button == 4:
        # scroll up, increase
        output = __get_output(["amixer", "set", "Master", "2%+"])
    elif button == 5:
        # scroll down, decrease
        output = __get_output(["amixer", "set", "Master", "2%-"])
    else:
        output = __get_output(["amixer", "get", "Master"])
    
    if output is None:
        return err()

    vol = re.search('\d+%', output).group()
    state = re.search('(\[on\]|\[off\])', output).group()

    if state == "[off]":
        result = "♪ " + vol + " (-)"
    else:
        result = "♪ " + vol

    return __format_json_output("volume", result)


def volume_mic(button = 0):
    if button == 2:
        # middle click to open pavucontrol-qt
        __run_detached_in_shell("pavucontrol-qt")
        return
    elif button == 3:
        # right klick to toggle mute/unmute
        output = __get_output(["amixer", "set", "Capture", "toggle"])
    elif button == 4:
        # scroll up, increase
        output = __get_output(["amixer", "set", "Capture", "2%+"])
    elif button == 5:
        # scroll down, decrease
        output = __get_output(["amixer", "set", "Capture", "2%-"])
    else:
        output = __get_output(["amixer", "get", "Capture"])

    if output is None:
        return err()

    vol = re.search('\d+%', output).group()
    state = re.search('(\[on\]|\[off\])', output).group()

    if state == "[off]":
        result = "# " + vol + " (-)"
    else:
        result = "# " + vol

    return __format_json_output("volume_mic", result)


def wifi(button = 0):
    operstate = __get_file_line("/sys/class/net/wlo1/operstate")
    if operstate is None:
        return err()
    elif operstate == "down":
        return __format_json_output("wifi", "W down", colors["red"])

    content = __get_file_content("/proc/net/wireless")
    if content is None:
        return err()

    for line in content:
        if re.match("^wlo1", line):
            break
    dbm = int(re.search("[^-]*-([0-9]*).*", line).group(1))

    if dbm > 93:
        result = "0%"
    elif dbm < 20:
        result = "100%"
    else:
        wifi = int(100-(((dbm-20)**2)/53))
        result = str(wifi) + "%"

    return __format_json_output("wifi", result, colors["green"])


def do_output(full = False, special = ""):
    global statusline
    if special:
        # update only one block
        statusline[special] = lineitems[special]()
    elif full:
        # update all blocks
        for lineitem in lineitems:
            if not callable(lineitems[lineitem]):
                statusline[lineitem] = err()
            else:
                statusline[lineitem] = lineitems[lineitem]()
    else:
        # update block which shall be updated periodically
        for lineitem in update:
            statusline[lineitem] = lineitems[lineitem]()

    if not special:
        # rerun do_output() every ten seconds
        threading.Timer(10, do_output).start()
        # TODO: check return condition to exit parent process

    try:
        print(json.dumps(list(statusline.values())) + ",", flush=True)
    except:
        sys.exit(1)


############################
## BEGIN: lineitem config ##
############################

# Which lineitems shall be displayed, in which order an which function is
# responsible for them?
lineitems = OrderedDict([
    ("time", time),
    ("wifi", wifi),
    ("volume", volume),
    ("volume_mic", volume_mic),
    ("ethernet", ethernet),
    ("backlight", backlight),
    ("battery", battery)
    ])

# Which lineitems shall be updated every 10 seconds?
# (The others are updated only on a click event.)
update = [
        "time",
        "wifi",
        "battery"
        ]

##########################
## END: lineitem config ##
##########################


# header
header = {
        "version": 1,
        "click_events": True,
        "stop_signal": signal.SIGSTOP,
        "cont_signal": signal.SIGCONT
        }
print(json.dumps(header), flush=True)

# start infinite array (cf. https://i3wm.org/docs/i3bar-protocol.html)
print('[', flush=True)

do_output(full=True)

while True:
    output = sys.stdin.readline()
    try:
        tmp = output[output.index("{"):]
    except:
        continue
    line = json.loads(tmp)
    lineitem = line["name"]
    if lineitem in lineitems:
        result = lineitems[lineitem](line["button"])
        if result:
            statusline[lineitem] = result
        # update only this block
        do_output(special=lineitem)

