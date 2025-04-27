#!/usr/bin/python3

import os, sys, time

sys.path.append('/usr/share/hatari/hconsole')

import hconsole

# emulatordir="$(realpath --relative-to="$PWD" "$(dirname "${BASH_SOURCE[0]}")")"
# basedir="$(realpath --relative-to="$PWD" "${emulatordir}/..")"
# cpmfilesdir="${basedir}/cpmfiles"

cwd = os.path.abspath(os.path.curdir)
emulatordir = os.path.relpath(os.path.dirname(sys.argv[0]), start=cwd)
basedir = os.path.normpath(os.path.join(emulatordir, '..'))
hatariworkdir = os.path.normpath(os.path.join(basedir, 'stfiles'))
donefile = os.path.normpath(os.path.join(hatariworkdir, 'DONE.TXT'))
print('cwd = %s' % cwd)
print('emulatordir = %s' % emulatordir)
print('basedir = %s' % basedir)
print('hatariworkdir = %s' % hatariworkdir)
print('donefile = %s' % donefile)
if os.path.exists(donefile):
  os.remove(donefile)

# forth="$1"
# forthcmd="$2"

forth = sys.argv[1] if len(sys.argv) > 1 else '4thcore.prg'
forthcmds = sys.argv[2:]

hatari_args = [
    'hatari', '--mono', '--sound', 'off', '--cpuclock', '32',
    '--fast-forward', 'true', # '--conout', '2',
    '--auto', forth, '--confirm-quit', 'false', hatariworkdir]

print('will create main')
main = hconsole.Main(hatari_args)
code = hconsole.Scancode
print('main created')

while not main.tokens.hatari.is_running():
  print('waiting for hatari to run')
  time.sleep(1)

time.sleep(3)

for cmd in forthcmds:
  main.run('text %s' % cmd)
  main.run('keypress %s' % code.Return)

if not 'KEEPEMU' in os.environ:
  main.run('text makefile done.txt')
  main.run('keypress %s' % code.Return)

while main.tokens.hatari.is_running() and not os.path.exists(donefile):
  print('waiting for %s' % donefile)
  time.sleep(1)
print('%s found' % donefile)

main.run("kill")
print('killed')

if os.path.exists(donefile):
  os.remove(donefile)
