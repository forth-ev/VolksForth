rem erstellt fÅr schnock&frosch-installation             clv09apr90
rem Ñndert Attribut der Statuszeile auf sichtbar, setzt path und installiert
rem volksFORTH kann nur 80 Zeichen in der Kommandozeile haben, deshalb 2 mal:
term4 path d:\clv\q\vf\term4;c:\apps\forth\vf;  saveterm x.com            endterm
x     $7f70 atts !  300. baud 8+1 halfduplex  saveterm c:\progs\term4.com endterm
del x.com
copy term4.doc c:\doc
