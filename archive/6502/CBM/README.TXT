README.TXT      zu museum/ultra4th  fÃ¼r Commodore C16, C64, Plus4         clv2000

        Claus.Vogt@Berlin.de       --- http://www.home.pages.de/~clv

        Dieser Bereich enthält Software für Commodores C64, C16 und Plus4
        und ein bißchen Software, um ultraForth-Screens auf PC und Atari ST weiterzunutzen.

        Es handelt sich um das Forth-System ultraForth 3.8,
        dessen Portierung auf C16/+4 ich ca. 1987 vornahm.

        Autoren des volksForth/ultraForth sind

        - Bernd Pennemann,
        - Claus Vogt,
        - Dietrich Weineck,
        - Georg Rehfeld,
        - Klaus Schleisieck,
        - Ulrich Hoffmann.

        Es existierte für ultraForth eine Kassettenversion, die sich
        auf einer dieser Disketten-Abbilder befinden könnte.
        Da ich grade keinen C16 zur Verfügung habe, kann ich leider
        nicht sagen, auf welchem :-( Vielleicht befindet sie sich aber
        auch noch im Karton im Keller ...

        Weitere Erläuterung zu ultraForth und den verwendeten
        Dateiformaten am Ende dieser Datei.

ULTRA4TH.ZIP ======= Archivdatei zum Download
ULTRA4TH.ZIP ======= Archivdatei zum Download
ULTRA4TH.ZIP ======= Archivdatei zum Download

        Im folgenden wird der Inhalt der Archivdatei beschrieben


UF83_380\ =================================================================================

UF83_380\UF38-1.        Image für C16/64: Disk1 Lieferumfang ultraForth 3.8 C16/C64
UF83_380\UF38-2.        Image für C16/64: Disk2 Lieferumfang ultraForth 3.8 C16/C64
UF83_380\UF38-3.        Image für C16/64: Disk3 Lieferumfang ultraForth 3.8 C16/C64
UF83_380\UF38-4.        Image für C16/64: Disk4 Lieferumfang ultraForth 3.8 C16/C64

                ultraForth ist das volksForth für C16, C64 und Plus4

UF83_380\TC38.           Image für C16/C64: Targetcompiler für das ultraForth 3.8 C16/C64

UF83TAPE\ =================================================================================

        Dieses Verzeichnis enthält einige Dokumentationen
        zum ultraForth, die so oder in überarbeiteter Form
        zum Lieferumfang hinzugefügt oder zur Werbung genutzt wurden.




UF83TAPE\UFTAPE.TXT         7.217  24.11.87  17:29 UFTAPE.TXT

        Zählerstände und Tonkopfeinstellung für die Kassettenversion.
        Bestandteil des Lieferumfangs.

UF83TAPE\VF83T.TXT         4.431  01.12.87  17:14 VF83T.TXT

        Kundeninformation 'Notlösungen zur Kassettenversion'

UF83TAPE\UF361.DOC        62.456  13.08.87  19:33 UF361.DOC

        Die ersten Schritte mit 'ultraForth'. Wurde in die
        neue Handbuchauflage zu ultraForth 3.8 eingearbeitet.

        Aus dem Inhalt:

        - Arbeit mit Forth-Disketten-Formaten und-Blockmechanismus, Back-Up
        - Definieren einzelner Worte
        - Systemabstürze verschiedener Art
        - Hexadezimal und Dezimal
        - Editor
        - Laden von Quellscreens
        - wichtige Informationen aus dem System holen
        - Speicherbelegung
        - Erstellen eines ultraFORTH Arbeitssystems


UF83TAPE\RAMDISK.DOC         3.256  11.05.87  16:03 RAMDISK.DOC

        Konzept der Kassettenversion. Eine Ramdisk.


PRESSE\   =================================================================================

                Einige öffentliche Erklärungen und Briefe an die Presse

PRESSE\CBMWELT1 BRF         1.447  26.10.87  18:04
PRESSE\EINLADUN             1.573  24.04.87  12:05
PRESSE\HCC1     BRF         2.456  19.11.87  13:48
PRESSE\M&T1     BRF         1.796  19.11.87  14:42
PRESSE\NIGGLES1 BRF         8.641  26.10.87  18:10
PRESSE\PC-PRES  BRF         2.549  09.04.90  15:20
PRESSE\PRESSE   KON         8.894  08.01.88  15:42
PRESSE\RECHBP   BRF         1.475  26.10.87  17:25
PRESSE\UF36B    BRF         2.373  21.09.87  14:02
PRESSE\UF38     BRF         3.228  26.10.87  18:04
PRESSE\ZANDE1   BRF         1.737  05.02.88  15:23


UF_TO_PC\ =================================================================================

        Dieses Verzeichnis enthält Utilities, um Forth-Quellen für
        Commodore auf Dos-PCs zu nutzen.

UF_TO_PC\CBM.ART

        Datenübertragung von Commodore-Plus4 auf den PC        clv03jul89
        Dieser Artikel erschien im Forth Magazin Vierte Dimension 2/1990
        Er enthält eine Bastelanleitung für die Hardware
        und die nötige Software für die serielle Datenübertragung vom C16 auf PC


UF_TO_PC\CBM      COM        38.628  28.06.89  19:56 CBM.COM
UF_TO_PC\CBM      SCR         6.144  04.07.89  14:27 CBM.SCR

        Ausführbares Forth-System und Quelle für PC zur Nutzung
        von Commodore-Quellen unter volksForth 3.8 für PC

        Beispiel:

        < CBM  use UF38-1  1 list bye >

        Listet den ersten Screen der Quelldiskette des ultraForth-Systems


UF_TO_PC\EDITORCB.SCR  38.912  28.06.89  19:47 EDITORCB.SCR
UF_TO_PC\UTI.SCR       35.840  28.06.89  19:05 UTI.SCR
UF_TO_PC\INDEX.SCR      2.048  28.06.89  17:42 INDEX.SCR

UF_TO_PC\LIST-CBM.BAT            45  28.06.89  16:19 LIST-CBM.BAT

        Dos-Batchfile zum Erstellen von Listings.

        < LIST-CBM CBM.SCR >

        gibt ein Listing von UTI.SCR auf den Drucker aus.
        Wildcards sind erlaubt.

        Sicherer ist es, erst mit einem kleinen File zu testen.


        < CD EXE; LIST-CBM ..\ERRHANDL\ERR.SCR >

        ergibt etwa sieben Druckseiten, wenn alles funktioniert.

        (Soeben gestet mit einem Pentium-III-500 mit HP Laserjet 5MP:
         Das Layout ist ziemlich murksig, aber man kann die Screens deutlich lesen.)


UF_TO_PC\EPSONCBM.PRN        12.288  28.06.89  15:44 EPSONCBM.PRN
UF_TO_PC\GRAPHIC.PRN         12.288  23.06.89  14:16 GRAPHIC.PRN

        Treiber für IBM Graphic Printer

UF_TO_PC\CBM_ST.SCR

        Benutzung von Commodore-Screens mit Atari       clv05jan90
        Das Commodore-Screen-Format von 25 Zeilen a 41 Buchstaben
        kann so unter Atari ST, volksFORTH 3.80 verwendet werden.

UF_TO_PC\FG0190.ART

        Kurzartikel zu CBM_ST.SCR


ERRHANDL\ =========================================================
ERRHANDL\ERR      ART        16.946  04.07.89  19:37    Artikel
ERRHANDL\ERR      SCR        18.432  04.07.89  19:14    ultraForth-Quelle
ERRHANDL\FEHLERBH KON         7.102  01.03.88  19:40    Konzept

        Dieses Verzeichnis enthält einen (unveröffentlichten)
        Artikel zum Thema 'Fehlerbehandlung in Forth'
        incl. Quellen.

        Es war noch ein kleines Fehlerchen drin und als ich
        mich das nächste Mal drangesetzt habe, war es schon auf PC.


CLV\ ====================================================================



CLV\DSK-GRF.487     Image für C16/C64. Unvollendet, ca. 1987

                Tests für die Basis eines Grafikpakets für ultraForth C16.
                Das Bankswitching, das etwas anders als auf dem C64
                organisiert war, machte da Ärger.


CLV\EDIT.           Image für C16/C64. Unvollendet, ca. 1987

                Ein selbstgebauter Editor ultraForth 3.8 C16/C64.
                Er sollte Dateien bearbeiten können. Das konnte
                er auch, war aber unerträglich langsam und
                der Cursor war meistens weg ...

                Dann begann auch bei mir zuhause die PC-Ära ...


CLV\EH-TERM.387     Image für C16/C64

                Scr 5 .. 11 : Fehlerbehandlung 1.Entwurf
                Scr12 .. 19 : Fehlerbehandlung 2.Entwurf
                Scr20 .. 27 : Fehlerbehandlung
                Scr33 .. 37 : Fehlerbehandlung mit tabort" Entwurf
                Scr45 .. 59 : backup ... r/w
                Scr60 .. 74 : Terminalprogramm für serielle Schnittschelle

CLV\MENUE.          Image. Test für ein Menüsystem ultraForth 3.8. Unvollendet, clv1989


====================================================================



======= UltraForth83 3.8 erschien 1987

        UltraForth83 ist das volksForth für die kleinen Commodore-Rechner
        C16, Plus4 und C64.

        UltraForth83 bestand aus ca. 200 Seiten Handbuch in
        deutscher Sprache und vier Diskettenseiten
        - ausgeliefert auf zwei Disketten, die
        mittels Diskettenlocher beidseitig bespielbar wurden.

        UltraForth83 war sicher das beste Forth-System für den C64.
        Für den C16/Plus4 war es die einzige enstzunehmende Programmiersprache
        Überhaupt. Sonst existierten dort Basic und Assembler.

        UltraForth83 ist eine komplette Programmierumgebung
        Es enthält einen Full-Screen-Editor, einen quelltextnahe Debugger,
        den Compiler/Interpreter und einen Assembler.


        UltraForth83 entspricht dem Forth83-Standard und existiert
        in ähnlichen Versionen auf Atari ST, C16/64/+4, CP/M und PC

        Der Name ultraForth wurde gewählt, da auf dem
        legendären C64 kein Programm geklaut
        wurde, das nicht mindestens Super, Turbo oder eben Ultra hieß.

        Der Transport vom Netz zum Commodore und zurück ist nicht
        ganz problemlos, daher hier ein paar Anmerkungen zum Format:

        - Bei den Images handelt es sich um Sektorkopien kompletter
          Forth-Disketten im Commodore-Format. Sie umfassen je 174 080 Bytes.

        - Bei den SCR-Dateien handelt es sich um Forth-Quellen im Blockformat.
          volksForth benutzte keine 'normalen' Dateien zur Speicherung von Quellen,
          sondern Blöcke (Screens) von je 1024 bytes organisiert als
          16 Zeilen mit je 64 Zeichen oder für Commodore als
          25 Zeilen mit je 41 Zeichen (letzte Zeile nur 40 Zeichen).

        - Bei COM-Dateien handelt es sich um ausführbare Dateien für DOS-PCs.

        Transport Richtung Commodore-Rechner ist praktisch nur über (selbstgebastelte) serielle Verbindungen
        möglich. Oft wird es sinnvoller sein, einen Commodore auf dem PC zu emulieren.


======= Zur Erläuterung der Blöcke/Screens :

        ultraForth/volksForth verwendete wie viele damalige Forth-Systeme statt 'normalen' Dateien Blöcke.
        Dabei handelt es sich um Disketten/Plattenbereiche fester Größe, im Forth-83-Standard
        von jeweils 1024 bytes. Diese wurden ohne Betriebssystem-Unterstützung auf die Platte geschrieben.
        Nebenbei werden Forth-Compiler auch heute noch oft als Forth-'Systeme' bezeichnet, da sie ursprünglich
        ohne Betriebssystem liefen und daher leicht auf neue Hardware portiert werden konnten.

        Die Forth-Quellen wurden in solchen Blöcken gespeichert. Sie hießen auch Screens,
        da sie mit 16 Zeilen zu je 64 Zeichen einen Bildschirm (fast) ausfüllen.

        Beim Commodore mit seinem 24x40-Bildschirm wurde ein anderes Format gewählt.
        Hier wurden 23 Zeilen zu 41 Zeichen und eine letzte Zeile zu 40 Zeichen benutzt.
        Das füllte den Bildschirm genau aus und bot noch eine (unsichtbare) Spalte mit
        Leerzeichen.

        Der Plattenzugriff in Blöcken ist leichter zu implementieren.
        Auf Commodore bot er zusätzlich extreme Geschwindigkeitsvorteile.

        Das Schreiben in Block-Manier führte positiv zu kurzen Quelltext-Abschnitten und Übersichtlicher Organisation der
        Quellen. Zum Problem wurde es, wenn man doch noch ein paar Zeilen einfügen wollte.
        Um einen Block einzuschieben, mußte man alle anderen Blöcke um eins verschieben.
        Dies geschah z.B. mit '10 50 11 CONVEY'. Ein Tippfehler und die Quellen waren weg.
        Die Erinnerung an Convey dürfte alte Forthler immer noch erbleichen lassen.
        Damit wurden wohl mehr Quellen vernichtet, als mit dem damaligen Dos-DISKCOPY.
        Letzteres erforderte ungefähr 6 Diskettenwechsel. Jede Verwechslung
        der Disketten führte zwangslÃ¤ufig zu Datenverlust.
        'Legen Sie die Quelle ins Ziel' war eine gängige Verballhornung der Diskcopy-Meldungen.
