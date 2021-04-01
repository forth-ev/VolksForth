
create log-dev  8 ,
create log-2nd  2 ,
: log-dev-2nd@ log-dev @ log-2nd @ ;

: log-emit
  dup c64emit log-dev-2nd@ busout bus! busoff ;

: log-cr
  c64cr log-dev-2nd@ busout #cr bus! busoff ;

: log-type
  2dup c64type log-dev-2nd@ busout bustype busoff ;

Output: alsologtofile
 log-emit log-cr log-type c64del c64page
 c64at c64at? ;

: logopen"
  ascii " parse  2dup type
  log-dev-2nd@ busopen
  bustype " ,s,w" count bustype busoff
  i/o-status? IF c64cr log-dev @ dos-error abort THEN
  alsologtofile ;

: logclose
  log-dev-2nd@ busclose  display ;
