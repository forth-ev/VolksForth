\ Atari Sound Commands

 ( $D200 = Pokey AUDBASE )

 : SOUND ( CH# FREQ DIST VOL -- )
   SWAP $10 * + ROT DUP + $D200 +
   ROT OVER C! 1+ C! ;


