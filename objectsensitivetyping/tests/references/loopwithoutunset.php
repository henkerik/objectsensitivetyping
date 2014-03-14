<?php

$i = 0;
while ($i < 3) {
    $a = $i;
    if ($i < 2) 
        $r =& $a;
    
    $i++;
}

$b =& $a;
$b = true;

$x = $a;
$y = $b;
$z = $r;

?>