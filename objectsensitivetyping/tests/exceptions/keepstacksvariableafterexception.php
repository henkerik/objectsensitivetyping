<?php

function inverse($x) {
    if ($x == 0) {
        throw new Exception('Division by zero.');
    } else {
        return 1/$x;
    }
}

$before = 42;

try {
    $a = inverse(5);
    $d = 12;
    $b = inverse(0);
} catch (Exception $e) {
    $c = $e;
    $x = $before;
    $y = $d;
    //$message = $e->getMessage();
}

$y2 = $d;
//echo "X: " . $foo . "\n";

// Test if flow continues
$after = $before;

?>