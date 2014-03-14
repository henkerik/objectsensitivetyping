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
    $b = inverse(0);
} catch (Exception $e) {
    $c = $e;
    //$message = $e->getMessage();
}

// Test if flow continues
$after = $before;

?>