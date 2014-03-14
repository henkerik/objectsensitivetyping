<?php
# Simple array

$arr = array("foo" => "bar", 12 => true);

$a = $arr["foo"]; // bar
$b = $arr[12];    // 1

# Array whose value is another array
$arrz = array("somearray" => array(6 => 5, 13 => 9, "a" => 42));

$x = $arrz["somearray"][6];    // 5
$y = $arrz["somearray"][13];   // 9
$z = $arrz["somearray"]["a"];  // 42

?>