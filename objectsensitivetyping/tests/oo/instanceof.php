<?php

class X {}
class Y {}
class Z extends X {}

$z = new Z();

$a = $z instanceof X;
$b = $z instanceof Y;
$c = $z instanceof Z;

$d = $a;

?>