<?php

$path = "/aa/bb/cc.txt";

preg_match('|^(?P<prefix>([a-zA-Z]:)?/)|', $path, $matches);

$x = $matches["prefix"];

?>