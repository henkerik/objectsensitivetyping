<?php
/* example 1 */

for ($i = 1; $i <= 10; $i++) {
    $a = 1;
}

/* example 2 */

for ($i = 1; ; $i++) {
    if ($i > 10) {
        break;
    }
}

/* example 3 */
$i = 1;
for (;;) {
    if ($i > 10) {
        break;
    }
    $i++;
}

/* example 4 */

//for ($i = 1, $j = 0; $i <= 10; $j += $i, print $i, $i++);

?>