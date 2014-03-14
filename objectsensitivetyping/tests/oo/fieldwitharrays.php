<?php

class DMS
{
    protected $units = array(
        'UTF-8' => array(
            'deg' => '°',
            'min' => '′',
            'sec' => '″',
        ),
        'ASCII' => array(
            'deg' => '°',
            'min' => '\'',
            'sec' => '"',
        ),
    );
    
    public function __construct ()
    {
        
    }
}

$x = new DMS();

?>