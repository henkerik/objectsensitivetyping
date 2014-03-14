<?php


class CompositeMimeEntity {

    public function getBoundary ()
    {
        return "XX";
    }

    public function build ()
    {
        $headers = "";
        
        $headers .= '--' . $this->getBoundary () . '--' . '\n';
        $headers .= '--' . $this->getBoundary () . '--' . '\n';
        
        return $headers;
    }
}

$root = new CompositeMimeEntity ();
$headers = $root->build();

?>