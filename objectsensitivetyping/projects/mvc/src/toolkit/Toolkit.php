<?php

class Toolkit {

    // Static function
    public static function stripslashes ($mixed)
    {
        if (is_array ($mixed)) {
            $result = array ();

            foreach ($mixed as $name => $value) {
                $result[$name] = Toolkit::stripslashes ($value);
            }

            return $result;
        } else {
            return stripslashes($mixed);
        }
    }

    public static function lowerCamelCaseToUnderscore($string) {
        $i = 0;
        $l = strlen($string);
        $ret = "";
        for (; $i < $l; $i++) {
          if ($i != 0 && $string{$i} != strtolower($string{$i})) { // it's uppercase
            $ret .= "_" . strtolower($string{$i});
          } else {
            $ret .= strtolower($string{$i}); 
          }   
        }
        return $ret;
    }

    public static function upperUnderscoreToCamelCase($string) {
        $i = 0;
        $l = strlen($string);
        $ret = "";
        for (; $i < $l; $i++) {
          if ($string{$i} == "_") {
            $ret .= strtoupper($string{$i+1});
            $i++;
          } else {
            $ret .= $string{$i}; 
          }   
        }
        return $ret;
    }
}

?>