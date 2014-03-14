<?php

require_once 'mvc/user/User.php';

class SecurityUser extends User {

    public function isAuthenticated ()
    {
        return $this->getAttribute ('is_authenticated', 'authentication');
    }

    public function login ()
    {
        $this->setAttribute ('is_authenticated', true, 'authentication');
    }
    
    public function logout ()
    {
        $this->setAttribute ('is_authenticated', false, 'authentication');
    }
}

?>