<?php

require_once 'security/SecurityContext.php';

class WebappSecurityContext implements SecurityContext {
    
    protected $user;
    
    public function __construct(User $user)
    {
        $this->user = $user;
    }
    
    public function getUser ()
    {
        return $this->user;
    }
}


?>