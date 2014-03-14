<?php

/**
 *
 * The Context class is a registry class. It's a well-known object that other
 * objects can use to find common objects and services. The Context
 * object gives access to the following objects:
 *
 * - An implementation of the User class
 * - An implementation of the Controller class
 * - An implementation of the Storage class
 *
 *
 *
 * @author Henk Erik van der Hoek (mail@henkerikvanderhoek.nl)
 * @author Jorgen Horstink (mail@jorgenhorstink.nl)
 *
 * @version 1.0
 * @package mvc
 */



class Context {

    // +-----------------------------------------------------------------------+
    // | PRIVATE VARIABLES                                                     |
    // +-----------------------------------------------------------------------+

    protected $controller;
    protected $user;
    protected $storage;



    // +-----------------------------------------------------------------------+
    // | METHODS                                                               |
    // +-----------------------------------------------------------------------+

    function Context($controller, $user, $storage) 
	{
        $this->controller	= $controller;
        $this->user			= $user;
        $this->storage 		= $storage;
        
    	$controller->initialize ($this);
    	$user->initialize ($this);
    	$storage->initialize ($this);
    }

    /**
     * Returns a reference to the controller
     *
     * @return Controller
     */
    function getController()
    {
        return $this->controller;
    }

    
    /**
     * Returns a reference to the user
     *
     * @return User
     */
    function getUser()
    {
        return $this->user;
    }

    /**
     * Returns a refenence to the storage. Only the User class should use this method. 
     * Use the User object to store information in a session.
     *
     * @return Storage
     */
    function getStorage ()
    {
        return $this->storage;
    }
}

?>