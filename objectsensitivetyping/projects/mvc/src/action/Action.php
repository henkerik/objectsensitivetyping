<?php

/**
 *
 * The Action class encapsulates a request as an object.
 * Form a MVC point of view, Action classes are part of the
 * Controller.
 *
 * See GOF Command design pattern for more information.
 *
 *
 * @author Henk Erik van der Hoek (mail@henkerikvanderhoek.nl)
 * @author Jorgen Horstink (mail@jorgenhorstink.nl)
 *
 * @version 1.0
 * @package mvc
 */


class Action
{
    // +-----------------------------------------------------------------------+
    // | PRIVATE VARIABLES                                                     |
    // +-----------------------------------------------------------------------+

    protected $context = null;

    protected $moduleName;
    
    protected $actionName;
    
    protected $presentation;

    public function __construct ($moduleName, $actionName)
    {
		$this->moduleName = $moduleName;
		$this->actionName = $actionName;
    }
    
    public function getModuleName ()
    {
		return $this->moduleName;
    }
    
    public function getActionName ()
    {
		return $this->actionName;
    }
    
    public function setPresentation ($presentation)
    {
		$this->presentation = $presentation;
    }
    
    public function getPresentation ()
    {
		return $this->presentation;
    }
    
    // +-----------------------------------------------------------------------+
    // | METHODS                                                               |
    // +-----------------------------------------------------------------------+

    public function initialize($context)
    {
        $this->context = $context;
    }

    /**
     * Enter description here...
     *
     * @return Context
     */
    // TODO: waarom protected? public kan handig zijn...
    public function getContext()
    {
        return $this->context;
    }


    // -------------------------------------------------------------------------


    public function execute (Request $request)
    {
        return View::SUCCESS;
    }

    public function handleError (Request $request)
    {
        return View::ERROR;
    }

    public function defaultView (Request $request)
    {
        return View::SUCCESS;
    }


    // -------------------------------------------------------------------------


    public function validate (Request $request)
    {
        return true;
    }

    public function isSecure ()
    {
        return false;
    }
    
    public function hasAccess (Credential $credential, Request $request)
    {
        if ($credential->getDomain()->isAll() || $credential->getDomain()->isOwn()) {
            return true;   
        } else {
            return false;
        }
    }

    public function getRequestMethod ()
    {
        return WebRequest::GET;
    }

    public function forward(Request $request, Message $message) {
        $controller = $this->getContext()->getController();

        if($message->getAttributeName() != 'empty') {
            $request->setAttribute($message->getAttributeName(), $message->getMessage());
        }
        $presentation = $controller->execute ($request);
        $this->setPresentation ($presentation);
            
        return View::NONE;
    }
    
    public function redirect(Url $url, Message $message) {
        $user = $this->getContext()->getUser();
        
        if($message->getAttributeName() == 'message') {
            $messages = new ArrayMap();
            $messages->put($url->toString(), $message->getMessage());
            $user->setAttribute('messages', $messages, 'framework');
        } else if($message->getAttributeName() == 'error') {
            $errors = new ArrayMap();
            $errors->put($url->toString(), $message->getMessage());
            $user->setAttribute('errors', $errors, 'framework');
        } else if($message->getAttributeName() == 'successMessage') {
            $successMessages = new ArrayMap();
            $successMessages->put($url->toString(), $message->getMessage());
            $user->setAttribute('successMessages', $successMessages, 'framework');
        }
        
        header('Location: ' . $url->toString());
    
        return View::NONE;
    }
}

?>