<?php

/**
 *
 * The View class is responsible for the presentation of the models. Models and
 * other data should be given to the attributes of the view. These attributes
 * will be accessible in the templates. Don't use the view class directly.
 * Use a subclass like PhpView or GdView instead.
 *
 *
 *
 * @author Henk Erik van der Hoek (mail@henkerikvanderhoek.nl)
 * @author Jorgen Horstink (mail@jorgenhorstink.nl)
 *
 * @version 1.0
 * @package mvc
 * @subpackage view
 */


abstract class View {

    // +-----------------------------------------------------------------------+
    // | CONSTANTS  	                                                       |
    // +-----------------------------------------------------------------------+

    const NONE = 'None';
    
    const SUCCESS = 'Success';
    
    const ERROR = 'Error';
    
    const INPUT = 'Input';


    // +-----------------------------------------------------------------------+
    // | PRIVATE VARIABLES                                                     |
    // +-----------------------------------------------------------------------+

    protected $context = null;
       
    protected $decorator;
    
    protected $moduleName;
    
    protected $actionName;
    
    protected $viewName;


    public function __construct ($moduleName, $actionName, $viewName)
    {
        $this->moduleName = $moduleName;
        $this->actionName = $actionName;
        $this->viewName = $viewName;
    }

    public function getModuleName ()
    {
        return $this->moduleName;
    }

    public function getActionName ()
    {
        return $this->actionName;
    }
    
    public function getViewName ()
    {
        return $this->viewName;
    }
    
    // +-----------------------------------------------------------------------+
    // | METHODS                                                               |
    // +-----------------------------------------------------------------------+

    /**
     * Returns a reference to the context
     * 
     * @return Context
     */
    protected function getContext()
    {
        return $this->context;
    }

    public function initialize(Context $context)
    {
        $this->context = $context;
    }


    // -------------------------------------------------------------------------

    // Override
    public abstract function render(Request $request);

    // Override
    public abstract function execute (Request $request);

    // -------------------------------------------------------------------------
   
    public function setDecorator ($decorator)
    {
        $this->decorator = $decorator;        
    }
    
    public function isDecorated ()
    {
        return isset ($this->decorator);
    }
}

?>