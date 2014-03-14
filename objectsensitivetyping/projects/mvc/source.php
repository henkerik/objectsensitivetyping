<?php


interface AbstractList {
    
    public function add ($item);
    
    public function addList ($list);
    
    public function contains ($item);
        
    public function isEmpty ();
    
    public function remove ($remove);
    
    public function length ();
    
    public function index ($item);
}

class ArrayList implements AbstractList {
    
    protected $list;
    
    public function __construct ()
    {
        $this->list = array ();
    }
    
    public function add ($item)
    {
        $this->list[$this->length()] = $item;
    }
    
    
    
    public function get ($index)
    {
        return $this->list[$index];
    }
    
    public function addList ($list)
    {
        for ($i = 0; $i < $list->length(); $i++) 
            $this->add($list->get($i));
    }
    
    public function contains ($item)
    {    
        for ($i = 0; $i < $list->length(); $i++) 
            if ($item === $list->get ($i))
                return true;
                
        return false;
    }
    
    public function remove ($remove)
    {
        $new = array();
        
        for ($i = 0; $i < $this->length(); $i++) 
            if ($remove != $this->get($i))
                $new[sizeof($new)] = $this->get($i);
               
        $this->list = $new;
    }
    
    
    public function isEmpty ()
    {
        return sizeof ($this->list) == null;   
    }
    
    public function join ($separator)
    {
        return join($separator, $this->list);
    }
    
    public function index ($item) 
    {
        for ($i = 0; $i < $this->length (); $i++)
            if ($this->list[$i] === $item)
                return $i;
    }
    
    public function length ()
    {
        return sizeof ($this->list);
    }
}

interface Map extends IteratorAggregate {
    
    public function put ($key, $value);
    
    public function get ($key);
    
    public function containsKey ($key);
    
    public function isEmpty ();
    
    public function length ();

    public function keySet();
}

class ArrayMap implements Map {
    
    protected $map = array ();
    
    public function keySet()
    {
        return array_keys($this->map);
    }
    
    public function put ($key, $value)
    {
        $this->map[$key] = $value;      
    }
    
    public function get ($key)
    {
        if ($this->containsKey ($key))
            return $this->map[$key];

        return null;
    }
    
    public function containsKey ($key)
    {
        return isset ($this->map[$key]);
    }
    
    /**
     * @return Iterator
     */
    public function getIterator ()
    {
        return new ArrayObject($this->map);
    }
    
    public function isEmpty ()
    {
        return $this->length() == 0;
    }
    
    public function join ($separator)
    {
        return join($separator, $this->map);
    }
    
    public function length()
    {
        return sizeof($this->map);   
    }
}

abstract class Validator {

    protected $value;
    
    protected $message;
    
    /**
     * @var Map
     */
    protected $properties;

    public function __construct ($value, $message)
    {
        $this->value = $value;
        $this->message = $message;
    }

    public function setProperty ($name, $value)
    {
        $this->properties->put($name, $value);
    }

    protected function getProperty ($name)
    {
        return $this->properties->get($name);
    }

    public function getMessage ()
    {
        return $this->message;
    }
        
    public abstract function execute ();
}

class RequiredValidator extends Validator {
    
    public function execute ()
    {              
        if (is_array($this->value)) {
            return true;
        } elseif (strlen ($this->value) > 0) {
            return true;
        } elseif (!empty ($this->value)) {
            return true;
        } else {
            return false;
        }
    }
}

class EmailValidator extends Validator 
{
    public function execute ()
    {   
        return preg_match ('/^([.0-9a-z_-]+)@(([0-9a-z-]+\.)+[0-9a-z]{2,4})$/i', $this->value);
    }
}

class ValidatorManager {
    
    /**
     * @var Map
     */
    protected $validators;
    
    /**
     * @var Map
     */
    protected $errorMessages;
    
    public function __construct (Map $errorMessages)
    {
        $this->errorMessages = $errorMessages;
        $this->validators = new ArrayMap ();
    }
    
    public function register ($fieldname, Validator $validator)
    {
        $this->validators->put($fieldname, $validator);
    }

    public function execute ()
    {
        $result = true;
        
        $keySet = $this->validators->keySet();
        for ($i = 0; $i < sizeof($keySet); $i++) {
            $fieldname = $keySet[$i];
            $validator = $this->validators->get($fieldname);
            if (!$validator->execute ()) {
                $this->errorMessages->put ($fieldname, $validator->getMessage());
                $result = false;
            }
        }
        
        return $result;
    }   

}


interface Database {
    
    /**
     * @return Database
     *
     
    public static function getInstance ();
    */
    
    /**
     * 
     * @throws DatabaseException
     */
    public function connect ($hostname, $username, $password, $database);

    /**
     * 
     * @throws DatabaseException
     */
    public function disconnect ();
    
    
    
    /**
     * 
     *
     * @param string $sql
     * @return Resultset
     * @throws DatabaseException
     */
    public function query ($sql);
    
    public function lastInsertId ();
    
    public function escapeString ($string);
    
    public function startTransaction ();
    
    public function commit ();
    
    public function rollback ();
    
    public function transactionStarted ();
}

class MysqlDatabase implements Database
{
    protected $link;

    public function connect($hostname, $username, $password, $database) 
    {       
        if (!$this->link = mysql_connect ($hostname, $username, $password, true))
            throw new DatabaseException ('Error while connect to the database server');
        
        if (!mysql_select_db ($database, $this->link))
            throw new DatabaseException ('Error selecting the database: ' . mysql_error());
    }
    
    public function disconnect ()
    {
        if (!mysql_close ($this->link)) 
            throw new DatabaseException ('Error closing the database: ' . mysql_error());
    }


    
    /**
     * @return MysqlResultset
     */
    public function query ($sql) 
    {    
        //echo $sql . "<br>";
        if (!$query = mysql_query ($sql, $this->link))
            throw new DatabaseException ('MySQL says: ' . mysql_error () . '. The following SQL was given to the database server: ' . $sql, E_USER_ERROR);
    
        return new MysqlResultset($query);
    }
    
    
    public function lastInsertId ()
    {
        return mysql_insert_id ($this->link);
    }
    
    public function escapeString ($string)
    {
        return mysql_real_escape_string ($string, $this->link);    
    }
    
    protected static $instance;
    
    protected $transactionStarted = false;
    // Zorg ervoor dat de commit die bij de echte start transaction hoort uitgevoerd wordt...
    // Alle andere commit aanroepen worden genegeerd.
    protected $transactionCount   = 0;
    
    public function startTransaction()
    {
        $this->transactionCount++;

        if ($this->transactionCount == 1)
            $this->query("START TRANSACTION");
    }
    
    public function commit ()
    {
        $this->transactionCount--;
        
        if ($this->transactionCount == 0) {
            $this->query("COMMIT");
        }
    }
    
    public function rollback ()
    {
        $this->query("ROLLBACK");
        $this->transactionCount--;
    }
    
    public function transactionStarted ()
    {
        return $this->transactionStarted;
    }

}

interface RowMapper {
    
    public function mapRow ($row);
    
}



class ConstraintViolatedException extends DatabaseException {}


class DatabasePool
{   
    protected static $instance = null;
    
    protected $instancePool;
    
    protected function __construct () 
    {
        $this->instancePool = new ArrayMap ();
    }
    
    /**
     * Singleton
     *
     * @return DatabasePool
     */
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new DatabasePool ();
        }
        return self::$instance;
    }
    
    public function register (Database $engine = null, $connection = 'default')
    {
        // the default database engine if the engine is not provided
        if ($engine == null) {
            $engine = new MysqlDatabase();   
        }
        
        if ($this->instancePool->get($connection) == null) {
            $this->instancePool->put($connection, $engine);
        } else {
            throw new DatabaseException ('A connection with the name \'' . $connection . '\' has already been registered.');   
        }
    }
    
    /**
     * @return Database
     */
    public function getDatabase ($connection = 'default')
    {
        
        if ($this->instancePool->get($connection) == null) {
            throw new DatabaseException ('There is not registered any database connection with the name \'' . $connection . '\' yet.');
        }
        
        return $this->instancePool->get($connection);
    }
}


class DatabaseException extends Exception {
    
    
}


class ResultsetIterator implements Iterator
{
    protected $key;
    protected $current;
    protected $valid;
    
    protected $resultset;

    public function __construct ($resultset) 
    {       
        $this->resultset = $resultset;
    }
    
    public function current ()
    {    
        return $this->current;
    }
    
    public function key ()
    {
        return $this->key;
    }
    
    public function next ()
    {    
        $this->current = $this->resultset->next ();
        $this->valid = $this->current;
        $this->key++;
    }
    
    public function rewind ()
    {
        $this->current = $this->resultset->first ();
        $this->valid = $this->current;
        $this->key = 0;
    }
    
    public function valid ()
    {
        return $this->valid;
    }
    
    public function hasNext ()
    {
        return $this->key + 1 < $this->resultset->length ();  
    }
    
}

interface Resultset extends IteratorAggregate {

    public function length();

    public function first ();
    
    public function next ();
}


class MysqlResultset implements Resultset
{
    protected $resource;

    public function __construct ($resource)
    {
        $this->resource = $resource;    
    }

    /**
     *  
     *
     * @return ResultsetIterator
     */
    public function getIterator ()
    {       
        return new ResultsetIterator($this);
    }

    public function length ()
    {
        return mysql_num_rows ($this->resource);
    }
    
    public function first ()
    {
        if ($this->length () > 0) {
            mysql_data_seek($this->resource, 0);
            return $this->next();
        }
        
        return null;
    }
    
    public function reset ()
    {
        if ($this->length () > 0) {
            mysql_data_seek($this->resource, 0);
        }
    }
    
    public function next ()
    {
        return mysql_fetch_assoc ($this->resource);
    }
}



interface Constraint {

    /**
     * @return boolean
     */
    public function verify (Database $database);
    
    
    public function getDescription ();
}



class DebugDatabase implements Database {
    
    /**
     * @var Database
     */
    protected $inner;
    
    protected $count;
    
    protected $time;
    
    protected $constraints;
    
    protected $violatedConstraint;
    
    protected $lastInsertId;

    public function __construct (Database $inner)
    {
        $this->inner = $inner;
        $this->count = 0;
        $this->time = 0;
        
        $this->constraints = new ArrayList ();
    }
    
    public function addConstraint (Constraint $constraint)
    {
        $this->constraints->add ($constraint);
    }
    
    protected function constraintsViolated ()
    {
        for ($i = 0; $i < $this->constraint->length(); $i++) {
            $constraint = $this->constraints->get($i);
            if (!$constraint->verify ($this->inner)) {
                $this->violatedConstraint = $constraint;     
                
                return true;
            }
        }
        
        return false;
    }
    
    public function connect ($hostname, $username, $password, $database)
    {
        $this->inner->connect ($hostname, $username, $password, $database);
    }

    /**
     * 
     * @throws DatabaseException
     */
    public function disconnect ()
    {
        $this->inner->disconnect ();
    }
    
    public function reconnect ()
    {
        $this->inner->reconnect ();
    }
    
    /**
     * 
     *
     * @param string $sql
     * @return Resultset
     * @throws DatabaseException
     */
    public function query ($sql)
    {
        $transactionStartedForVerification = false;
        
        // Start a transaction, if one is not already a started. This enables us 
        // to rollback this query if it violates one of our constrains
        if (!$this->inner->transactionStarted ()) {
            $this->inner->startTransaction();
            $transactionStartedForVerification = true;
        }
        
        // Keep track of the statistics
        $this->count++;
        
        $timeStart = microtime(true);
        
        // Execute the original query        
        $resultset = $this->inner->query ($sql);
        
        // Keep track of the statistics
        $this->time += microtime(true) - $timeStart;
        
        // Keep track of the last insert id, this value is unavailable after a commit or rollback
        $this->lastInsertId = $this->inner->lastInsertId();
        
        // Look if we started a transaction 
        if ($transactionStartedForVerification) {
            

            if ($this->constraintsViolated ()) {
                
                // One of our constraints was violated. Rollback our query
                $this->inner->rollback ();
                
                throw new ConstraintViolatedException ($this->violatedConstraint->getDescription());
                  
            } else {
                
                // The query didn't violated any of our constraints
                $this->inner->commit ();
                
            }
        }
        
        return $resultset;
    }
    
    public function getQueryCount ()
    {
        return $this->count;
    }
    
    public function getQueryExecutionTime ()
    {
        return $this->time;
    }
    
    public function lastInsertId ()
    {
        return $this->lastInsertId;
    }

    public function escapeString ($string)
    {
        return $this->inner->escapeString ($string);
    }
    
    /**
     * @throws DatabaseException
     */
    public function startTransaction ()
    {
        $this->inner->startTransaction ();  
    }
    
    public function commit ()
    {
        if ($this->constraintsViolated ()) {
                
            // One of our constraints was violated. Rollback our transaction
            $this->inner->rollback ();
            
            throw new ConstraintViolatedException ($this->violatedConstraint->getDescription ());
              
        } else {
            
            // The transaction didn't violated any of our constraints
            $this->inner->commit ();
            
        }
    }
    
    public function rollback ()
    {
        $this->inner->rollback ();
    }
    
    public function transactionStarted ()
    {
        return $this->inner->transactionStarted ();
    }
}

// Decorator pattern

class MappingResultset implements Resultset {
    
    protected $resultset;
    
    protected $rowMapper;
    
    public function __construct (Resultset $resultset, RowMapper $rowMapper)
    {
        $this->resultset = $resultset;
        $this->rowMapper = $rowMapper;
    }
    
    public function length()
    {
        return $this->resultset->length ();
    }

    public function first ()
    {
        $row = $this->resultset->first ();;
        return $this->mapRow ($row);
    }
    
    public function next ()
    {
        $row = $this->resultset->next ();
        return $this->mapRow ($row);
    }
    
    /**
     * @return ResultsetIterator
     */
    public function getIterator ()
    {       
        return new ResultsetIterator($this);
    }
    
    protected function mapRow ($row) 
    {        
        if ($row) {
            return $this->rowMapper->mapRow($row);
        } else {
            return null;
        }
    }
}


class Toolkit {

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


abstract class Storage {
    
    protected $context 	= null;
    
    protected $data = array();
    

    public function initialize (Context $context)
    {
        $this->context = $context;
    }

    /**
     * Returns a reference to the context
     *
     * @return Context
     */
    protected function getContext ()
    {
        return $this->context;
    }

    public abstract function getAttribute ($name, $ns = null);

    public abstract function setAttribute ($name, $value, $ns = null);

    public abstract function hasAttribute ($name, $ns = null);
    
    public abstract function removeAttribute ($name, $ns = null);
}

class SessionStorage extends Storage
{
    const GLOBAL_NAMESPACE = 'global';
    
    public function __construct ()
    {
        // Start a session if not present
        if(!session_id ()) {
            session_start ();
        }
    }

    public function getAttribute ($name, $ns = null)
    {
        if ($this->hasAttribute ($name, $ns)) {
            if ($ns == null)
                $ns = SessionStorage::GLOBAL_NAMESPACE;
            
            return $_SESSION[$ns][$name];
        } else {
            return null;
        }
    }

    public function setAttribute ($name, $value, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
            
        $_SESSION[$ns][$name] = $value;
    }

    public function hasAttribute ($name, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
        
        return isset ($_SESSION[$ns][$name]);
    }
    
    public function removeAttribute($name, $ns = null)
    {
        if ($ns == null)
            $ns = SessionStorage::GLOBAL_NAMESPACE;
        
        unset($_SESSION[$ns][$name]);   
    }
}

abstract class Request {
    
    /**
     * @var Map
     */
    protected $attributes;
    
    /**
     * @var Map
     */
    protected $errorMessages;
    
    protected $moduleName;
    
    protected $actionName;
    
    protected $requestMethod;

    public function __construct ()
    {
        $this->attributes    = new ArrayMap();
        $this->errorMessages = new ArrayMap();
    }
    
    public function getModuleName ()
    {
        return $this->moduleName;
    }
    
    public function getActionName ()
    {
        return $this->actionName;
    }

    public function setAttribute($name, $value) 
    {
        $this->attributes->put ($name, $value);
    }

    public function getAttribute($name) 
    {
        $parts = explode(".", $name);
        
        if (sizeOf($parts) == 1) {
            return $this->attributes->get ($name);
        } else {
            // it is an array
            $attribute = $this->attributes->get ($parts[0]);
            return isset($attribute[$parts[1]]) ? $attribute[$parts[1]] : null;  
        }

    }
    
    public function hasAttribute ($name)
    {
        return $this->attributes->containsKey ($name);
    }
    
    public function getAttributes ()
    {
        return $this->attributes;
    }
    

    /*
        ErrorMessages
    */

    /**
     * @return Map
     */
    public function getErrorMessages ()
    {
        return $this->errorMessages;
    }

    
    public function addErrorMessage ($fieldName, $message)
    {
        $this->errorMessages->put($fieldName, $message);
    }

    public function hasErrorMessages ()
    {
        return !$this->errorMessages->isEmpty();
    }

    function getRequestMethod ()
    {
        return $this->requestMethod;
    }

    function setRequestMethod ($requestMethod)
    {
        $this->requestMethod = $requestMethod;
    }
    
    // TODO: iewl willen we dit hier?
    public function addJavascriptFile($file)
    {   
        if (!$this->hasAttribute('javascriptFiles')) {
            $javascriptFiles = new ArrayList();   
        } else {
            $javascriptFiles = $this->getAttribute('javascriptFiles');   
        }

        $javascriptFiles->add($file);

        $this->setAttribute('javascriptFiles', $javascriptFiles);
    }
}


class WebRequest extends Request {

    protected $requestMethod;

    const POST = 1;

    const GET = 2;

    const ALL = 3;

    function __construct ()
    {
		parent::__construct (                                                                                                                                                                                                                                                                                                                                                                                 );
		
        // Get the request method
        switch ($_SERVER['REQUEST_METHOD']) {
            case 'GET':
                $this->setRequestMethod (WebRequest::GET);
                break;

            case 'POST':
                $this->setRequestMethod (WebRequest::POST);
                break;

            default:
                throw new MvcException ('The given HTTP request method is not supported.');
                break;
        }


        // Store the POST and the GET parameters
        /*
        foreach ($_POST as $key => $value) {
        	$value = (empty($value) && !is_numeric($value)) ? null : Toolkit::stripslashes ($value);
            $this->setAttribute ($key, $value);
        }

        foreach ($_GET as $key => $value) {
            $value = (empty($value) && !is_numeric ($value)) ? null : Toolkit::stripslashes ($value);
            $this->setAttribute ($key, $value);
        }


        // Store the files
        foreach ($_FILES as $key => $value) {
            $this->setAttribute ($key, $value);
        }
        */
        
        
        
        if ($this->hasAttribute ('module')) {
            $this->moduleName = $this->getAttribute ('module');
        } else {
            $this->moduleName = WEBAPP_DEFAULT_MODULE;
        }
        
        if ($this->hasAttribute ('action')) {
            $this->actionName = $this->getAttribute ('action');
        } else {
            $this->actionName = WEBAPP_DEFAULT_ACTION;
        }

    }  
}

class StubWebRequest extends WebRequest {
    function __construct ($requestMethod) 
    {
        Request::__construct();

        $this->requestMethod = $requestMethod;
    }
}



class InternalWebRequest extends StubWebRequest {

	public function __construct ()
	{
		parent::__construct (WebRequest::GET);
	}
}



class FileNotFoundException extends Exception {}


class MvcException extends Exception {}



class Action
{
    // +-----------------------------------------------------------------------+
    // | PRIVATE VARIABLES                                                     |
    // +-----------------------------------------------------------------------+

    protected $context = null;
    
    protected $presentation;

   
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

class Template
{
    protected $template;
        
    public function __construct($template) 
    {
        $this->template = $template;
    }  
    
    
    public function render($attributes) {
        $template = file_get_contents($this->template);

        $keySet = $attributes->keySet();
        for ($i = 0; $i < sizeof($keySet); $i++) {
            $key   = $keySet[$i];
            $value = $attributes->get($key);

            $template = str_replace("[$" . $key . "$]", $value, $template);
        }

        return $template;
    }
    
}

abstract class View {

    // +-----------------------------------------------------------------------+
    // | CONSTANTS                                                             |
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


abstract class PhpView extends View {

    // +-----------------------------------------------------------------------+
    // | PRIVATE ATTRIBUTES                                                     |
    // +-----------------------------------------------------------------------+

    protected $template = null;



    // +-----------------------------------------------------------------------+
    // | METHODS                                                               |
    // +-----------------------------------------------------------------------+

 
    public function setTemplate ($template)
    {

    }

    protected function getTemplate ()
    {
        if (!$this->template) {

            $actionName 	= $this->getActionName ();
            $viewName 		= $this->getViewName ();

            $template = $actionName . $viewName . 'Template.php';
			$this->setTemplate ($template);
        }

        return $this->template;
    }


    // -------------------------------------------------------------------------


    public function render (Request $request)
    {
        /*
		// Make attributes accessible in the template
        $attributes = $request->getAttributes();
        //foreach ($request->getAttributes () as $key => $value)
        	//$$key = $value;

        // Make the error messages accesible in the template
        $errorMessages = $request->getErrorMessages ();

        // Make the isAuthenticated status accessible in the template
        $user = $this->getContext()->getUser ();
       
        // render to variable
        ob_start();

        //require($this->getTemplate());

        $content = ob_get_contents();

        ob_end_clean();
        
        $request->setAttribute ('content', $content);                
        
        // Look if we need to decorate the output
    	if ($this->isDecorated ()) {
			$controller = $this->getContext()->getController();
	    	$decorator = $controller->getDecorator ($this->decorator);
	    	$decorator->execute ($request);
	    
			// Return the decorated output			
			return $decorator->render ($request);
		} else {
	        // Return the undecorated output
    	    return $content;
		}*/
    }
}

abstract class XslView extends View {
    
    protected $template = null;
    

    public function setTemplate ($template)
    {
        // Look if this template exists.
        if (is_readable ($file = (NXT_MODULES_DIR . $this->moduleName . '/'. $this->actionName . '/templates/' . $template))) {
            $this->template = $file;
        } elseif (is_readable ($template)) {
            $this->template = $template;
        } else {
            $message = 'The given template doesn\'t exist: ' . $template;
            
            throw new FileNotFoundException ($message);
        }
    }

    protected function getTemplate ()
    {
        if (!$this->template) {

            $actionName     = $this->getActionName ();
            $viewName       = $this->getViewName ();

            $template = $actionName . $viewName . 'Template.xsl';
            $this->setTemplate ($template);
        }

        return $this->template;
    }
    
    public function render (Request $request)
    {

        $document = $request->getAttribute ('document');
        
        // Look if there exists a javascript file for this actions
        if (is_readable ($file = (NXT_MODULES_DIR . $this->getModuleName() . '/' . $this->getActionName() . '/static/js/' . $this->getActionName() . 'Action.js'))) {

            $javascriptFiles = $request->getAttribute ('javascriptFiles');
            if ($javascriptFiles == null) {
                $javascriptFiles = new ArrayList ();
                $request->setAttribute('javascriptFiles', $javascriptFiles);            
            }
                
            if (!$javascriptFiles->contains('/' . $file))
               $javascriptFiles->add('/' . $file);
        }
        
        $xsl = new DOMDocument ();
        $xsl->load($this->getTemplate ());
        $xsl->xinclude();
        
        $proc = new XSLTProcessor ();
        $proc->importStyleSheet($xsl);
        
        $keySet = $request->getAttributes()->keySet();
        for ($i = 0; $i < sizeof($keySet); $i++) {
            $key = $keySet[$i];
            $value = $request->getAttributes()->get($key);

            if (is_string ($value)) 
                $proc->setParameter ('', $key, $value);
        }

        
        $time_start = microtime(true);
        
        $content = $proc->transformToXml($document);
        
        $time_end = microtime(true);
        $time = $time_end - $time_start;
        
        
        $request->setAttribute ('xsltExecutionTime', $time);
        
        $request->setAttribute ('content', $content);
        
        // Look if we need to decorate the output
        if ($this->isDecorated ()) {
            $controller = $this->getContext()->getController();
            $decorator = $controller->getDecorator ($this->decorator);
            $decorator->execute ($request);
        
            // Return the decorated output          
            return $decorator->render ($request);
        } else {
            // Return the undecorated output
            return $content;
        }
    }    
}


abstract class User {
    protected $context = null;

    
    public function initialize (Context $context)
    {
        $this->context = $context;
    }

    /**
     * Returns a reference to the context
     * 
     * @return Context
     */
    protected function getContext ()
    {
        return $this->context;
    }

    /**
     * Attributes in the User object are kept in a session. They will 
     * be available in a next http request.
     *
     * @param string $name
     * @param object $value
     */
    public function setAttribute($name, $value, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        $storage->setAttribute ($name, $value, $ns);
    }

    public function getAttribute($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->getAttribute ($name, $ns);
    }
    
    public function removeAttribute($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->removeAttribute ($name, $ns);
    }

    public function hasAttribute ($name, $ns = 'application')
    {
        $storage = $this->getContext()->getStorage ();
        
        return $storage->hasAttribute ($name, $ns);
    }

}


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



class FilterChain {
    protected $chain = array ();
    protected $index = -1;
    protected $context = null;

    public function initialize ($context)
    {
        $this->context = $context;
    }

    public function register (Filter $filter)
    {
        $filter->initialize ($this->context);
        $this->chain[] = $filter;
    }


    public function execute (Action $action, Request $request)
    {
        $this->index++;

        if ($this->index < count($this->chain)) {

            $this->chain[$this->index]->execute($this, $action, $request);

        }
    }

    public function reset ()
    {
        $this->index = -1;
    }
}

abstract class Filter {

	protected $context;	

	abstract public function execute (FilterChain $filterChain, Action $action, Request $request);

	/**
	 * Returns a reference to the context
	 *
	 * @return Context
	 */
	protected function getContext ()
	{
		return $this->context;
	}

	public function initialize (Context $context)
	{
		$this->context = $context;
	}
}

class SecurityFilter extends Filter {

    public function execute (FilterChain $filterChain, Action $action, Request $request)
    {
        $user = $this->getContext ()->getUser ();

        if ($action->isSecure () && !$user->isAuthenticated ()) {

            $controller = $this->getContext()->getController ();            
            $action->setPresentation ($controller->execute (
                new LoginAction (),
                new InternalWebRequest ()
            ));

        } else {

            // Continue with this action
            $filterChain->execute($action, $request);

        }
    }
}

class DatabaseFilter extends Filter {

    protected $initialized = false;

    /**
     * Initializes a database connnection. 
     *
     * @param Filterchain $filterChain
     * @param Action $action
     * @param Request $request
     * 
     */
    public function execute (FilterChain $filterChain, Action $action, Request $request)
    {
        // Look if there is a database connection
        if (!$this->initialized) {
            
            $databasePool = DatabasePool::getInstance();
            $database = new MysqlDatabase();
            $database->connect(WEBAPP_DATABASE_HOST, WEBAPP_DATABASE_USERNAME, WEBAPP_DATABASE_PASSWORD, WEBAPP_DATABASE_DATABASE);
            $databasePool->register($database, 'default');
            
            $this->initialized = true;
        }

        $filterChain->execute ($action, $request);

    }
}


class ExecutionFilter extends Filter {

	/**
	 * Executes action and the view. This is the last filter in the chain
	 *
	 * @param FilterChain $filterChain
	 * @param Action $action
	 * @param Request $request
	 */
    public function execute (FilterChain $filterChain, Action $action, Request $request)
    {
    	// Look if the request method is right
		if ($action->getRequestMethod () & $request->getRequestMethod ()) {
     		
            if (!$action->validate ($request)) {

                // handle error
                $view = $action->handleError ($request);

            } else {

                // No validation errors found. Fire the execute method
                $view = $action->execute($request);

            }

        } else {
            // getDefaultView
            $view = $action->defaultView($request);
        }

        /*
            ---------------------------------------------------
            View
            ---------------------------------------------------
        */

        if ($view != null) {
            // Initialize view
            $view->initialize ($this->getContext());

			// Execute view
			$view->execute ($request);
			
			// Render view
            $action->setPresentation ($view->render ($request));
        }
    }
}

class Controller {

    /**
     * @var FilterChain
     */
    protected $filterChain = null;

    /**
     * @var Context
     */
    protected $context = null;
  
    public function initialize ($context)
    {
        // Save the context
        $this->context = $context;

        // This sets the default filterchain...

        // Make a filterchain
        $this->filterChain = new FilterChain ();
        $this->filterChain->initialize ($context);
    }
    
    public function setFilterChain(FilterChain $filterChain)
    {
        $this->filterChain = $filterChain;   
    }

    protected function getContext ()
    {
        return $this->context;
    }

    /**
     * 
     * @throws MvcException
     */
    public function execute (Action $action, Request $request)
    {
        $action->initialize ($this->context);


        // Get the filterChain and execute it.
        $this->filterChain->reset ();        
        $this->filterChain->execute ($action, $request);

		return $action->getPresentation ();
    }
}

class WebController extends Controller {
    
    /*
    public function initialize (Context $context)
    {
        parent::initialize($context);
    }
    */
    

}


class Log {

    protected static $instance = null;
    
    protected $debugMode;
    
    protected function __construct () 
    {
        $this->debugMode = true;
    }
    
    /**
     * Singleton
     *
     * @return Log
     */
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new Log ();
        }
        return self::$instance;
    }
    
    
    public function debug ($message)
    {
        if ($this->debugMode)   
            echo $message . '<br>';
    }
    
    public function write ($message)
    {
        echo $message;
    }
}    


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
    
    public function __construct($controller, $user, $storage) 
	{
        $this->controller	= $controller;
        $this->user			= $user;
        $this->storage 		= $storage;

        $a = $controller;
        
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


interface Mailable {

    public function getToAddresses ();
    
    public function getSubject ();
    
    public function build ();
    
}

class MailException extends Exception { }

/******************************************************************************
 * Mime messages                                                              *
 *****************************************************************************/
 
interface Encoding {

    public function getType ();
    
    public function encode ($content);

}

class Base64Encoding implements Encoding {

    protected function __construct () { }
    
    protected static $instance;
    
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new Base64Encoding ();
        }
        
        return self::$instance;
    }

    public function getType ()
    {
        return 'base64';
    }

    public function encode ($content)
    {
        return rtrim (chunk_split (base64_encode ($content), 76, MimeMail::LINE_BREAK_SEQUENCE));
    }
}

class SevenBitEncoding implements Encoding {

    protected function __construct () { }
    
    protected static $instance;
    
    public static function getInstance ()
    {
        if (self::$instance == null) {
            self::$instance = new SevenBitEncoding ();
        }
        
        return self::$instance;
    }

    public function getType ()
    {
        return '7bit';
    }

    public function encode ($content)
    {
        return $content;
    }

}
 
class MimeEntity {

    protected $contentType;
    protected $contentTransferEncoding;
    protected $contentDisposition;
    protected $contentID;
    protected $content;

    public function __construct ($content, $contentType, $contentTransferEncoding, $contentDisposition = null, $contentID = null)
    {
        $this->content = $content;
        $this->contentType = $contentType;
        $this->contentTransferEncoding = $contentTransferEncoding;
        $this->contentDisposition = $contentDisposition;
        $this->contentID = $contentID;
    }

    public function build ()
    {
        $headers  = isset ($this->contentType) ? 'Content-Type: ' . $this->contentType . MimeMail::LINE_BREAK_SEQUENCE : ''; 
        $headers .= isset ($this->contentTransferEncoding) ? 'Content-Transfer-Encoding: ' . $this->contentTransferEncoding->getType () . MimeMail::LINE_BREAK_SEQUENCE : ''; 
        $headers .= isset ($this->contentDisposition) ? 'Content-Disposition: ' . $this->contentDisposition . MimeMail::LINE_BREAK_SEQUENCE : '';
        $headers .= isset ($this->contentID) ? 'Content-ID: ' . $this->contentID . MimeMail::LINE_BREAK_SEQUENCE : '';

        $headers .= MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= $this->contentTransferEncoding->encode ($this->content); 
        $headers .= MimeMail::LINE_BREAK_SEQUENCE  . MimeMail::LINE_BREAK_SEQUENCE;
        
        return $headers;
    }

}

class CompositeMimeEntity extends MimeEntity {

    protected $children;
    
    protected $boundary;

    public function __construct ()
    {
        $this->content = 'This part of the E-mail should never be seen. If you are reading this, consider upgrading your e-mail client to a MIME-compatible client.';
        $this->children = new ArrayList ();
        $this->boundary = 'XX' . chr (rand (65, 90)) . '------' . md5 (uniqid (rand (0, getrandmax()), false), false);
    }

    
    public function add ($mimeEntity)
    {
        $this->children->add ($mimeEntity);
    }

    public function getBoundary ()
    {
        return $this->boundary;
    }


    public function build ()
    {
        $headers = parent::build ();
        
        for ($i = 0; $i < $this->children->length(); $i++) {
            $headers .= '--' . $this->getBoundary () . MimeMail::LINE_BREAK_SEQUENCE;
            $headers .= $this->children->get($i)->build ();
        }

        $headers .= '--' . $this->getBoundary () . '--' . MimeMail::LINE_BREAK_SEQUENCE;

        return $headers;
    }
}

class AlternativeMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct ();

        $this->contentType = sprintf ('multipart/alternative; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class MixedMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct ();

        $this->contentType = sprintf ('multipart/mixed; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class RelatedMimeEntity extends CompositeMimeEntity {

    public function __construct ()
    {
        parent::__construct();
        
        $this->contentType = sprintf ('multipart/related; boundary=%s', $this->getBoundary ());
        $this->contentTransferEncoding = SevenBitEncoding::getInstance ();
    }

}

class MimeMail implements Mailable {

    const LINE_BREAK_SEQUENCE = '\n';
    
    protected $rootMimeEntity = null;

    protected $subject;

    protected $fromAddress;

    protected $version = '1.0';

    protected $toAddresses;
    
    protected $ccAddresses;
    
    protected $bccAddresses;

    public function __construct ($fromAddress, $subject)
    {
        $this->fromAddress = $fromAddress;
        $this->subject = $subject;
        
        $this->toAddresses = new ArrayList();
        $this->ccAddresses = new ArrayList();
        $this->bccAddresses = new ArrayList();
    }

    public function getVersion ()
    {
        return $this->version;
    }

    protected function getDate ()
    {
        return date ('r', time());
    }

    public function hasFromAddress ()
    {
        return (($this->fromAddress) ? true : false);
    } 
    public function setFromAddress ($fromAddress)
    {
        $this->fromAddress = $fromAddress;
    }

    public function setSubject ($subject)
    {
        $this->subject = $subject;
    }
    
    public function hasSubject ()
    {
        return (($this->subject) ? true : false);
    } 

    public function getSubject ()
    {
        return $this->subject;
    }
    
    public function addToAddress ($address)
    {
        $this->toAddresses->add($address);
    }

    
    public function removeToAddresses ()
    {
        $this->toAddresses = new ArrayList ();
    }
    
    public function getToAddresses ()
    {
        return $this->toAddresses;
    }

    public function addCcAddress ($address)
    {
        $this->ccAddresses->add($address);
    }

    public function addBccAddress ($address) {
        $this->bccAddresses->add($address);
    }


    public function setRootMimeEntity ($rootMimeEntity)
    {
        $this->rootMimeEntity = $rootMimeEntity;
    }
    
    public function hasRootMimeEntity ()
    {
        return (($this->rootMimeEntity) ? true : false);
    } 

    public function build ()
    {        
        if (!$this->hasFromAddress ())
            throw new MailException ('The from address doesn\'t exists.');

        if (!$this->hasSubject ()) 
            throw new MailException ('The subject doesn\'t exists.');
        
        if ($this->toAddresses->isEmpty () && $this->ccAddresses->isEmpty () && $this->bccAddresses->isEmpty ()) 
            throw new MailException ('There is no valid to, cc or bcc address');
        
        if (!$this->hasRootMimeEntity ()) 
            throw new MailException ('The root mime entity doesn\'t exists.');
        
        $headers  = 'MIME-Version: ' .  $this->getVersion () . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= 'From: ' .          $this->fromAddress . MimeMail::LINE_BREAK_SEQUENCE;

        if (!$this->toAddresses->isEmpty ()) {
            $toAddresses = array ();
            for ($i = 0; $i < $this->toAddresses->length(); $i++)
                $toAddresses[sizeof($toAddresses)] = $this->toAddresses->get($i);
            
            $headers .= 'To: ' . implode (', ', $toAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
        
        if (!$this->ccAddresses->isEmpty ()) {
            $ccAddresses = array ();
            for ($i = 0; $i < $this->ccAddresses->length(); $i++)
                $ccAddresses[sizeof($ccAddresses)] = $this->ccAddresses->get($i);
        
            $headers .= 'Cc: ' . implode (', ', $ccAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
        
        if (!$this->bccAddresses->isEmpty ()) {
            $bccAddresses = array ();
            for ($i = 0; $i < $this->bccAddresses->length(); $i++)
                $bccAddresses[sizeof($bccAddresses)] = $this->bccAddresses->get($i);
        
            $headers .= 'Bcc:  ' . implode (', ', $bccAddresses) . MimeMail::LINE_BREAK_SEQUENCE;
        }
            
        $headers .= 'Date: ' . $this->getDate () . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= 'Subject: ' . $this->subject . MimeMail::LINE_BREAK_SEQUENCE;
        $headers .= $this->rootMimeEntity->build();
        

        return $headers;
    }
}

/******************************************************************************
 * Mail Transport                                                             *
 *****************************************************************************/
 
interface MailTransport {
    public function open ();
    
    public function send ($mailable);
    
    public function close ();
}

class NativeMailTransport implements MailTransport {

    public function open () { }
    
    public function send ($mailable)
    {
        $headers = $mailable->build ();
        
        // Implode to addresses
        $toAddresses = array ();
        for ($i = 0; $i < $mailable->getToAddresses()->length(); $i++)
          $toAddresses[sizeof($toAddresses)] = $mailable->getToAddresses()->get($i);
        
        // Send email
        //if (!mail (implode (', ', $toAddresses), $mailable->getSubject (), '', $headers, ''))
        //    throw new MailException ('Failed to send email');
    }

    public function close () { }

}

class Mail implements Mailable {
    
    /**
     * @var MimeMail
     */
    protected $mimeMail;
    
    protected $textContent;
    protected $htmlContent;
    
    public function __construct ($fromAddress, $subject)
    {
        $this->mimeMail = new MimeMail($fromAddress, $subject);
    }
    
    public function addToAddress ($address)
    {
        $this->mimeMail->addToAddress($address);
    }
    
    public function setHtmlContent ($content)
    {
        $this->htmlContent = $content;
    }
    
    public function setTextContent ($content)
    {
        $this->textContent = $content;
    }
    
    public function addAttachment ()
    {
        throw new Exception ("Not yet implemented"); 
    }
    
    public function build ()
    {
        $hasTextContent = ($this->textContent) ? true : false;
        $hasHtmlContent = ($this->htmlContent) ? true : false;
        
        if ($hasHtmlContent && $hasTextContent) {
            throw new Exception ("Both text and html content has not yet been implemented"); 
        } elseif ($hasHtmlContent) {
           $entity = new MimeEntity ($this->htmlContent, 'text/html; charset=utf-8', Base64Encoding::getInstance()); 
            
           $this->mimeMail->setRootMimeEntity($entity);
        } elseif ($hasTextContent) {
           $entity = new MimeEntity ($this->textContent, 'text/plain; charset=utf-8', Base64Encoding::getInstance()); 
            
           $this->mimeMail->setRootMimeEntity($entity); 
        } else {
           throw new MailException ('No content specified');
        }
        
        return $this->mimeMail->build();
    }
    
    public function getSubject ()
    {
       return $this->mimeMail->getSubject ();
    }
    
    public function getToAddresses ()
    {
       return $this->mimeMail->getToAddresses ();   
    }
}

class AppController extends WebController {
    
    public function initialize (Context $context)
    {
        parent::initialize($context);

        $this->filterChain->register (new SecurityFilter ());
        $this->filterChain->register (new ExecutionFilter ());
    }
}

class LoginAction extends Action {
    function execute (Request $request)
    {
        return new LoginInputView ();
    }    
}

class LoginInputView extends SimpleView {
    function execute (Request $request)
    {
        $this->setTemplate("login.tpl");
        $request->setAttribute ('title', 'Login');
    }
}

class SecureAction extends Action {
    public function isSecure ()
    {
        return true;
    }
}

class SendEmailAction extends Action {

    function execute (Request $request)
    {        
        $attributes = new ArrayMap();
        $attributes->put('name', $request->getAttribute ('name'));
        $attributes->put('email', $email = $request->getAttribute ('email'));
        $attributes->put('subject', $subject = $request->getAttribute ('subject'));
        $attributes->put('message', $request->getAttribute ('message'));

        $template = new Template('message.tpl');
        $content = $template->render($attributes);
        
        $mail = new Mail ($email, $subject);
        $mail->addToAddress (WEBAPP_CONTACT_EMAIL);
        $mail->setHtmlContent ($content);

        try { 
            $transport = new NativeMailTransport ();
            $transport->open ();
            $transport->send ($mail);
            $transport->close ();
            
            return new SendEmailSuccessView();
        } catch (MailException $e) {            
            $request->setAttribute ('message', 'Het versturen van de email is mislukt.');
            
            return new SendEmailErrorView();
        }
    }

    function validate (Request $request)
    {
        $validatorManager = new ValidatorManager ($request->getErrorMessages ());
        
        $v = new RequiredValidator ($request->getAttribute('name'), 'Your name is required.');
        $validatorManager->register('name',$v);
        
        $v = new EmailValidator ($request->getAttribute('email'), 'Your email address is required.');
        $validatorManager->register ('email',$v);

        $v = new RequiredValidator ($request->getAttribute('subject'), 'Please provide a subject.');
        $validatorManager->register ('subject',$v);

        $v = new RequiredValidator ($request->getAttribute('message'), 'Please provide a message.');
        $validatorManager->register ('message',$v);

        return $validatorManager->execute ();
    }

    function handleError (Request $request)
    {   
        return new SendEmailInputView();
    }

    function defaultView (Request $request)
    {
        return new SendEmailInputView();
    }

    function getRequestMethod ()
    {
        return WebRequest::POST;
    }

}

abstract class SimpleView extends View {
    protected $template;

    public function render (Request $request) 
    {
        return $this->template->render($request->getAttributes());
    }

    public function setTemplate ($template)
    {
        $this->template = new Template ($template);
    }
}

class SendEmailSuccessView extends SimpleView {

    function execute (Request $request)
    {
        $this->setTemplate("success.tpl");
        $request->setAttribute ('title', 'Bericht verstuurd');
    }
}

class SendEmailInputView extends SimpleView
{
    function execute (Request $request)
    {
        $this->setTemplate("input.tpl");
        $request->setAttribute ('title', 'Contact');
    }
}

class SendEmailErrorView extends SimpleView {

    function execute (Request $request)
    {
        $this->setTemplate('error.tpl');
        $request->setAttribute ('title', 'Foutmelding');
    }
}

define ('WEBAPP_CONTACT_EMAIL', 'no@spam.com');

/*
try {
    // Initialize the framework
    $user       = new SecurityUser ();
    $controller = new AppController();
    $storage    = new SessionStorage ();
    $context    = new Context($controller, $user, $storage);
    
    // Execute the framework
    $request = new StubWebRequest(WebRequest::POST);
    //$request->setAttribute('name', 'Henk Erik');
    //$request->setAttribute('email', 'no@spam.com');
    //$request->setAttribute('subject', 'Test subject');
    //$request->setAttribute('message', 'Test message');
    echo $controller->execute (new SecureAction(), $request);
} catch (Exception $e) {
    echo $e->getMessage ();
}

exit();

*/

abstract class TestCase {
    public function setup () {
        
    }
    
    public function tearDown () {
        
    }
    
    protected function pass () 
    {
        echo "PASS\n";
    }

    protected function fail ()
    {
        echo "FAIL\n";
    }
}


class SendEmailActionTest extends TestCase {

    protected $controller;

    public function setUp ()
    {
        $user             = new SecurityUser ();
        $this->controller = new AppController();
        $storage          = new SessionStorage ();
        $context          = new Context($this->controller, $user, $storage);
    }

    public function testSecureAction ()
    {
        $request = new StubWebRequest (WebRequest::GET);
        try {
            $this->controller->execute(new SecureAction(), $request);
            $this->pass();
        } catch (Exception $e) {
            $this->fail();
        }
    }

    public function testDisplayForm ()
    {
        $request = new StubWebRequest (WebRequest::GET);
        try {
            $this->controller->execute(new SendEmailAction(), $request);
            $this->pass();
        } catch (Exception $e) {
            $this->fail();
        }
    }

    public function testInvalidInput ()
    {
        $request = new StubWebRequest (WebRequest::POST);
        $request->setAttribute('email', 'invalid');
        try {
            $this->controller->execute(new SendEmailAction(), $request);
            $this->pass();
        } catch (Exception $e) {
            $this->fail();
        }
    }

    public function testValidInput ()
    {
        $request = new StubWebRequest (WebRequest::POST);
        $request->setAttribute('name', 'Henk Erik');
        $request->setAttribute('email', 'no@spam.com');
        $request->setAttribute('subject', 'Test subject');
        $request->setAttribute('message', 'Test message');
        try {
            $this->controller->execute(new SendEmailAction(), $request);
            $this->pass();
        } catch (Exception $e) {
            $this->fail();
        }
    }
}


$test = new SendEmailActionTest ();
$test->setUp();
$test->testSecureAction();
$test->setUp();
$test->testDisplayForm();
$test->setup();
$test->testInvalidInput();
$test->setUp();
$test->testValidInput();

?>