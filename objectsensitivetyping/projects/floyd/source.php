<?php
	/**
 * @copyright Copyright (c) 2005 by Bruno R. Preiss, P.Eng.
 *
 * @author $Author: brpreiss $
 * @version $Id: Exceptions.php,v 1.8 2005/12/09 01:11:11 brpreiss Exp $
 * @package Opus11
 */
	
	/**
 * Argument error exception.
 *
 * @package Opus11
 */
	class ArgumentError extends Exception
	{
		/**
     * Constructs an ArgumentError.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Container empty exception.
 *
 * @package Opus11
 */
	class ContainerEmptyException extends Exception
	{
		/**
     * Constructs a ContainerEmptyException.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Container full exception.
 *
 * @package Opus11
 */
	class ContainerFullException extends Exception
	{
		/**
     * Constructs a ContainerFullException.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Index error exception.
 *
 * @package Opus11
 */
	class IndexError extends Exception
	{
		/**
     * Constructs an IndexError.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Method not implemented exception.
 *
 * @package Opus11
 */
	class MethodNotImplementedException extends Exception
	{
		/**
     * Constructs an MethodNotImplementedException.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Type error exception.
 *
 * @package Opus11
 */
	class TypeError extends Exception
	{
		/**
     * Constructs an ArgumentError.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * State error exception.
 *
 * @package Opus11
 */
	class StateError extends Exception
	{
		/**
     * Constructs an ArgumentError.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	/**
 * Illegal operation exception.
 *
 * @package Opus11
 */
	class IllegalOperationException extends Exception
	{
		/**
     * Constructs an IllegalOperationException.
     */
		public function __construct()
		{
			parent::__construct(__CLASS__);
		}
	}
	
	//{
	/**
 * Interface implemented by all objects.
 */
	interface IObject
	{
		/**
     * Returns a unique identifier for this object.
     * @return integer An identifier.
     */
		function getId();
		/**
     * Returns a hash code for this object.
     * @return integer A hash code. 
     */
		function getHashCode();
		/**
     * Returns the class of this object.
     * @return object ReflectionClass A ReflectionClass.
     */
		function getClass();
	}
	//}>a
	
	//{
	/**
 * Returns a hash code for the given item.
 * @param mixed item An item.
 * @return integer A hash code.
 */
	function hash_($item)
	{
		$type = gettype($item);
		if ($type == 'object') {
			return $item->getHashCode();
		} elseif ($type == 'NULL') {
			return 0;
		} else {
			throw new ArgumentError();
		}
	}
	//}>b
	
	//{
	/**
 * Returns a textual representation of the given item.
 * @param mixed item An item.
 * @return string A string.
 
function str($item)
{
    $type = gettype($item);
    if ($type == 'boolean')
    {
        return $item ? 'true' : 'false';
    }
    elseif ($type == 'object')
    {
        return $item->__toString();
    }
    elseif ($type == 'NULL')
    {
        return 'NULL';
    }
    else
    {
        return strval($item);
    }
}
//}>c

*/
	
	//{
	/**
 * Abstract base class from which all object classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractObject implements IObject
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		/**
     * Constructs an AbstractObject.
     */
		public function __construct()
		{
		}
		
		//{
		/**
     * Returns a unique identifier for this object.
     *
     * @return integer An identifier.
     */
		public function getId()
		{
			preg_match('/^Object id #(\d*)$/', strval($this), $matches);
			return intval($matches[1]);
		}
		//}>a
		
		//{
		/**
     * Returns the class of this object.
     *
     * @return object ReflectionClass A ReflectionClass.
     */
		public function getClass()
		{
			return new ReflectionClass(get_class($this));
		}
		//}>b
		
		/**
     * Returns a hash code for this object.
     *
     * @return integer A hash code.
     */
		public function getHashCode()
		{
			return $this->getId();
		}
		
		/**
     * Returns a textual representation of this object.
     *
     * @return string A string.
     */
		public abstract function __toString();
	}
	
	
	/**
 * @copyright Copyright (c) 2005 by Bruno R. Preiss, P.Eng.
 *
 * @author $Author: brpreiss $
 * @version $Id: IComparable.php,v 1.13 2005/12/09 01:11:12 brpreiss Exp $
 * @package Opus11
 */
	
	//{
	/**
 * Interface implemented by all comparable objects.
 *
 * @package Opus11
 */
	interface IComparable
	{
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		function compare(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is equal to the given object.
     */
		function eq(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is not equal to the given object.
     */
		function ne(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is less than the given object.
     */
		function lt(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is less than or equal to the given object.
     */
		function le(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is greater than the given object.
     */
		function gt(IComparable $object);
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object The given object.
     * @return boolean True if this object is greater than or equal to the given object.
     */
		function ge(IComparable $object);
	}
	//}>a
	
	//{
	/**
 * Returns true if the given items compare equal.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are equal.
 */
	function eq($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->eq($right);
		} else {
			return $left == $right;
		}
	}
	//}>b
	
	/**
 * Returns true if the given items compare not equal.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are not equal.
 */
	function ne($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->ne($right);
		} else {
			return $left != $right;
		}
	}
	
	/**
 * Returns true if the left item is greater than the right item.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are equal.
 */
	function gt($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->gt($right);
		} else {
			return $left > $right;
		}
	}
	
	/**
 * Returns true if the left item is greater than or equal to the right item.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are equal.
 */
	function ge($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->ge($right);
		} else {
			return $left >= $right;
		}
	}
	
	/**
 * Returns true if the left item is less than the right item.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are equal.
 */
	function lt($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->lt($right);
		} else {
			return $left < $right;
		}
	}
	
	/**
 * Returns true if the left item is less than or equal to the right item.
 *
 * @param mixed $left An item.
 * @param mixed $right An item.
 * @return boolean True if the given items are equal.
 */
	function le($left, $right)
	{
		if (gettype($left) == 'object' && gettype($right) == 'object') {
			return $left->le($right);
		} else {
			return $left <= $right;
		}
	}
	
	//{
	/**
 * Abstract base class from which all comparable object classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractComparable extends AbstractObject implements IComparable
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		/**
     * Constructs an AbstractComparable.
     */
		public function __construct()
		{
			parent::__construct();
		}
		
		//{
		/**
     * Returns true if this object is equal to the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is equal to the given object.
     */
		public function eq(IComparable $object)
		{
			return $this->compare($object) == 0;
		}
		
		/**
     * Returns true if this object is not equal to the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is not equal to the given object.
     */
		public function ne(IComparable $object)
		{
			return $this->compare($object) != 0;
		}
		
		/**
     * Returns true if this object is less than the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is less than the given object.
     */
		public function lt(IComparable $object)
		{
			return $this->compare($object) < 0;
		}
		
		/**
     * Returns true if this object is less than or equal to the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is
     * less than or equal to the given object.
     */
		public function le(IComparable $object)
		{
			return $this->compare($object) <= 0;
		}
		
		/**
     * Returns true if this object is greater than the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is greater than the given object.
     */
		public function gt(IComparable $object)
		{
			return $this->compare($object) > 0;
		}
		
		/**
     * Returns true if this object is greater than or equal to the given object.
     * 
     * @param object IComparable $object A comparable object.
     * @return boolean True if this object is
     * greater than or equal to the given object.
     */
		public function ge(IComparable $object)
		{
			return $this->compare($object) >= 0;
		}
		//}>a
		
		//{
		/**
     * Compares this object with the given object.
     * This object and the given object are instances of the same class.
     *
     * @param object IComparable $object The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		protected abstract function compareTo(IComparable $object);
		
		/**
     * Compares this object with the given object.
     *
     * @param object IComparable $object A comparable object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		public function compare(IComparable $object)
		{
			$result = 0;
			if ($this->getClass() == $object->getClass()) {
				$result = $this->compareTo($object);
			} else {
				$result = strcmp($this->getClass()->getName(), $object->getClass()->getName());
			}
			return $result;
		}
		//}>b
		
		/**
     * Main program.
     *
     * @param array $args Command-line arguments.
     * @return integer Zero on succes; non-zero on failure.
     */
		public static function main($args)
		{
			printf("AbstractComparable main program.\n");
			return 0;
		}
	}
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(AbstractComparable::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * Interface implemented by all visitors.
 *
 * @package Opus11
 */
	interface IVisitor
	{
		/**
     * Visits the given object.
     *
     * @param object IObject $obj The object to visit.
     */
		function visit(IObject $obj);
		/**
     * IsDone predicate.
     *
     * @return boolean True if this visitor is done.
     */
		function isDone();
	}
	//}>a
	
	interface IReduceFunction
	{
		function invoke($acc, $item);
	}
	
	
	//{
	/**
 * Interface implemented by all containers.
 *
 * @package Opus11
 */
	interface IContainer extends IComparable, IteratorAggregate
	{
		/**
     * Count getter.
     *
     * @return integer The number of items in this container.
     */
		function getCount();
		/**
     * IsEmpty predicate.
     *
     * @return boolean True if this container is empty.
     */
		function isEmpty();
		/**
     * IsFull predicate.
     *
     * @return boolean True if this container is full.
     */
		function isFull();
		/**
     * Purges this container.
     */
		function purge();
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this container.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this container.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial value.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		function reduce(IReduceFunction $f, $initialState);
		/**
     * Calls the visit method of the given visitor
     * for each item in this container.
     *
     * @param object IVisitor $visitor A visitor.
     */
		function accept(IVisitor $visitor);
	}
	//}>a
	
	//{
	/**
 * Abstract base class from which all container classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractContainer extends AbstractComparable implements IContainer
	{
		/**
     * @var integer The number of items in this container.
     */
		protected $count;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		/**
     * Constructs an AbstractContainer.
     */
		public function __construct()
		{
			parent::__construct();
			$this->count = 0;
		}
		
		/**
     * Purge method.
     */
		public function purge()
		{
			$this->count = 0;
		}
		
		//{
		/**
     * Count getter.
     *
     * @return integer The number of items in this container.
     */
		public function getCount()
		{
			return $this->count;
		}
		
		/**
     * IsEmpty predicate.
     *
     * @return boolean True if this container is empty.
     */
		public function isEmpty()
		{
			return $this->getCount() == 0;
		}
		
		/**
     * IsFull predicate.
     *
     * @return boolean True if this container is full.
     */
		public function isFull()
		{
			return false;
		}
		//}>a
		
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this container.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this container.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial state.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		public function reduce(IReduceFunction $f, $initialState)
		{
			$state = $initialState;
			foreach ($this as $obj) {
				$state = $f->invoke($state, $obj);
			}
			return $state;
		}
		
		/**
     * Calls the visit method of the given visitor
     * for each object in this container.
     *
     * @param object IVisitor $visitor A visitor.
     */
		public function accept(IVisitor $visitor)
		{
			foreach ($this as $obj) {
				if ($visitor->isDone()) 
					break;
				$visitor->visit($obj);
			}
		}
		
		//{
		/**
     * Returns a textual representation of this container.
     *
     * @return string A string.
     */
		public function __toString()
		{
			//        $s = $this->reduce(
			//            create_function(
			//                '$s, $item', 
			//                'return array($s[0] . $s[1] . str($item), ", ");'
			//            ), array('',''));
			//        return $this->getClass()->getName() .  '{' . $s[0] . '}';
			$s = $this->reduce(new AbstractContainerToStringReduceFunction(), array('', ''));
			return $this->getClass()->getName() . '{' . $s[0] . '}';
		}
		//}>b
		
		//{
		/**
     * Returns a hash code for this container.
     *
     * @return integer A hash code. 
     */
		public function getHashCode()
		{
			//        $s = $this->reduce(
			//            create_function(
			//                '$s, $obj',
			//                'return $s + $obj->getHashCode();'
			//            ), 0);
			//        return $s;
			
			return $this->reduce(new AbstractContainerHashCodeReduceFunction(), 0);
		}
	}
	//}>c
	
	
	class AbstractContainerToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			return array($s[0] . $s[1] . $item->__toString(), ", ");
		}
	}
	class AbstractContainerHashCodeReduceFunction implements IReduceFunction
	{
		public function invoke($s, $obj)
		{
			return $s + $obj->getHashCode();
		}
	}
	
	
	//{
	/**
 * Abstract base class from which all visitor classes are derived..
 *
 * @package Opus11
 */
	abstract class AbstractVisitor implements IVisitor
	{
		/**
     * Constructs this AbstractVisitor.
     */
		public function __construct()
		{
		}
		
		/**
     * IsDone predicate.
     *
     * @return boolean True if this visitor is done.
     */
		public function isDone()
		{
			return false;
		}
	}
	//}>a
	
	//{
	/**
 * Interface implemented by all graph edges.
 *
 * @package Opus11
 */
	interface IEdge extends IComparable
	{
		/**
     * Returns one of the two vertices that this edge joins.
     * If this edge is a directed edge,
     * this method returns the tail of the arc.
     *
     * @return object IVertex One of the vertices that this edge joins.
     */
		function getV0();
		/**
     * Returns one of the two vertices that this edge joins.
     * If this edge is a directed edge,
     * this method returns the head of the arc.
     *
     * @return object IVertex One of the vertices that this edge joins.
     */
		function getV1();
		/**
     * Returns an object that represents the weight associated with this edge.
     *
     * @return mixed The weight associated with this edge.
     */
		function getWeight();
		/**
     * Tests whether this edge is a directed edge.
     * A directed edge is an edge in a directed graph.
     *
     * @return boolean True if this edge is a directed edge; false otherwise.
     */
		function isDirected();
		/**
     * Returns the vertex at the end of this edge opposite
     * to the specified vertex.
     * It is always the case that Mate(V0())==V1() and Mate(V1())==VO().
     *
     * @param object IVertex $vertex The specified vertex.
     */
		function getMate(IVertex $vertex);
	}
	//}>a
	
	//{
	/**
 * Interface implemented by all graph vertices.
 *
 * @package Opus11
 */
	interface IVertex extends IComparable
	{
		/**
     * Returns the number of this vertex.
     * @return integer The number of this vertex.
     */
		function getNumber();
		/**
     * Returns an object the represents the weight associated with this vertex.
     * @return mixed The weight associated with this vertex.
     */
		function getWeight();
		/**
     * Returns the edges incident on this vertex.
     *
     * @return object IteratorAggregate
     * The edges incident on this vertex.
     */
		function getIncidentEdges();
		/**
     * Returns the edges emanating from this vertex.
     *
     * @return object IteratorAggregate The edges emanating from this vertex.
     */
		function getEmanatingEdges();
		/**
     * Returns the vertices that are the predecessors of this vertex.
     *
     * @return object IteratorAggregate
     * The vertices that are the predecessors of this vertex.
     */
		function getPredecessors();
		/**
     * Returns the vertices that are the successors of this vertex.
     *
     * @return object IteratorAggregate
     * The vertices that are the successors of this vertex.
     */
		function getSuccessors();
	}
	//}>a
	
	//{
	/**
 * Interface implemented by all graphs.
 *
 * @package Opus11
 */
	interface IGraph extends IContainer
	{
		/**
     * Returns the number of edges in this graph.
     *
     * @return integer The number of edges in this graph.
     */
		function getNumberOfEdges();
		/**
     * Returns the number of vertices in this graph.
     *
     * @return integer The number of vertices in this graph.
     */
		function getNumberOfVertices();
		/**
     * Tests whether this graph is a directed graph.
     *
     * @return boolean True if this graph is directed; false otherwise.
     */
		function isDirected();
		/**
     * Adds a weighted vertex with a specified number to this graph.
     *
     * @param integer $v The number of the vertex to add.
     * @param mixed $weight The weight to be associated with the vertex.
     */
		function addVertex($v, $weight = NULL);
		/**
     * Returns the vertex in this graph with the specified number.
     *
     * @param integer $v The number of the vertex to be returned.
     * @return The vertex with the specified number.
     */
		function getVertex($v);
		/**
     * Adds a weighted edge to this graph that connects the two vertices
     * specified by their vertex numbers.
     *
     * @param integer $v The vertex at the tail of the edge.
     * @param integer $w The vertex at the head of the edge.
     * @param mixed $weight The weight to be associated with the edge.
     */
		function addEdge($v, $w, $weight = NULL);
		/**
     * Returns the edge that connects the two vertices
     * specified by their vertex numbers.
     *
     * @param integer $v The vertex at the tail of the edge.
     * @param integer $w The vertex at the head of the edge.
     * @return object IEdge The edge that connects the given vertices.
     */
		function getEdge($v, $w);
		/**
     * Tests whether there is an edge in this graph that connects
     * the two vertices specified by their numbers.
     * @param integer $v The vertex at the tail of the edge.
     * @param integer $w The vertex at the head of the edge.
     * @return boolean True if the edge exists; false otherwise.
     */
		function isEdge($v, $w);
		/**
     * Tests whether this graph is connected.
     * If this graph is an directed graph,
     * this method tests whether the graph is weakly connected.
     *
     * @return boolean True if this graph is weakly connected; false otherwise.
     */
		function isConnected();
		/**
     * Tests whether this graph is cyclic.
     *
     * @return True if this graph is cyclic; false otherwise.
     */
		function isCyclic();
		/**
     * Returns the vertices in this graph.
     *
     * @return object IteratorAggregate The vertices in this graph
     */
		function getVertices();
		/**
     * Returns The edges in this graph.
     *
     * @return object iteratorAggreagte The edges in this graph.
     */
		function getEdges();
		/**
     * Causes a visitor to visit the vertices of this directed graph
     * in depth-first traversal order starting from a given vertex.
     * This method invokes the PreVisit and PostVisit methods of the visitor
     * for each vertex in this graph.
     * The traversal continues as long as the IsDone method of the visitor
     * returns false.
     *
     * @param object IPrePostVisitor $visitor The visitor to accept.
     * @param integer $start The vertex at which to start the traversal.
     */
		function depthFirstTraversal(IPrePostVisitor $visitor, $start);
		/**
     * Causes a visitor to visit the vertices of this directed graph
     * in breadth-first traversal order starting from a given vertex.
     * This method invokes the Visit method of the visitor
     * for each vertex in this graph.
     * The traversal continues as long as the IsDone method of the visitor
     * returns false.
     *
     * @param object IVisitor $visitor The visitor to accept.
     * @param integer $start The vertex at which to start the traversal.
     */
		function breadthFirstTraversal(IVisitor $visitor, $start);
	}
	//}>a
	
	//{
	/**
 * Interface implemented by all digraphs.
 *
 * @package Opus11
 */
	interface IDigraph extends IGraph
	{
		/**
     * Tests whether this graph is strongly connected.
     *
     * @return boolean
     * True if this graph is strongly connected; false otherwise.
     */
		function isStronglyConnected();
		/**
     * Causes a visitor to visit the vertices of this directed graph
     * in topological order.
     * This method takes a visitor and,
     * as long as the IsDone method of that visitor returns false,
     * this method invokes the Visit method of the visitor
     * for each vertex in the graph.
     * The order in which the vertices are visited
     * is given by a topological sort of the vertices.
     *
     * @param object IVisitor $visitor The visitor to accept.
     */
		function topologicalOrderTraversal(IVisitor $visitor);
	}
	//}>a
	
	//{
	/**
 * Represents an edge in a graph.
 *
 * @package Opus11
 */
	class Edge extends AbstractComparable implements IEdge
	{
		/**
     * @var object AbstractGraph The graph with which this edge is associated.
     */
		protected $graph = NULL;
		/**
     * @var integer
     * The number of the vertex in this graph from which this edge emanates.
     */
		protected $v0 = 0;
		/**
     * @var integer
     * The number of the vertex in this graph upon which this edge
     * is incident.
     */
		protected $v1 = 0;
		/**
     * @var mixed The weight associated with this edge.
     */
		protected $weight = NULL;
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		/**
     * Constructs an Edge that connects
     * the specified vertices and with the specified weight.
     *
     * @param object AbstractGraph $graph
     * The graph with which this edge is associated.
     * @param integer $v0 The number of the vertex in this graph
     * from which this edge emanates.
     * @param integer $v1 The number of the vertex in this
     * upon which this edge is incident.
     * @param mixed $weight The weight associated with this edge.
     */
		public function __construct(AbstractGraph $graph, $v0, $v1, $weight)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->v0 = $v0;
			$this->v1 = $v1;
			$this->weight = $weight;
		}
		
		/**
     * V0 getter.
     *
     * @return object IVertex
     * The vertex in this graph from which this edge emenates.
     */
		public function getV0()
		{
			return $this->graph->getVertex($this->v0);
		}
		
		/**
     * V1 getter.
     *
     * @return object IVertex
     * The vertex in this graph upon which this edge is incident.
     */
		public function getV1()
		{
			return $this->graph->getVertex($this->v1);
		}
		
		/**
     * Weight getter.
     *
     * @return The weight associated with this edge.
     */
		public function getWeight()
		{
			return $this->weight;
		}
		
		/**
     * Returns the vertex at the end of this edge opposite
     * to the specified vertex.
     * It is always the case that
     * getMate(getV0())==getV1()
     * and
     * getMate(getV1())==getV0().
     *
     * @param object IVertex $v The specified vertex.
     * @return object IVertex The vertex opposite to the specified vertex.
     */
		public function getMate(IVertex $v)
		{
			if ($v->getNumber() == $this->v0) 
				return $this->getV1();
			 elseif ($v->getNumber() == $this->v1) 
				return $this->getV0();
			 else 
				throw new ArgumentError();
		}
		
		/**
     * Tests whether this edge is directed.
     * An edge is directed if it is an edge in a directed graph.
     *
     * @return boolean True if this edge is directed.
     */
		public function isDirected()
		{
			return $this->graph->isDirected();
		}
		
		/**
     * Compares this edge with the specified comparable object.
     * This method is not implemented yet.
     */
		protected function compareTo(IComparable $obj)
		{
			throw new MethodNotImplementedException();
		}
		
		/**
     * HashCode getter.
     *
     * @return A hashcode for this edge.
     */
		public function getHashCode()
		{
			$result = $this->v0 * $this->graph->getNumberOfVertices() + $v1;
			if ($this->weight !== NULL) 
				$result += $weight->getHashCode();
			return $result;
		}
		
		/**
     * Returns a textual representation of this edge.
     *
     * @return string A string.
     */
		public function __toString()
		{
			//debug_print_backtrace(); exit;
			
			$s = '';
			$s .= "Edge{" . $this->v0;
			if ($this->isDirected()) 
				$s .= '->' . $this->v1;
			 else 
				$s .= '--' . $this->v1;
			if ($this->weight !== NULL) 
				$s .= ', weight = ' . $this->weight->__toString();
			$s .= '}';
			return $s;
		}
	}
	
	
	/**
 * @copyright Copyright (c) 2005 by Bruno R. Preiss, P.Eng.
 *
 * @author $Author: brpreiss $
 * @version $Id: IIterator.php,v 1.3 2005/11/27 23:32:32 brpreiss Exp $
 * @package Opus11
 */
	
	//{
	/**
 * Interface implemented by all iterators.
 *
 * @package Opus11
 */
	interface IIterator extends Iterator
	{
		/**
     * Returns the next object to be enumerated by this iterator.
     * Returns NULL when there are not more objects.
     *
     * @return mixed The next object to be enumerated.
     */
		function succ();
	}
	//}>a
	
	//{
	/**
 * Abstract base class from which all iterator classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractIterator implements IIterator
	{
		
		//!    // ...
		//!}
		//}>a
		
		/**
     * Constructor.
     */
		public function __construct()
		{
		}
		
		
		/**
     * Returns the next object to be enumerated by this iterator.
     * Returns NULL when there are no more objects.
     *
     * @return mixed The next object to be enumerated.
     */
		public function succ()
		{
			$result = NULL;
			if ($this->valid()) {
				$result = $this->current();
				$this->next();
			}
			return $result;
		}
	}
	
	
	
	/**
 * Iterator that enumerates the vertices adjacent to a given vertex
 * using a given edge iterator.
 *
 * @package Opus11
 */
	class Vertex_Iterator extends AbstractIterator
	{
		/**
     * @var object Vertex A vertex.
     */
		protected $vertex = NULL;
		/**
     * @var object Iterator An edge iterator.
     */
		protected $edgeIterator = NULL;
		
		/**
     * Constructs a Vertex_Iterator from the given set of edges.
     *
     * @param object Vertex $vertex This vertex.
     * @param object Iterator $edgeIterator An edge iterator.
     */
		public function __construct(Vertex $vertex, Iterator $edgeIterator)
		{
			parent::__construct();
			$this->vertex = $vertex;
			$this->edgeIterator = $edgeIterator;
		}
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->edgeIterator->valid();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->edgeIterator->key();
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			return $this->edgeIterator->current()->getMate($this->vertex);
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$this->edgeIterator->next();
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$this->edgeIterator->rewind();
		}
	}
	
	/**
 * An aggregate of the vertices adjacent to a given vertex
 * in a given aggregate of edges.
 *
 * @package Opus11
 */
	class Vertex_IteratorAggregate implements IteratorAggregate
	{
		/**
     * @var object Vertex A vertex.
     */
		protected $vertex = NULL;
		/**
     * @var object IteratorAggregate An aggregate of edges.
     */
		protected $edges = NULL;
		
		/**
     * Constructs a Vertex_IteratorAggregate from the given aggregate of edges.
     *
     * @param object IteratorAggregate $edges.
     */
		public function __construct(Vertex $vertex, IteratorAggregate $edges)
		{
			$this->vertex = $vertex;
			$this->edges = $edges;
		}
		
		
		/**
     * Returns an iterator that enumerates the elements of this aggregate.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new Vertex_Iterator($this->vertex, $this->edges->getIterator());
		}
	}
	
	//{
	/**
 * Represents a vertex in a graph.
 *
 * @package Opus11
 */
	class Vertex extends AbstractComparable implements IVertex
	{
		/**
     * @var object AbstractGraph The graph with which this edge is associated.
     */
		protected $graph = NULL;
		
		/**
     * @var integer The number of this vertex.
     */
		protected $number = 0;
		/**
     * @var mixed The weight associated with this vertex.
     */
		protected $weight = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		/**
     * Constructs a Vertex with
     * the specified number and weight.
     *
     * @param object AbstractGraph $graph
     * The graph with which this vertex is associated.
     * @param integer $number The specified number.
     * @param mixed $weight The specified weight.
     */
		public function __construct(AbstractGraph $graph, $number, $weight = NULL)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->number = $number;
			$this->weight = $weight;
		}
		
		/**
     * Returns the number of this vertex.
     *
     * @return integer The number of this vertex.
     */
		public function getNumber()
		{
			return $this->number;
		}
		
		/**
     * Returns the weight associated with this vertex.
     *
     * @return mixed The weight associated with this vertex.
     */
		public function getWeight()
		{
			return $this->weight;
		}
		
		/**
     * Compares this vertex with a specified comparable object.
     * This method is not implemented yet.
     */
		protected function compareTo(IComparable $obj)
		{
			throw new MethodNotImplementedException();
		}
		
		/**
     * Returns a hashcode for this vertex.
     *
     * @return A hashcode for this vertex.
     */
		public function getHashCode()
		{
			$result = $this->number;
			if ($this->weight !== NULL) 
				$result += $weight->getHashCode();
			return $result;
		}
		
		/**
     * Returns a textual representation of this vertex.
     *
     * @return string A string.
     */
		public function __toString()
		{
			$s = '';
			$s .= 'Vertex{' . $this->number;
			if ($this->weight !== NULL) 
				$s .= ', weight = ' . $this->weight->__toString();
			$s .= '}';
			return $s;
		}
		
		/**
     * Returns the edges in this graph
     * that are incident upon this vertex.
     *
     * @return object IteratorAggregate The edges in this graph
     * that are incident upon this vertex.
     */
		public function getIncidentEdges()
		{
			return $this->graph->getIncidentEdges($this->number);
		}
		
		/**
     * Returns the edges in this graph
     * that emanate from this vertex.
     *
     * @return object Iterator Aggreate The edges in this graph
     * that emanate from this vertex.
     */
		public function getEmanatingEdges()
		{
			return $this->graph->getEmanatingEdges($this->number);
		}
		
		/**
     * Returns the vertices in this graph
     * which are predecessors of this vertex.
     *
     * @return object IteratorAggregate
     * The vertices in this graph which are predecessors of this vertex.
     */
		public function getPredecessors()
		{
			return new Vertex_IteratorAggregate($this, $this->getIncidentEdges());
		}
		
		/**
     * Returns the vertices in this graph
     * which are successors of this vertex.
     *
     * @return object IteratorAggregate
     * The vertices in this graph which are successors of this vertex.
     */
		public function getSuccessors()
		{
			return new Vertex_IteratorAggregate($this, $this->getEmanatingEdges());
		}
	}
	
	
	//{
	/**
 * A basic array class.
 *
 * @package Opus11
 */
	class BasicArray extends AbstractObject implements ArrayAccess
	{
		/**
     * The array data.
     */
		protected $data = NULL;
		/**
     * The length of the array.
     */
		protected $length = 0;
		/**
     * The base index of the array.
     */
		protected $baseIndex = 0;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a BasicArray.
     *
     * @param mixed $arg1 Either an integer or an array.
     * If an integer, this argument specifies the array length.
     * If an array, this array is initialized with contents of the given array.
     * @param integer $arg2 The base index. (Optional).
     */
		public function __construct($arg1 = 0, $baseIndex = 0)
		{
			parent::__construct();
			if (gettype($arg1) == 'integer') {
				$this->length = $arg1;
				$this->data = array();
				for($i = 0; $i < $this->length; ++$i) {
					$this->data[$i] = NULL;
				}
				$this->baseIndex = $baseIndex;
			} elseif (gettype($arg1) == 'array') {
				$this->length = sizeof($arg1);
				$this->data = array();
				for($i = 0; $i < $this->length; ++$i) {
					$this->data[$i] = $arg1[$i];
				}
				$this->baseIndex = $baseIndex;
			} else {
				throw new TypeError();
			}
		}
		//}>a
		
		
		
		
		//{
		/**
     * Returns a clone of this array.
     * @return object BasicArray A BasicArray.
     */
		public function __clone()
		{
			$result = new BasicArray($this->length, $this->baseIndex);
			for($i = 0; $i < $this->length; ++$i) {
				$result->data[$i] = $this->data[$i];
			}
			return $result;
		}
		//}>c
		
		//{
		/**
     * Returns true if the given index is valid.
     *
     * @param integer $index An index.
     * @return boolean True if the given index is valid.
     */
		public function offsetExists($index)
		{
			
			
			return $index >= $this->baseIndex && $index <= $this->baseIndex + $this->length;
		}
		
		/**
     * Returns the item in this array at the given index.
     *
     * @param integer $index An index.
     * @return mixed The item at the given index.
     */
		public function offsetGet($index)
		{
			if (!$this->offsetExists($index)) 
				throw new IndexError();
			return $this->data[$index - $this->baseIndex];
		}
		
		/**
     * Sets the item in this array at the given index to the given value.
     *
     * @param integer $index An index.
     * @param mixed $value A value.
     */
		public function offsetSet($index, $value)
		{
			if (!$this->offsetExists($index)) 
				throw new IndexError();
			$this->data[$index - $this->baseIndex] = $value;
		}
		
		/**
     * Unsets the item in this array at the given index.
     *
     * @param integer $index An index.
     */
		public function offsetUnset($index)
		{
			if (!$this->offsetExists($index)) 
				throw new IndexError();
			$this->data[$index - $this->baseIndex] = NULL;
		}
		//}>d
		
		//{
		/**
     * Data getter.
     *
     * @return array The data of this array.
     */
		public function &getData()
		{
			return $this->data;
		}
		
		/**
     * BaseIndex getter.
     *
     * @return integer The base index of this array.
     */
		public function getBaseIndex()
		{
			return $this->baseIndex;
		}
		
		/**
     * BaseIndex setter.
     *
     * @param integer $baseIndex A base index.
     */
		public function setBaseIndex($baseIndex)
		{
			$this->baseIndex = $baseIndex;
		}
		//}>e
		
		//{
		/**
     * Length getter.
     *
     * @return integer The length of this array.
     */
		public function getLength()
		{
			return $this->length;
		}
		
		/**
     * Length setter.
     *
     * @param integer $length A length.
     */
		public function setLength($length)
		{
			if ($this->length != $length) {
				$newData = array();
				$min = $this->length < $length ? $this->length : $length;
				for($i = 0; $i < $min; ++$i) {
					$newData[$i] = $this->data[$i];
				}
				for($i = $min + 1; $i < $length; ++$i) {
					$newData[$i] = NULL;
				}
				$this->data = $newData;
				$this->length = $length;
			}
		}
		//}>f
		
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this array.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this array.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial state.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		public function reduce(IReduceFunction $f, $initialState)
		{
			$acc = $initialState;
			for($i = 0; $i < length($this->data); $i++) 
				$acc = $f->invoke($acc, $this->data[$i]);
			
			return $acc;
			
			//return array_reduce($this->data, $callback, $initialState);
		}
		
		/**
     * Returns a textual representation of this array.
     *
     * @return string A string.
     */
		public function __toString()
		{
			$s = $this->reduce(new BasicArrayToStringReduceFunction(), array('', ''));
			//create_function(
			//    '$s, $item', 
			//    'return array($s[0] . $s[1] . str($item), ", ");'
			//), array('',''));
			return 'Array{baseIndex=' . $this->baseIndex . 
				', data=(' . $s[0] . ')}';
		}
	}
	
	class BasicArrayToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			return array($s[0] . $s[1] . $item->__toString(), ", ");
		}
	}
	
	//{
	/**
 * Interface implemented by all queues.
 *
 * @package Opus11
 */
	interface IQueue extends IContainer
	{
		/**
     * Enqueues the given object at the tail of this queue.
     *
     * @param object IObject $obj The object to enqueue.
     */
		function enqueue(IObject $obj);
		/**
     * Dequeues and returns the object at the head of this queue.
     *
     * @return object IObject The object at the head of this queue.
     */
		function dequeue();
		/**
     * Head getter.
     *
     * @return object IObject The object at the head of this queue.
     */
		function getHead();
	}
	//}>a
	
	//{
	/**
 * Abstract base class from which all boxed value classes are derived.
 *
 * @package Opus11
 */
	abstract class Box extends AbstractComparable
	{
		/**
     * $var mixed The boxed value.
     */
		protected $value = NULL;
		
		/**
     * Constructs a Box with the given value.
     *
     * @param mixed $value A value.
     */
		public function __construct($value)
		{
			parent::__construct();
			$this->value = $value;
		}
		
		
		/**
     * Value getter.
     *
     * @return mixed Return the value of this box.
     */
		public function getValue()
		{
			return $this->value;
		}
		
		/**
     * Returns a textual representation of the value in this box.
     *
     * @return string A string.
     */
		public function __toString()
		{
			return $this->value;
		}
	}
	
	//{
	/**
 * Represents a boolean value.
 *
 * @package Opus11
 */
	class BoxedBoolean extends Box
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a BoxedBoolean with the given boolean value.
     *
     * @param boolean $value A boolean value.
     */
		public function __construct($value)
		{
			parent::__construct($value ? true : false);
		}
		
		
		/**
     * Value setter.
     *
     * @param value A value.
     */
		public function setValue($value)
		{
			$this->value = $value ? true : false;
		}
		
		/**
     * Compares this BoxedBoolean with the given object.
     * This given object must be a BoxedBoolean.
     *
     * @param object IComparable $obj The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		protected function compareTo(IComparable $obj)
		{
			return ($this->value ? 1 : 0) - ($obj->value ? 1 : 0);
		}
		
		/**
     * Returns a textual representation of this BoxedBoolean.
     *
     * @return string A string.
     */
		public function __toString()
		{
			return $item ? 'true' : 'false';
		}
		//}>a
		
		//{
		/**
     * Returns a hash of the value of this BoxedBoolean.
     *
     * @return integer An integer.
     */
		public function getHashCode()
		{
			return $this->value ? 1 : 0;
		}
	}
	//}>b
	
	
	//if (realpath($argv[0]) == realpath(__FILE__))
	//{
	//    exit(BoxedBoolean::main(array_slice($argv, 1)));
	//}
	
	//{
	/**
 * Represents an integer.
 *
 * @package Opus11
 */
	class BoxedInteger extends Box
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a BoxedInteger with the given value.
     *
     * @param integer $value A value.
     */
		public function __construct($value)
		{
			parent::__construct(intval($value));
		}
		
		
		/**
     * Value setter.
     *
     * @param integer $value A value.
     */
		public function setValue($value)
		{
			$this->value = intval($value);
		}
		
		/**
     * Compares this object with the given object.
     * This object and the given object are instances of the same class.
     *
     * @param object IComparable $obj The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		protected function compareTo(IComparable $obj)
		{
			return $this->value - $obj->value;
		}
		//}>a
		
		//{
		/**
     * Returns a hash of the value of this BoxedInteger.
     *
     * @return integer An integer.
     */
		public function getHashCode()
		{
			return $this->value;
		}
	}
	//}>b
	
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(BoxedInteger::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * Represents a float value.
 *
 * @package Opus11
 */
	class BoxedFloat extends Box
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a BoxedFloat with the given value.
     *
     * @param float $value A value.
     */
		public function __construct($value)
		{
			parent::__construct(floatval($value));
		}
		
		
		/**
     * Value setter.
     *
     * @param float $value A value.
     */
		public function setValue($value)
		{
			$this->value = floatval($value);
		}
		
		/**
     * Compares this object with the given object.
     * This object and the given object are instances of the same class.
     *
     * @param object IComparable $obj The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		protected function compareTo(IComparable $obj)
		{
			return $this->value - $obj->value;
		}
		//}>a
		
		//{
		/**
     * Returns a hash of the value of this BoxedFloat.
     *
     * @return integer An integer.
     */
		public function getHashCode()
		{
			$abs = abs($this->value);
			$exponent = intval(log($abs, 2) + 1);
			$mantissa = $abs / pow(2, $exponent);
			$result = intval((2 * $mantissa - 1) * 2147483648);
			return $result;
		}
	}
	//}>b
	
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(BoxedFloat::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * Represents a string.
 *
 * @package Opus11
 */
	class BoxedString extends Box
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a BoxedString with the given value.
     *
     * @param string $value A value.
     */
		public function __construct($value)
		{
			parent::__construct(strval($value));
		}
		
		/**
     * Value setter.
     *
     * @param string $value A value.
     */
		public function setValue($value)
		{
			$this->value = strval($value);
		}
		
		/**
     * Compares this object with the given object.
     * This object and the given object are instances of the same class.
     *
     * @param object IComparable $obj The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		protected function compareTo(IComparable $obj)
		{
			return strcmp($this->value, $obj->value);
		}
		//}>a
		
		//{
		const SHIFT = 6;
		const MASK = 0xfc000000;
		
		public function getHashCode()
		{
			$result = 0;
			for($i = 0; $i < strlen($this->value); ++$i) {
				$result = ($result & self::MASK) ^ ($result << self::SHIFT) ^ ord(substr($this->value, $i, 1));
			}
			return $result;
		}
	}
	//}>b
	
	
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(BoxedString::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * Boxes the given value.
 *
 * @param mixed $value A value.
 * @return object Box A boxed value.
 *
function box($value)
{
    $type = gettype($value);
    if ($type == 'boolean')
    {
        return new BoxedBoolean($value);
    }
    elseif ($type == 'integer')
    {
        return new BoxedInteger($value);
    }
    elseif ($type == 'float' || $type == 'double')
    {
        return new BoxedFloat($value);
    }
    elseif ($type == 'string')
    {
        return new BoxedString($value);
    }
    elseif ($type == 'array')
    {
        return new BasicArray($value);
    }
    else
    {
        throw new TypeError();
    }
}
*/
	
	/**
 * Unboxes the given value.
 *
 * @param object Box box A boxed value.
 * @return mixed The value in the box.
 */
	function unbox($box)
	{
		return $box->getValue();
	}
	//}>b
	
	//if (realpath($argv[0]) == realpath(__FILE__))
	//{
	//exit(Box::main(array_slice($argv, 1)));
	//}
	
	//{
	/**
 * Abstract base class from which all queue classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractQueue extends AbstractContainer implements IQueue
	{
		
		//!    // ...
		//!}
		//}>a
		
		/**
     * Constructor.
     */
		public function __construct()
		{
			parent::__construct();
		}
	}
	
	
	
	//{
	/**
 * Represents an element of a linked list.
 *
 * @package Opus11
 */
	class LinkedList_Element
	{
		/**
     * @var object LinkedList The linked list to which this element belongs.
     */
		protected $list = NULL;
		/**
     * @var mixed The datum in this element.
     */
		protected $datum = NULL;
		/**
     * @var object LinkedList_Element The next list element.
     */
		protected $next = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a LinkedList_Element with the given values.
     *
     * @param mixed $list A linked list.
     * @param mixed $datum An item.
     * @param mixed $next The next element.
     */
		public function __construct($list, $datum, $next)
		{
			$this->list = $list;
			$this->datum = $datum;
			$this->next = $next;
		}
		
		
		/**
     * List getter.
     * 
     * @return object LinkedList The list of this element.
     */
		public function getList()
		{
			return $this->list;
		}
		
		/**
     * Datum getter.
     *
     * @return mixed The datum of this element.
     */
		public function getDatum()
		{
			return $this->datum;
		}
		
		/**
     * Next getter.
     *
     * @return mixed The next element of this element.
     */
		public function getNext()
		{
			return $this->next;
		}
		
		/**
     * Next setter.
     *
     * @param object LinkedList_Element $next The new next element.
     */
		public function setNext(LinkedList_Element $next)
		{
			if ($this->list !== $next->list) 
				throw new ArgumentError();
			$this->next = $next;
		}
		
		/**
     * Next unsetter.
     */
		public function unsetNext()
		{
			$this->next = NULL;
		}
		//}>a
		
		//{
		/**
     * Inserts the given item in the linked list after this element.
     *
     * @param mixed $item The item to insert.
     */
		public function insertAfter($item)
		{
			$this->next = new LinkedList_Element($this->list, $item, $this->next);
			if ($this->list->getTail() === $this) 
				$this->list->setTail($this->next);
		}
		
		/**
     * Inserts the given item in the linked list before this element.
     *
     * @param mixed $item The item to insert.
     */
		public function insertBefore($item)
		{
			$tmp = new LinkedList_Element($this->list, $item, $this);
			if ($this === $this->list->getHead()) {
				if ($tmp === NULL) 
					$list->unsetHead();
				 else 
					$list->setHead($tmp);
			} else {
				$prevPtr = $this->list->getHead();
				while ($prevPtr !== NULL && $prevPtr->next != $this) 
					$prevPtr = $prevPtr->next;
				$prevPtr->next = $tmp;
			}
		}
		//}>j
		
		/**
     * Extracts this list element from the linked list.
     */
		public function extract()
		{
			$prevPtr = NULL;
			if ($this->list->getHead() === $this) {
				if ($this->next === NULL) 
					$list->unsetHead();
				 else 
					$list->setHead($this->next);
			} else {
				$prevPtr = $this->list->getHead();
				while ($prevPtr !== NULL && $prevPtr->next != $this) 
					$prevPtr = $prevPtr->next;
				if (prevPtr === NULL) 
					throw new ArgumentError();
				$prevPtr->next = $this->next;
			}
			if ($this->list->getTail() === $this) {
				if ($prevPtr === NULL) 
					$list->unsetTail();
				 else 
					$list->setTail($prevPtr);
			}
		}
	}
	
	//{
	/**
 * Represents a linked list.
 *
 * @package Opus11
 */
	class LinkedList extends AbstractObject
	{
		/**
     * @var object LinkedList_Element
     * The element at the head of the linked list.
     */
		protected $head = NULL;
		/**
     * @var object LinkedList_Element
     * The element at the tail of the linked list.
     */
		protected $tail = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a LinkedList.
     */
		public function __construct()
		{
			parent::__construct();
			$this->head = NULL;
			$this->tail = NULL;
		}
		
		
		//}>b
		
		//{
		/**
     * Purges this linked list.
     */
		public function purge()
		{
			$this->head = NULL;
			$this->tail = NULL;
		}
		//}>c
		
		//{
		/**
     * Head getter.
     *
     * @return mixed The head element of this linked list.
     */
		public function getHead()
		{
			return $this->head;
		}
		
		/**
     * Head setter.
     *
     * @param object LinkedList_Element $element An element.
     */
		public function setHead(LinkedList_Element $element)
		{
			if ($element->getList() !== $this) 
				throw new ArgumentError();
			$this->head = $element;
		}
		
		/**
     * Head unsetter.
     */
		public function unsetHead()
		{
			$this->head = NULL;
		}
		
		/**
     * Tail getter.
     *
     * @return mixed The tail element of this linked list.
     */
		public function getTail()
		{
			return $this->tail;
		}
		
		/**
     * Tail setter.
     *
     * @param object LinkedList_Element $element An element.
     */
		public function setTail(LinkedList_Element $element)
		{
			if ($element->getList() !== $this) 
				throw new ArgumentError();
			$this->tail = $element;
		}
		
		/**
     * Tail unsetter.
     */
		public function unsetTail()
		{
			$this->tail = NULL;
		}
		
		/**
     * IsEmpty predicate.
     *
     * @return boolean True if this linked list is empty.
     */
		public function isEmpty()
		{
			return $this->head === NULL;
		}
		//}>d
		
		//{
		/**
     * First getter.
     *
     * @return mixed The first item in this linked list.
     */
		public function getFirst()
		{
			if ($this->head === NULL) 
				throw new ContainerEmptyException();
			return $this->head->getDatum();
		}
		
		/**
     * Last getter.
     *
     * @return mixed The last item in this linked list.
     */
		public function getLast()
		{
			if ($this->tail === NULL) 
				throw new ContainerEmptyException();
			return $this->tail->getDatum();
		}
		//}>e
		
		//{
		/**
     * Prepends the given item to this linked list.
     *
     * @param mixed $item The item to prepend.
     */
		public function prepend($item)
		{
			$tmp = new LinkedList_Element($this, $item, $this->head);
			if ($this->head === NULL) 
				$this->tail = $tmp;
			$this->head = $tmp;
		}
		//}f
		
		//{
		/**
     * Appends the given item to this linked list.
     *
     * @param mixed $item The item to append.
     */
		public function append($item)
		{
			$tmp = new LinkedList_Element($this, $item, NULL);
			if ($this->head === NULL) 
				$this->head = $tmp;
			 else 
				$this->tail->setNext($tmp);
			$this->tail = $tmp;
		}
		//}>g
		
		//{
		/**
     * Returns a clone of this linked list.
     *
     * @return object LinkedList A LinkedList.
     */
		public function __clone()
		{
			$result = new LinkedList();
			for($ptr = $this->head; $ptr !== NULL; $ptr = $ptr->getNext()) {
				$result->append($ptr->getDatum());
			}
			return $result;
		}
		//}>h
		
		//{
		/**
     * Extracts an item that equals the given item from this linked list.
     *
     * @param mixed $item The item to extract.
     */
		public function extract($item)
		{
			$ptr = $this->head;
			$prevPtr = NULL;
			while ($ptr !== NULL && $ptr->getDatum() !== $item) {
				$prevPtr = $ptr;
				$ptr = $ptr->getNext();
			}
			if ($ptr === NULL) 
				throw new ArgumentError();
			if ($ptr === $this->head) 
				$this->head = $ptr->getNext();
			 else {
				$tmp = $ptr->getNext();
				if ($tmp === NULL) 
					$prevPtr->unsetNext();
				 else 
					$prevPtr->setNext($ptr->getNext());
			}
			if ($ptr === $this->tail) 
				$this->tail = $prevPtr;
		}
		//}>i
		
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this container.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this container.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial state.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		public function reduce(IReduceFunction $f, $initialState)
		{
			$state = $initialState;
			$ptr = $this->head;
			while ($ptr !== NULL) {
				$state = $f->invoke($state, $ptr->getDatum());
				$ptr = $ptr->getNext();
			}
			return $state;
		}
		
		/**
     * Returns a textual representation of this linked list.
     *
     * @return string A string.
     */
		public function __toString()
		{
			$s = $this->reduce(new LinkedListToStringReduceFunction(), array('', ''));
			//create_function(
			// '$s, $item', 
			//  'return array($s[0] . $s[1] . str($item), ", ");'
			//), array('',''));
			return 'LinkedList{' . $s[0] . '}';
		}
	}
	
	
	class LinkedListToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			return array($s[0] . $s[1] . $item->__toString(), ", ");
		}
	}
	
	
	
	
	//{
	/**
 * An iterator that enumerates the items in a QueueAsLinkedList.
 *
 * @package Opus11
 */
	class QueueAsLinkedList_Iterator extends AbstractIterator
	{
		/**
     * @var object QueueAsLinkedList The queue to enumerate.
     */
		protected $queue = NULL;
		/**
     * @var object LinkedList_Element The current position.
     */
		protected $position = NULL;
		/**
     * @var integer The key for the current position.
     */
		protected $key = 0;
		
		//}@head
		
		//{
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a QueueAsLinkedList_Iterator for the given queue.
     *
     * @param object QueueAsLinkedList $queue A queue.
     */
		public function __construct(QueueAsLinkedList $queue)
		{
			parent::__construct();
			$this->queue = $queue;
			$this->position = $queue->getList()->getHead();
			$this->key = 0;
		}
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->position !== NULL;
		}
		
		/**
     * Key getter.
     *
     * @return integer The key at the current position of this iterator.
     */
		public function key()
		{
			return $this->key;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value at the current position of this iterator.
     */
		public function current()
		{
			return $this->position->getDatum();
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$this->position = $this->position->getNext();
			$this->key += 1;
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$this->position = $this->queue->getList()->getHead();
			$this->key = 0;
		}
	}
	//}>f
	
	//{
	/**
 * Represents a queue implemented using a linked list.
 *
 * @package Opus11
 */
	class QueueAsLinkedList extends AbstractQueue
	{
		/**
     * @var object LinkedList The linked list.
     */
		protected $list = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a QueueAsLinkedList.
     */
		public function __construct()
		{
			parent::__construct();
			$this->list = new LinkedList();
		}
		
		
		/**
     * List getter.
     *
     * @return object LinkedList The linked list of this queue.
     */
		public function getList()
		{
			return $this->list;
		}
		//}>a
		
		//{
		/**
     * Purges this queue.
     */
		public function purge()
		{
			$this->list->purge();
		}
		//}>b
		
		//{
		/**
     * Enqueues the given object at the tail of this queue.
     *
     * @param object IObject $obj The object to enqueue.
     */
		public function enqueue(IObject $obj)
		{
			$this->list->append($obj);
			$this->count += 1;
		}
		
		/**
     * Dequeues and returns the object at the head of this queue.
     *
     * @return object IObject The object at the head of this queue.
     */
		public function dequeue()
		{
			if ($this->count == 0) 
				throw new ContainerEmptyException();
			$result = $this->list->getFirst();
			$this->list->extract($result);
			$this->count -= 1;
			return $result;
		}
		
		/**
     * Head getter.
     *
     * @return object IObject The object at the head of this queue.
     */
		public function getHead()
		{
			if ($this->count == 0) 
				throw new ContainerEmptyException();
			return $this->list->getFirst();
		}
		//}>c
		
		//{
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this container.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this container.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial state.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		public function reduce(IReduceFunction $f, $initialState)
		{
			return $this->list->reduce($f, $initialState);
		}
		//}>d
		
		//{
		/**
     * Returns an iterator that enumerates the objects in this queue.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new QueueAsLinkedList_Iterator($this);
		}
		//}>e
		
		/**
     * Compares this object with the given object.
     * This object and the given object are instances of the same class.
     *
     * @param object IComparable $obj The given object.
     * @return integer A number less than zero
     * if this object is less than the given object,
     * zero if this object equals the given object, and
     * a number greater than zero
     * if this object is greater than the given object.
     */
		public function compareTo(IComparable $obj)
		{
			throw new MethodNotImplementedException();
		}
	}
	
	
	
	
	//{
	/**
 * Counting visitor.
 *
 * @package Opus11
 */
	class CountingVisitor extends AbstractVisitor
	{
		/**
     * @var integer The count.
     */
		protected $count = 0;
		
		/**
     * Constructs this CountingVisitor.
     *
     * @param integer $count The initial count.
     */
		public function __construct($count = 0)
		{
			parent::__construct();
			$this->count = 0;
		}
		
		/**
     * Count getter.
     *
     * @return integer The count.
     */
		public function getCount()
		{
			return $this->count;
		}
		
		/**
     * Count setter.
     *
     * @param count The count.
     */
		public function setCount($count)
		{
			$this->count = count;
		}
		
		/**
     * Counts the given object.
     *
     * @param object IObject $obj An object.
     */
		public function visit(IObject $obj)
		{
			++$this->count;
		}
	}
	//}>a
	/**
 * @copyright Copyright (c) 2005 by Bruno R. Preiss, P.Eng.
 *
 * @author $Author: brpreiss $
 * @version $Id: IPrePostVisitor.php,v 1.3 2005/11/27 23:32:32 brpreiss Exp $
 * @package Opus11
 */
	
	//{
	/**
 * Interface implemented by all pre/post visitors.
 *
 * @package Opus11
 */
	interface IPrePostVisitor
	{
		/**
     * "Pre"-visits the given object.
     *
     * @param object IObject $obj The object to visit.
     */
		function preVisit(IObject $obj);
		/**
     * "In"-visits the given object.
     *
     * @param object IObject $obj The object to visit.
     */
		function inVisit(IObject $obj);
		/**
     * "Post"-visits the given object.
     *
     * @param object IObject $obj The object to visit.
     */
		function postVisit(IObject $obj);
		/**
     * Done predicate.
     *
     * @return boolean True if this visitor is done.
     */
		function isDone();
	}
	//}>a
	
	//{
	/**
 * Abstract base class from which all pre/post visitor classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractPrePostVisitor implements IPrePostVisitor
	{
		/**
     * Constructs this AbstractPrePostVisitor.
     */
		public function __construct()
		{
		}
		
		/**
     * Default preVisit method does nothing.
     *
     * @param object IObject $obj An object.
     */
		public function preVisit(IObject $obj)
		{
		}
		
		/**
     * Default inVisit method does nothing.
     *
     * @param object IObject $obj An object.
     */
		public function inVisit(IObject $obj)
		{
		}
		
		/**
     * Default postVisit method does nothing.
     *
     * @param object IObject $obj An object.
     */
		public function postVisit(IObject $obj)
		{
		}
		
		/**
     * Done predicate.
     *
     * @return boolean False always.
     */
		public function isDone()
		{
			return false;
		}
	}
	//}>a
	
	//{
	/**
 * Adapter to convert a IVisitor to a IPrePostVisitor
 *
 * @package Opus11
 */
	class PreOrder extends AbstractPrePostVisitor
	{
		/**
     * @var object IVisitor A visitor.
     */
		protected $visitor = NULL;
		
		/**
     * Constructs this PreOrder.
     */
		public function __construct($visitor)
		{
			$this->visitor = $visitor;
		}
		
		/**
     * "Pre"-visit the given object.
     * Calls the visit method of the underlying visitor.
     *
     * @param object IObject $obj An object.
     */
		public function preVisit(IObject $obj)
		{
			$this->visitor->visit($obj);
		}
		
		/**
     * Done predicate.
     *
     * @return boolean True if this visitor is done.
     */
		public function isDone()
		{
			return $this->visitor->isDone();
		}
	}
	//}>a
	
	//{
	/**
 * Printing visitor.
 *
 * @package Opus11
 */
	class PrintingVisitor extends AbstractVisitor
	{
		/**
     * @var resource The output stream.
     */
		protected $stream = NULL;
		
		/**
     * Constructs this PrintingVisitor.
     */
		public function __construct($stream)
		{
			parent::__construct();
			$this->stream = $stream;
		}
		
		
		/**
     * Prints the given object.
     *
     * @param object IObject $obj An object.
     */
		public function visit(IObject $obj)
		{
			fprintf($this->stream, "%s\n", $obj->__toString());
		}
	}
	//}>a
	
	/**
 * Visitor used to implement the __toString method.
 *
 * @package Opus11
 */
	class AbstractGraph_ToStringVisitor extends AbstractVisitor
	{
		/**
     * The text.
     */
		protected $text = '';
		
		/**
     * Constructor.
     */
		public function __construct()
		{
			parent::__construct();
		}
		
		/**
     * Text getter.
     *
     * @return string The text.
     */
		public function getText()
		{
			return $this->text;
		}
		
		/**
     * Visits the given object (vertex).
     *
     * @param object IObject $obj An object (vertex).
     */
		public function visit(IObject $obj)
		{
			$this->text .= $obj->__toString() . "\n";
			$it = $obj->getEmanatingEdges()->getIterator();
			$it->rewind();
			while ($it->valid()) {
				$edge = $it->current();
				$this->text .= "    " . $edge->__toString() . "\n";
				$it->next();
			}
		}
	}
	
	/**
 * Iterator that enumerates the vertices of a graph.
 *
 * @package Opus11
 */
	class AbstractGraph_VertexIterator extends AbstractIterator
	{
		/**
     * @var object AbstractGraph The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current vertex.
     */
		protected $v = 0;
		
		/**
     * Constructs an AbstractGraph_VertexIterator for the given graph.
     *
     * @param object AbstractGraph $graph This graph.
     */
		public function __construct(AbstractGraph $graph)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->v = 0;
		}
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->v < $this->graph->getNumberOfVertices();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			return $this->graph->getVertex($this->v);
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			++$this->v;
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$this->v = 0;
		}
	}
	
	/**
 * Aggregate that represents the vertices of a graph.
 *
 * @package Opus11
 */
	class AbstractGraph_VertexAggregate implements IteratorAggregate
	{
		/**
     * @var object AbstractGraph The graph.
     */
		protected $graph = NULL;
		
		/**
     * Constructs an AbstractGraph_VertexAggregate for the given graph.
     *
     * @var object AbstractGraph The graph.
     */
		public function __construct(AbstractGraph $graph)
		{
			$this->graph = $graph;
		}
		
		/**
     * Returns an iterator that enumerates the vertices of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return $this->graph->getIterator();
		}
	}
	
	//{
	/**
 * Abstract base class from which all graph classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractGraph extends AbstractContainer implements IGraph
	{
		/**
     * @var integer The number of vertices in this graph.
     */
		protected $numberOfVertices = 0;
		/**
     * @var integer The number of edges in this graph.
     */
		protected $numberOfEdges = 0;
		/**
     * @var object BasicArray The vertices.
     */
		protected $vertex = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs an AbstractGraph with the specified size.
     *
     * @param integer $size The maximum number of vertices.
     */
		public function __construct($size)
		{
			parent::__construct();
			$this->numberOfVertices = 0;
			$this->numberOfEdges = 0;
			$this->vertex = new BasicArray($size);
		}
		
		/**
     * Returns the edges incident upon the specified vertex.
     *
     * @param integer $v The number of the specified vertex.
     * @return object IteratorAggregate
     * The edges incident upon the specified vertex.
     */
		public abstract function getIncidentEdges($v);
		
		/**
     * Returns the edges that emanate from the specified vertex.
     *
     * @param integer $v The number of the specified vertex.
     * @return object IteratorAggregate
     * The edges that emanate from the specified vertex.
     */
		public abstract function getEmanatingEdges($v);
		//}>a
		
		/**
     * Purges the vertices from this graph.
     */
		public function purge()
		{
			for($i = 0; $i < $this->numberOfVertices; ++$i) 
				$this->vertex->offsetSet($i, NULL);
			$this->numberOfVertices = 0;
			$this->numberOfEdges = 0;
		}
		
		/**
     * Inserts the given vertex into this graph.
     *
     * @param object Vertex $v The vertex to insert.
     */
		protected function insertVertex(Vertex $v)
		{
			if ($this->numberOfVertices == $this->vertex->getLength()) 
				throw new ContainerFullException();
			if ($v->getNumber() != $this->numberOfVertices) 
				throw new ArgumentError();
			$this->vertex->offsetSet($this->numberOfVertices, $v);
			++$this->numberOfVertices;
		}
		
		/**
     * Adds a vertex to this graph with the specified number and weight.
     *
     * @param integer $v The number of the vertex to be added.
     * @param mixed $weight The weight to be associated with this vertex.
     */
		public function addVertex($v, $weight = NULL)
		{
			$this->insertVertex(new Vertex($this, $v, $weight));
		}
		
		/**
     * Returns the vertex with the specified vertex number.
     *
     * @param integer $v The vertex number.
     * @return The vertex with the specified vertex number.
     */
		public function getVertex($v)
		{
			if ($v < 0 || $v >= $this->numberOfVertices) 
				throw new IndexError();
			return $this->vertex->offsetGet($v);
		}
		
		/**
     * Returns the vertices in this graph.
     *
     * @return object IteratorAggregate The vertices in this graph.
     */
		public function getVertices()
		{
			return new AbstractGraph_VertexAggregate($this);
		}
		
		/**
     * Inserts an edge into this graph.
     *
     * @param object Edge $edge The edge to be insert into this graph.
     */
		protected abstract function insertEdge(Edge $edge);
		
		/**
     * Tests whether this graph is a directed graph.
     * A graph is a directed graph
     * if it implements the <code>Digraph</code> interface.
     *
     * @return True if this graph is a directed graph.
     */
		public function isDirected()
		{
			return $this instanceof IDigraph;
		}
		
		/**
     * Adds an edge to this graph that connects the specified vertices
     * and has the specified weight.
     *
     * @param integer $v
     * The number of the vertex from which the edge emanates.
     * @param integer $w
     * The number of the vertex upon which the edge is incident.
     * @param mixed $weight The weight associated with the edge.
     */
		public function addEdge($v, $w, $weight = NULL)
		{
			$this->insertEdge(new Edge($this, $v, $w, $weight));
		}
		
		/**
     * Returns the number of vertices in this graph.
     *
     * @return integer The number of vertices in this graph.
     */
		public function getNumberOfVertices()
		{
			return $this->numberOfVertices;
		}
		
		/**
     * Returns the number of edges in this graph.
     *
     * @return integer The number of edges in this graph.
     */
		public function getNumberOfEdges()
		{
			return $this->numberOfEdges;
		}
		
		/**
     * Accepts a visitor and makes it visit the vertices in this graph.
     *
     * @param object IVisitor $visitor The visitor to accept.
     */
		public function accept(IVisitor $visitor)
		{
			for($v = 0; $v < $this->numberOfVertices; ++$v) {
				if ($visitor->isDone()) 
					break;
				$visitor->visit($this->vertex->offsetGet($v));
			}
		}
		
		/**
     * Returns a value computed by calling the given callback function
     * for each item in this container.
     * Each time the callback function will be called with two arguments:
     * The first argument is the next item in this container.
     * The first time the callback function is called,
     * the second argument is the given initial value.
     * On subsequent calls to the callback function,
     * the second argument is the result returned from
     * the previous call to the callback function.
     *
     * @param callback $callback A callback function.
     * @param mixed $initialState The initial state.
     * @return mixed The value returned by
     * the last call to the callback function.
     */
		public function reduce(IReduceFunction $f, $initialState)
		{
			return $this->vertex->reduce($f, $initialState);
		}
		
		//{
		/**
     * Causes a visitor to visit the vertices of this graph
     * in depth-first traversal order starting from a given vertex.
     * This method invokes the preVisit
     * and postVisit methods of the visitor
     * for each vertex in this graph.
     * The default implementation is recursive.
     * The default implementation never invokes
     * the inVisit method of the visitor.
     * The traversal continues as long as the isDone
     * method of the visitor returns false.
     *
     * @param object IPrePostVisitor $visitor The visitor to accept.
     * @param integer $start The vertex at which to start the traversal.
     */
		public function depthFirstTraversal(IPrePostVisitor $visitor, $start)
		{
			$visited = new BasicArray($this->numberOfVertices);
			for($v = 0; $v < $this->numberOfVertices; ++$v) 
				$visited->offsetSet($v, false);
			$this->doDepthFirstTraversal($visitor, $this->vertex->offsetGet($start), $visited);
		}
		
		/**
 * Recursive depth-first traversal method.
     *
     * @param object IPrePostVisitor $visitor The visitor to accept.
     * @param integer $start The vertex at which to start the traversal.
     * @param object BasicArray $visited
     * Used to keep track of the visited vertices.
     */
		private function doDepthFirstTraversal(IPrePostVisitor $visitor, $v, $visited)
		{
			if ($visitor->isDone()) 
				return;
			$visitor->preVisit($v);
			$visited->offsetSet($v->getNumber(), true);
			foreach ($v->getSuccessors() as $to) {
				if (!$visited->offsetGet($to->getNumber())) {
					$this->doDepthFirstTraversal($visitor, $to, $visited);
				}
			}
			$visitor->postVisit($v);
		}
		//}>b
		
		//{
		/**
     * Causes a visitor to visit the vertices of this directed graph
     * in breadth-first traversal order starting from a given vertex.
     * This method invokes the visit method of the visitor
     * for each vertex in this graph.
     * The default implementation is iterative and uses a queue
     * to keep track of the vertices to be visited.
     * The traversal continues as long as the isDone
     * method of the visitor returns false.
     *
     * @param object IVisitor $visitor The visitor to accept.
     * @param integer $start The vertex at which to start the traversal.
     */
		public function breadthFirstTraversal(IVisitor $visitor, $start)
		{
			$enqueued = new BasicArray($this->numberOfVertices);
			for($v = 0; $v < $this->numberOfVertices; ++$v) 
				$enqueued->offsetSet($v, false);
			
			$queue = new QueueAsLinkedList();
			
			$enqueued->offsetSet($start, true);
			$queue->enqueue($this->vertex->offsetGet($start));
			while (!$queue->isEmpty() && !$visitor->isDone()) {
				$v = $queue->dequeue();
				$visitor->visit($v);
				foreach ($v->getSuccessors() as $to) {
					if (!$enqueued->offsetGet($to->getNumber())) {
						$enqueued->offsetSet($to->getNumber(), true);
						$queue->enqueue($to);
					}
				}
			}
		}
		//}>c
		
		//{
		/**
     * Causes a visitor to visit the vertices of this graph
     * in topological order.
     * This method takes a visitor and,
     * as long as the IsDone method of that visitor returns false,
     * this method invokes the Visit method of the visitor
     * for each vertex in the graph.
     * The order in which the vertices are visited
     * is given by a topological sort of the vertices.
     *
     * @param object IVisitor $visitor The visitor to accept.
     */
		public function topologicalOrderTraversal(IVisitor $visitor)
		{
			$inDegree = new BasicArray($this->numberOfVertices);
			for($v = 0; $v < $this->numberOfVertices; ++$v) 
				$inDegree->offsetSet($v, 0);
			foreach ($this->getEdges() as $edge) {
				$to = $edge->getV1();
				$inDegree->offsetSet($to->getNumber(), $inDegree->offsetGet($to->getNumber()) + 1);
			}
			
			$queue = new QueueAsLinkedList();
			for($v = 0; $v < $this->numberOfVertices; ++$v) 
				if ($inDegree->offsetGet($v) == 0) 
					$queue->enqueue($this->vertex->offsetGet($v));
			while (!$queue->isEmpty() && !$visitor->isDone()) {
				$v = $queue->dequeue();
				$visitor->visit($v);
				foreach ($v->getSuccessors() as $to) {
					$inDegree->offsetSet($to->getNumber(), $inDegree->offsetGet($to->getNumber()) - 1);
					if ($inDegree->offsetGet($to->getNumber()) == 0) 
						$queue->enqueue($to);
				}
			}
		}
		
		/**
     * Returns a textual representation of this graph.
     *
     * @return string A string.
     */
		public function __toString()
		{
			$visitor = new AbstractGraph_ToStringVisitor();
			$this->accept($visitor);
			return $this->getClass()->getName() . 
				"{\n" . $visitor->getText() . "}";
		}
		
		//{
		/**
     * Tests whether this graph is connected.
     * The default implementation does a depth-first traversal
     * and counts the number of vertices visited.
     * If all the vertices are visited, the graph is connected.
     *
     * @return boolean True if this graph is connected; false otherwise.
     */
		public function isConnected()
		{
			$visitor = new CountingVisitor();
			$this->depthFirstTraversal(new PreOrder($visitor), 0);
			return $visitor->getCount() == $this->numberOfVertices;
		}
		//}>e
		
		//{
		/**
     * Tests whether this graph is strongly connected.
     * The default implementation does a depth-first traversal
     * starting at each vertex in this graph.
     * If every traversal visits all the vertices in this graph,
     * the graph is strongly connected.
     *
     * @return boolean 
     * True if this graph is strongly connected; false otherwise.
     */
		public function isStronglyConnected()
		{
			$visitor = new CountingVisitor();
			for($v = 0; $v < $this->numberOfVertices; ++$v) {
				$visitor->setCount(0);
				$this->depthFirstTraversal(new PreOrder($visitor), $v);
				if ($visitor->getCount() != $this->numberOfVertices) 
					return false;
			}
			return true;
		}
		//}>f
		
		//{
		/**
     * Tests whether this graph is cyclic.
     * The default implementation does a topological order traversal
     * and counts the number of vertices visited.
     * If every vertex in this graph is visited,
     * the graph is acyclic.
     *
     * @return True if this graph is cyclic; false otherwise.
     */
		public function isCyclic()
		{
			$visitor = new CountingVisitor();
			$this->topologicalOrderTraversal($visitor);
			return $visitor->getCount() != $this->numberOfVertices;
		}
		//}>g
		
		/**
     * Returns an iterator that enumerates the vertices of this graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new AbstractGraph_VertexIterator($this);
		}
	}
	
	
	/**
 * Iterator that enumerates the edges of a GraphAsLists.
 *
 * @package Opus11
 */
	class GraphAsLists_EdgeIterator extends AbstractIterator
	{
		/**
     * @var object GraphAsLists The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var object LinkedList_Element The current item.
     */
		protected $ptr = NULL;
		/**
     * @var integer The position in the adjacency list.
     */
		protected $pos = 0;
		
		/**
     * Constructs a GraphAsLists_EdgeIterator for the given graph.
     *
     * @param object GraphAsLists $graph The graph.
     */
		public function __construct(GraphAsLists $graph)
		{
			parent::__construct();
			$this->graph = $graph;
			$list = $this->graph->getAdjacencyList();
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				$this->ptr = $list->offsetGet($this->v)->getHead();
				$this->pos = 0;
				if ($this->ptr !== NULL) 
					break;
			}
		}
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->ptr !== NULL;
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->pos;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			return $this->ptr->getDatum();
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$this->ptr = $this->ptr->getNext();
			$this->pos += 1;
			if ($this->ptr !== NULL) 
				return;
			$list = $this->graph->getAdjacencyList();
			for(++$this->v; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				$this->ptr = $list->offsetGet($this->v)->getHead();
				$this->pos = 0;
				if ($this->ptr !== NULL) 
					break;
			}
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$list = $this->graph->getAdjacencyList();
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				$this->ptr = $list->offsetGet($this->v)->getHead();
				$this->pos = 0;
				if ($this->ptr !== NULL) 
					break;
			}
		}
	}
	
	/**
 * Aggregate that represents the edges of a GraphAsLists.
 *
 * @package Opus11
 */
	class GraphAsLists_EdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object GraphAsLists The graph.
     */
		protected $graph = NULL;
		
		/**
     * Constructs a GraphAsLists_EdgeAggregate for the given graph.
     *
     * @param object GraphAsLists $graph The graph.
     */
		public function __construct(GraphAsLists $graph)
		{
			$this->graph = $graph;
		}
		
		/**
     * Returns an iterator the enumerates the edges of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new GraphAsLists_EdgeIterator($this->graph);
		}
	}
	
	/**
 * Iterator that enumerates the emanating edges of a given vertex
 * in a GraphAsLists.
 *
 * @package Opus11
 */
	class GraphAsLists_EmanatingEdgeIterator extends AbstractIterator
	{
		/**
     * @var object GraphAsLists The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var object LinkedList_Element The current item.
     */
		protected $ptr = NULL;
		/**
     * @var integer The position in the adjacency list.
     */
		protected $pos = 0;
		
		/**
     * Constructs a GraphAsLists_EmanatingEdgeIterator
     * for the given graph and vertex.
     *
     * @param object GraphAsLists $graph The graph.
     * @param integer $v The vertex.
     */
		public function __construct(GraphAsLists $graph, $v)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->v = $v;
			$list = $this->graph->getAdjacencyList();
			$this->ptr = $list->offsetGet($this->v)->getHead();
			$this->pos = 0;
		}
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->ptr !== NULL;
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->pos;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			return $this->ptr->getDatum();
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$this->ptr = $this->ptr->getNext();
			$this->pos += 1;
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$list = $this->graph->getAdjacencyList();
			$this->ptr = $list->offsetGet($this->v)->getHead();
			$this->pos = 0;
		}
	}
	
	/**
 * Aggregate that represents the emanating edges
 * of a given vertex of a GraphAsLists.
 *
 * @package Opus11
 */
	class GraphAsLists_EmanatingEdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object GraphAsLists The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The vertex.
     */
		protected $v = 0;
		
		/**
     * Constructs a GraphAsLists_EmanatingEdgeAggregate
     * for the given graph and vertex.
     *
     * @param object GraphAsLists $graph The graph.
     * @param integer $v The vertex.
     */
		public function __construct(GraphAsLists $graph, $v)
		{
			$this->graph = $graph;
			$this->v = $v;
		}
		
		/**
     * Returns an iterator the enumerates the emanating edges
     * of the given vertex of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			//debug_print_backtrace(); exit;
			
			return new GraphAsLists_EmanatingEdgeIterator($this->graph, $this->v);
		}
	}
	
	//{
	/**
 * An undirected graph implemented using adjacency lists.
 *
 * @package Opus11
 */
	class GraphAsLists extends AbstractGraph
	{
		/**
     * @var object BasicArray The array of adjacency lists.
     */
		protected $adjacencyList = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a GraphAsLists with the specified size.
     *
     * @param size The maximum number of vertices.
     */
		public function __construct($size = 0)
		{
			parent::__construct($size);
			$this->adjacencyList = new BasicArray($size);
			for($i = 0; $i < $size; ++$i) 
				$this->adjacencyList->offsetSet($i, new LinkedList());
		}
		//}>a
		
		
		/**
     * Adjacency list array getter.
     */
		public function &getAdjacencyList()
		{
			return $this->adjacencyList;
		}
		
		/**
     * Purges this graph, making it the empty graph.
     */
		public function purge()
		{
			for($i = 0; $i < $this->numberOfVertices; ++$i) 
				$this->adjacencyList->offsetGet($i)->purge();
			parent::purge();
		}
		
		/**
     * Inserts the specified edge into this graph.
     *
     * @param object Edge $edge The edge to insert into this graph.
     */
		protected function insertEdge(Edge $edge)
		{
			$v = $edge->getV0()->getNumber();
			$this->adjacencyList->offsetGet($v)->append($edge);
			/*
        $w = $edge->getV1()->getNumber();
        $this->adjacencyList[$w]->append(
            new Edge($this, $w, $v, $edge->getWeight()));
        */
			++$this->numberOfEdges;
		}
		
		/**
     * Returns the edge in this graph that connects the specified vertices.
     *
     * @param integer $v A vertex number.
     * @param integer $w A vertex number.
     * @return object Edge
     * The edge in this graph that connects the specified vertices.
     */
		public function getEdge($v, $w)
		{
			if ($v < 0 || $v >= $this->numberOfVertices) 
				throw new IndexError();
			if ($w < 0 || $w >= $this->numberOfVertices) 
				throw new IndexError();
			for($ptr = $this->adjacencyList->offsetGet($v)->getHead(); $ptr !== NULL; $ptr = $ptr->getNext()) {
				$edge = $ptr->getDatum();
				if ($edge->getV1()->getNumber() == $w) 
					return $edge;
			}
			throw new ArgumentError();
		}
		
		/**
     * Tests whethere there is an edge in this graph
     * that connects the specified vertices.
     *
     * @param integer $v A vertex number.
     * @param integer $w A vertex number.
     * @return boolean True if there is an edge in this graph
     * that connects the specified vertices; false otherwise.
     */
		public function isEdge($v, $w)
		{
			if ($v < 0 || $v >= $this->numberOfVertices) 
				throw new IndexError();
			if ($w < 0 || $w >= $this->numberOfVertices) 
				throw new IndexError();
			for($ptr = $this->adjacencyList->offsetGet($v)->getHead(); $ptr !== NULL; $ptr = $ptr->getNext()) {
				$edge = $ptr->getDatum();
				if ($edge->getV1()->getNumber() == $w) 
					return true;
			}
			return false;
		}
		
		/**
     * Returns the edges in this graph.
     *
     * @return object IteratorAggregate
     * The edges in this graph.
     */
		public function getEdges()
		{
			return new GraphAsLists_EdgeAggregate($this);
		}
		
		/**
     * Returns the edges that emanate from the specified vertex.
     *
     * @param integer $v A vertex.
     * @return object IteratorAggregate
     * The edges that emanate from the specified vertex.
     */
		public function getEmanatingEdges($v)
		{
			return new GraphAsLists_EmanatingEdgeAggregate($this, $v);
		}
		
		/**
     * Returns the edges that are incident upon the specified vertex.
     *
     * @param integer $v A vertex.
     * @return object IteratorAggregate
     * The edges that are incident upon the specified vertex.
     */
		public function getIncidentEdges($w)
		{
			throw new MethodNotImplementedException();
		}
		
		/**
     * Compares this graph with the specified comparable object.
     * This method is not implemented.
     *
     * @param object IComparable $arg
     * The object with which to compare this graph.
     */
		protected function compareTo(IComparable $arg)
		{
			throw new MethodNotImplementedException();
		}
	}
	
	
	
	//{
	/**
 * Abstract base class from which all digraph classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractDigraph extends AbstractGraph implements IDigraph
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs an AbstractDigraph with the specified size.
     *
     * @param integer $size The maximum number of vertices.
     */
		public function __construct($size)
		{
			parent::__construct();
		}
	}
	
	
	
	
	//{
	/**
 * A directed graph implemented using adjacency lists.
 *
 * @package Opus11
 */
	class DigraphAsLists extends GraphAsLists implements IDigraph
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a DigraphAsLists with the specified size.
     *
     * @param size The maximum number of vertices.
     */
		public function __construct($size = 0)
		{
			parent::__construct($size);
		}
		//}>a
		
		/**
     * Inserts the specified edge into this graph.
     *
     * @param object Edge $edge The edge to insert into this graph.
     */
		protected function insertEdge(Edge $edge)
		{
			$v = $edge->getV0()->getNumber();
			$this->adjacencyList->offsetGet($v)->append($edge);
			++$this->numberOfEdges;
		}
	}
	
	
	//{
	/**
 * Interface implemented by all matrix classes.
 *
 * @package Opus11
 */
	interface IMatrix extends IObject, ArrayAccess
	{
		/**
     * Rows getter.
     *
     * @return integer The number of rows in this matrix.
     */
		function getNumRows();
		/**
     * Columns getter.
     *
     * @return integer The number of columns in this matrix.
     */
		function getNumCols();
		/**
     * Returns the transpose of this matrix.
     *
     * @return object IMatrix The transpose.
     */
		function getTranspose();
		/**
     * Returns the sum of this matrix and the given matrix.
     *
     * @param object IMatrix $matrix A matrix.
     * @return object IMatrix The sum.
     */
		function plus(IMatrix $matrix);
		/**
     * Returns the product of this matrix and the given matrix.
     *
     * @param object IMatrix $matrix A matrix.
     * @return object IMatrix The product.
     */
		function times(IMatrix $matrix);
	}
	//}>a
	
	//{
	/**
 * Abstract base class from which all matrix classes are derived.
 *
 * @package Opus11
 */
	abstract class AbstractMatrix extends AbstractObject implements IMatrix
	{
		/**
     * @var integer The number of rows.
     */
		protected $numRows = 0;
		/**
     * @var integer The number of columns.
     */
		protected $numCols = 0;
		
		//!    // ...
		//!}
		//}>a
		
		/**
     * Constructs an AbstractMatrix with the given number of rows and columns.
     *
     * @param integer $numRows The number of rows.
     * @param integer $numCols The number of columns.
     */
		public function __construct($numRows, $numCols)
		{
			parent::__construct();
			$this->numRows = $numRows;
			$this->numCols = $numCols;
		}
		
		/**
     * Rows getter.
     *
     * @return integer The number of rows.
     */
		public function getNumRows()
		{
			return $this->numRows;
		}
		
		/**
     * Columns getter.
     *
     * @return integer The number of columns.
     */
		public function getNumCols()
		{
			return $this->numCols;
		}
		
		/**
     * Returns a textual representation of this matrix.
     *
     * @return string A string.
     */
		public function __toString()
		{
			$s = '';
			for($i = 0; $i < $this->numRows; ++$i) {
				for($j = 0; $j < $this->numCols; ++$j) {
					$s .= str($this->offsetGet(array($i, $j))) . ' ';
				}
				$s .= "\n";
			}
			return $s;
		}
		
		/**
     * Matrix test program.
     *
     * @param object IMatrix $mat The matrix to test.
     */
		public static function test(IMatrix $mat)
		{
			printf("Matrix test program.\n");
			try {
				$k = 0;
				for($i = 0; $i < $mat->getNumRows(); ++$i) {
					for($j = 0; $j < $mat->getNumCols(); ++$j) {
						$mat[array($i, $j)] = $k++;
					}
				}
				printf("%s\n", str($mat));
				$mat = $mat->plus($mat);
				printf("%s\n", str($mat));
			} catch (Exception $e) {
				printf("Caught %s\n", $e->getMessage());
			}
		}
		
		/**
     * Matrix transpose test program.
     *
     * @param object IMatrix $mat The matrix to test.
     */
		public static function testTranspose(IMatrix $mat)
		{
			printf("Matrix transpose test program.\n");
			try {
				$mat[array(0, 0)] = 31;
				$mat[array(0, 2)] = 41;
				$mat[array(0, 3)] = 59;
				$mat[array(1, 1)] = 26;
				$mat[array(2, 3)] = 53;
				$mat[array(2, 4)] = 58;
				$mat[array(4, 2)] = 97;
				$mat[array(5, 1)] = 93;
				$mat[array(5, 5)] = 23;
				printf("%s\n", str($mat));
				$mat[array(2, 4)] = 0;
				$mat[array(5, 3)] = 0;
				$mat = $mat->getTranspose();
				printf("%s\n", str($mat));
			} catch (Exception $e) {
				printf("Caught %s\n", $e->getMessage());
			}
		}
		
		/**
     * Matrix multiply test program.
     *
     * @param object IMatrix $mat1 A matrix to test.
     * @param object IMatrix $mat2 A matrix to test.
     */
		public static function testTimes(IMatrix $mat1, IMatrix $mat2)
		{
			try {
				printf("Matrix multiply test program.\n");
				$mat1[array(0, 0)] = 1;
				$mat1[array(0, 1)] = 2;
				$mat1[array(0, 2)] = 3;
				$mat2[array(0, 0)] = 1;
				$mat2[array(1, 0)] = 2;
				$mat2[array(2, 0)] = 3;
				printf("%s\n", str($mat1));
				printf("%s\n", str($mat2));
				$mat1 = $mat2->times($mat1);
				printf("%s\n", str($mat1));
			} catch (Exception $e) {
				printf("Caught %s\n", $e->getMessage());
			}
		}
		
		/**
     * Main program.
     *
     * @param array $args Command-line arguments.
     * @return integer Zero on success; non-zero on failure.
     */
		public static function main($args)
		{
			printf("AbstractMatrix main program.\n");
			$status = 0;
			return $status;
		}
	}
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(AbstractMatrix::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * Represents a multi-dimensional array.
 *
 * @package Opus11
 */
	class MultiDimensionalArray extends AbstractObject implements ArrayAccess
	{
		/**
     * @var object BasicArray The dimensions of the array.
     */
		protected $dimensions = NULL;
		/**
     * @var object BasicArray
     * Used in the calculation that maps a set of indices
     * into a position in a one-dimensional array.
     */
		protected $factors = NULL;
		/**
     * @var object BasicArray
     * A one-dimensional array that holds the elements
     * of the multi-dimensional array.
     */
		protected $data = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a MultiDimensionalArray with the specified dimensions.
     *
     */
		public function __construct($dimensions)
		{
			parent::__construct();
			$length = sizeof($dimensions);
			$this->dimensions = new BasicArray($length);
			$this->factors = new BasicArray($length);
			$product = 1;
			for($i = $length - 1; $i >= 0; --$i) {
				$this->dimensions->offsetSet($i, $dimensions[$i]);
				$this->factors->offsetSet($i, $product);
				$product *= $this->dimensions->offsetGet($i);
			}
			$this->data = new BasicArray($product);
		}
		
		//}>a
		
		public function __toString()
		{
			return "MultiDimensionalArray";
		}
		
		//{
		/**
     * Maps a set of indices for the multi-dimensional array
     * into the corresponding position in the one-dimensional array.
     *
     * @param array $indices The set of indices.
     */
		private function getOffset($indices)
		{
			if (sizeof($indices) != $this->dimensions->getLength()) 
				throw new IndexError();
			$offset = 0;
			for($i = 0; $i < $this->dimensions->getLength(); ++$i) {
				if ($indices[$i] < 0 || $indices[$i] >= $this->dimensions->offsetGet($i)) 
					throw new IndexError();
				
				$offset += $this->factors->offsetGet($i) * $indices[$i];
			}
			return $offset;
		}
		
		/**
     * Returns true if the given set of indices is valid.
     *
     * @param array $indices A set of indices.
     * @return boolean True if the given set of indices is valid.
     */
		public function offsetExists($indices)
		{
			$this->getOffset($indices);
		}
		
		/**
     * Returns the item in this array at the given indices.
     *
     * @param array $indices A set of indices.
     * @return mixed The item at the given indices.
     */
		public function offsetGet($indices)
		{
			return $this->data->offsetGet($this->getOffset($indices));
		}
		
		/**
     * Sets the item in this array at the given indices to the given value.
     *
     * @param array $indices A set of indices.
     * @param mixed $value A value.
     */
		public function offsetSet($indices, $value)
		{
			$this->data->offsetSet($this->getOffset($indices), $value);
		}
		
		/**
     * Unsets the item in this array at the given indices.
     *
     * @param array $indices A set of indices.
     */
		public function offsetUnset($indices)
		{
			$this->data->offsetSet($this->getOffset($indices), NULL);
		}
	}
	//}>b
	
	// if (realpath($argv[0]) == realpath(__FILE__))
	// {
	//     exit(MultiDimensionalArray::main(array_slice($argv, 1)));
	// }
	
	//{
	/**
 * A dense matrix implemented using a multi-dimensional array with 2 dimensions.
 *
 * @package Opus11
 */
	class DenseMatrix extends AbstractMatrix
	{
		/**
     * @var object MultiDimensionalArray The multi-dimensional array.
     */
		protected $array = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a DenseMatrix with the given number of rows and columns.
     *
     * @param integer $rows The number of rows.
     * @param integer $columns The number of columns.
     */
		public function __construct($rows, $columns)
		{
			parent::__construct($rows, $columns);
			$this->array = new MultiDimensionalArray(array($rows, $columns));
		}
		
		//}>a
		
		//{
		/**
     * Returns true if the given indices are valid.
     *
     * @param array $indices A set of indices.
     * @return boolean True if the given indices are valid.
     */
		public function offsetExists($indices)
		{
			return $this->array->offsetExists($indices);
		}
		
		/**
     * Returns the item in this array at the given indices.
     *
     * @param array $indices A set of indices.
     * @return mixed The item at the given indices.
     */
		public function offsetGet($indices)
		{
			return $this->array->offsetGet($indices);
		}
		
		/**
     * Sets the item in this array at the given indices to the given value.
     *
     * @param array $indices A set of indices.
     * @param mixed $value A value.
     */
		public function offsetSet($indices, $value)
		{
			$this->array->offsetSet($indices, $value);
		}
		
		/**
     * Unsets the item in this array at the given indices.
     *
     * @param array $indices A set of indices.
     */
		public function offsetUnset($indices)
		{
			$this->array->offsetSet($indices, NULL);
		}
		//}>b
		
		//{
		/**
     * Returns the product of this dense matrix and the given dense matrix.
     *
     * @param object DenseMatrix $mat A dense matrix.
     * @return object DenseMatrix The product.
     */
		public function times(IMatrix $mat)
		{
			if (!($mat instanceof self) || $this->getNumCols() != $mat->getNumRows()) 
				throw new ArgumentError();
			$result = new DenseMatrix($this->getNumRows(), $mat->getNumCols());
			for($i = 0; $i < $this->getNumRows(); ++$i) {
				for($j = 0; $j < $mat->getNumCols(); ++$j) {
					$sum = 0;
					for($k = 0; $k < $this->getNumCols(); ++$k) {
						$sum += $this->offsetGet(array($i, $k)) * $mat->offsetGet(array($k, $j));
					}
					$result->offsetSet(array($i, $j), $sum);
				}
			}
			return $result;
		}
		//}>c
		
		/**
     * Returns the sum of this dense matrix and the given dense matrix.
     *
     * @param object DenseMatrix $mat A dense matrix.
     * @return object DenseMatrix The sum.
     */
		public function plus(IMatrix $mat)
		{
			if (!($mat instanceof self) || $this->getNumRows() != $mat->getNumRows() || $this->getNumCols() != $mat->getNumCols()) 
				throw new ArgumentError();
			$result = new DenseMatrix($this->getNumRows(), $this->getNumCols());
			for($i = 0; $i < $this->getNumRows(); ++$i) {
				for($j = 0; $j < $this->getNumCols(); ++$j) {
					$result->offsetSet(array($i, $j), $this->offsetGet(array($i, $j)) + $mat->offsetGet(array($i, $j)));
				}
			}
			return $result;
		}
		
		/**
     * Returns the transpose of this dense matrix.
     *
     * @return object DenseMatrix The tranpose.
     */
		public function getTranspose()
		{
			$result = new DenseMatrix($this->getNumCols(), $this->getNumRows());
			for($i = 0; $i < $this->getNumRows(); ++$i) {
				for($j = 0; $j < $this->getNumCols(); ++$j) {
					$result->offsetSet(array($j, $i), $this->offsetGet(array($i, $j)));
				}
			}
			return $result;
		}
	}
	
	/**
 * @copyright Copyright (c) 2005 by Bruno R. Preiss, P.Eng.
 *
 * @author $Author: brpreiss $
 * @version $Id: Limits.php,v 1.1 2005/12/05 00:37:56 brpreiss Exp $
 * @package Opus11
 */
	
	
	//{
	/**
 * Defines various constants.
 *
 * @package Opus11
 */
	class Limits
	{
		/**
     * @var integer Number of bits in an integer.
     */
		const INTBITS = 32;
		/**
     * @var integer Maximum integer value.
     */
		const MAXINT = 2147483647;
		/**
     * @var integer Minimum integer value.
     */
		const MININT = -2147483648;
	}
	//}>a
	
	/**
 * Iterator that enumerates the edges of a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_EdgeIterator extends AbstractIterator
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var integer The current column.
     */
		protected $w = 0;
		
		/**
     * Constructs a GraphAsMatrix_EdgeIterator for the given graph.
     *
     * @param object GraphAsMatrix $graph The graph.
     */
		public function __construct(GraphAsMatrix $graph)
		{
			parent::__construct();
			$this->graph = $graph;
			$matrix = $this->graph->getMatrix();
			$breakOuter = false;
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = $this->v + 1; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) {
						$breakOuter = true;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = false;
					break;
				}
			}
		}
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->v < $this->graph->getNumberOfVertices() && $this->w < $this->graph->getNumberOfVertices();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->w;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			$matrix = $this->graph->getMatrix();
			return $matrix->offsetGet(array($this->v, $this->w));
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$matrix = $this->graph->getMatrix();
			for(++$this->w; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					return;
			}
			for(++$this->v; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = $this->v + 1; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
						return;
				}
			}
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$matrix = $this->graph->getMatrix();
			$breakOuter = false;
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = $this->v + 1; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) {
						$breakOuter = true;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = false;
					break;
				}
			}
		}
	}
	
	/**
 * Aggregate that represents the edges of a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_EdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		
		/**
     * Constructs a GraphAsMatrix_EdgeAggregate for the given graph.
     *
     * @param object GraphAsMatrix $graph The graph.
     */
		public function __construct(GraphAsMatrix $graph)
		{
			$this->graph = $graph;
		}
		
		
		
		/**
     * Returns an iterator the enumerates the edges of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new GraphAsMatrix_EdgeIterator($this->graph);
		}
	}
	
	/**
 * Iterator that enumerates the emanating edges of a given vertex
 * in a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_EmanatingEdgeIterator extends AbstractIterator
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var integer The current column.
     */
		protected $w = 0;
		
		/**
     * Constructs a GraphAsMatrix_EmanatingEdgeIterator
     * for the given graph and vertex.
     *
     * @param object GraphAsMatrix $graph The graph.
     * @param integer $v The vertex.
     */
		public function __construct(GraphAsMatrix $graph, $v)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->v = $v;
			$matrix = $this->graph->getMatrix();
			for($this->w = 0; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
		
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->v < $this->graph->getNumberOfVertices() && $this->w < $this->graph->getNumberOfVertices();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->w;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			$matrix = $this->graph->getMatrix();
			return $matrix->offsetGet(array($this->v, $this->w));
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$matrix = $this->graph->getMatrix();
			for(++$this->w; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$matrix = $this->graph->getMatrix();
			for($this->w = 0; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
	}
	
	/**
 * Aggregate that represents the emanating edges
 * of a given vertex of a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_EmanatingEdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The vertex.
     */
		protected $v = 0;
		
		/**
     * Constructs a GraphAsMatrix_EmanatingEdgeAggregate
     * for the given graph and vertex.
     *
     * @param object GraphAsMatrix $graph The graph.
     * @param integer $v The vertex.
     */
		public function __construct(GraphAsMatrix $graph, $v)
		{
			$this->graph = $graph;
			$this->v = $v;
		}
		
		
		/**
     * Returns an iterator the enumerates the emanating edges
     * of the given vertex of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new GraphAsMatrix_EmanatingEdgeIterator($this->graph, $this->v);
		}
	}
	
	/**
 * Iterator that enumerates the incident edges of a given vertex
 * in a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_IncidentEdgeIterator extends AbstractIterator
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var integer The current column.
     */
		protected $w = 0;
		
		/**
     * Constructs a GraphAsMatrix_IncidentEdgeIterator
     * for the given graph and vertex.
     *
     * @param object GraphAsMatrix $graph The graph.
     * @param integer $w The vertex.
     */
		public function __construct(GraphAsMatrix $graph, $w)
		{
			parent::__construct();
			$this->graph = $graph;
			$this->w = $w;
			$matrix = $this->graph->getMatrix();
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
		
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->v < $this->graph->getNumberOfVertices() && $this->w < $this->graph->getNumberOfVertices();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->w;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			$matrix = $this->graph->getMatrix();
			return $matrix->offsetGet(array($this->v, $this->w));
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$matrix = $this->graph->getMatrix();
			for(++$this->v; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$matrix = $this->graph->getMatrix();
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					break;
			}
		}
	}
	
	/**
 * Aggregate that represents the incident edges
 * of a given vertex of a GraphAsMatrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix_IncidentEdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object GraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The vertex.
     */
		protected $w = 0;
		
		/**
     * Constructs a GraphAsMatrix_IncidentEdgeAggregate
     * for the given graph and vertex.
     *
     * @param object GraphAsMatrix $graph The graph.
     * @param integer $w The vertex.
     */
		public function __construct(GraphAsMatrix $graph, $w)
		{
			$this->graph = $graph;
			$this->w = $w;
		}
		
		/**
     * Returns an iterator the enumerates the emanating edges
     * of the given vertex of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new GraphAsMatrix_IncidentEdgeIterator($this->graph, $this->w);
		}
	}
	
	//{
	/**
 * An undirected graph implemented using an adjacency matrix.
 *
 * @package Opus11
 */
	class GraphAsMatrix extends AbstractGraph
	{
		/**
     * @var object DenseMatrix The adjacency matrix.
     */
		protected $matrix = NULL;
		
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a GraphAsMatrix with the specified size.
     *
     * @param size The maximum number of vertices.
     */
		public function __construct($size = 0)
		{
			parent::__construct($size);
			$this->matrix = new DenseMatrix($size, $size);
		}
		//}>a
		
		/**
     * Matrix getter.
     */
		public function &getMatrix()
		{
			return $this->matrix;
		}
		
		/**
     * Purges this graph, making it the empty graph.
     */
		public function purge()
		{
			for($i = 0; $i < $this->numberOfVertices; ++$i) 
				for($j = 0; $j < $this->numberOfVertices; ++$j) 
					$this->matrix->offsetSet(array($i, $j), NULL);
			
			parent::purge();
		}
		
		/**
     * Inserts the given edge into this graph.
     *
     * @param object Edge $edge The edge to insert.
     */
		protected function insertEdge(Edge $edge)
		{
			$v = $edge->getV0()->getNumber();
			$w = $edge->getV1()->getNumber();
			if ($this->matrix->offsetGet(array($v, $w)) !== NULL) 
				throw new ArgumentError();
			
			$this->matrix->offsetSet(array($v, $w), $edge);
			$this->matrix->offsetSet(array($w, $v), $edge);
			++$this->numberOfEdges;
		}
		
		/**
     * Returns the edge that connects the specified vertices.
     *
     * @param integer $v A vertex number.
     * @param integer $w A vertex number.
     * @return object Edge The edge that connects the specified vertices.
     */
		public function getEdge($v, $w)
		{
			if ($v < 0 || $v >= $this->numberOfVertices) 
				throw new IndexError();
			if ($w < 0 || $w >= $this->numberOfVertices) 
				throw new IndexError();
			if ($this->matrix->offsetGet(array($v, $w)) === NULL) 
				throw new ArgumentError();
			
			return $this->matrix->offsetGet(array($v, $w));
		}
		
		/**
     * Tests whether there is an edge in this graph
     * that connects the specified vertices.
     *
     * @param integer $v A vertex number.
     * @param integer $w A vertex number.
     * @return boolean True if there is an edge in this graph
     * that connects the specified vertices; false otherwise.
     */
		public function isEdge($v, $w)
		{
			if ($v < 0 || $v >= $this->numberOfVertices) 
				throw new IndexError();
			if ($w < 0 || $w >= $this->numberOfVertices) 
				throw new IndexError();
			return $this->matrix->offsetGet(array($v, $w)) !== NULL;
		}
		
		
		/**
     * Returns the edges in this graph.
     *
     * @return object IteratorAggregate
     * The edges in this graph.
     */
		public function getEdges()
		{
			return new GraphAsMatrix_EdgeAggregate($this);
		}
		
		/**
     * Returns the edges that emanate from the specified vertex.
     *
     * @param integer $v A vertex.
     * @return object IteratorAggregate
     * The edges that emanate from the specified vertex.
     */
		public function getEmanatingEdges($v)
		{
			return new GraphAsMatrix_EmanatingEdgeAggregate($this, $v);
		}
		
		/**
     * Returns the edges that are incident upon the specified vertex.
     *
     * @param integer $v A vertex.
     * @return object IteratorAggregate
     * The edges that are incident upon the specified vertex.
     */
		public function getIncidentEdges($w)
		{
			return new GraphAsMatrix_IncidentEdgeAggregate($this, $w);
		}
		
		/**
     * Compares this graph with the specified comparable object.
     * This method is not implemented.
     *
     * @param object IComparable $arg
     * The object with which to compare this graph.
     */
		protected function compareTo(IComparable $arg)
		{
			throw new MethodNotImplementedException();
		}
	}
	
	
	
	/**
 * Iterator that enumerates the edges of a DigraphAsMatrix.
 *
 * @package Opus11
 */
	class DigraphAsMatrix_EdgeIterator extends AbstractIterator
	{
		/**
     * @var object DigraphAsMatrix The graph.
     */
		protected $graph = NULL;
		/**
     * @var integer The current row.
     */
		protected $v = 0;
		/**
     * @var integer The current column.
     */
		protected $w = 0;
		
		/**
     * Constructs a DigraphAsMatrix_EdgeIterator for the given graph.
     *
     * @param object DigraphAsMatrix $graph The graph.
     */
		public function __construct(DigraphAsMatrix $graph)
		{
			parent::__construct();
			$this->graph = $graph;
			$matrix = $this->graph->getMatrix();
			$breakOuter = false;
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = 0; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) {
						$breakOuter = true;
						break;
					}
				}
				
				if ($breakOuter) {
					$breakOuter = false;
					break;
				}
			}
		}
		
		/**
     * Valid predicate.
     *
     * @return boolean True if the current position of this iterator is valid.
     */
		public function valid()
		{
			return $this->v < $this->graph->getNumberOfVertices() && $this->w < $this->graph->getNumberOfVertices();
		}
		
		/**
     * Key getter.
     *
     * @return integer The key for the current position of this iterator.
     */
		public function key()
		{
			return $this->v * $this->graph->getNumberOfVertices() + $this->w;
		}
		
		/**
     * Current getter.
     *
     * @return mixed The value for the current postion of this iterator.
     */
		public function current()
		{
			$matrix = $this->graph->getMatrix();
			return $matrix->offsetGet(array($this->v, $this->w));
		}
		
		/**
     * Advances this iterator to the next position.
     */
		public function next()
		{
			$matrix = $this->graph->getMatrix();
			for(++$this->w; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
				if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
					return;
			}
			for(++$this->v; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = 0; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) 
						return;
				}
			}
		}
		
		/**
     * Rewinds this iterator to the first position.
     */
		public function rewind()
		{
			$matrix = $this->graph->getMatrix();
			$breakOuter = false;
			for($this->v = 0; $this->v < $this->graph->getNumberOfVertices(); ++$this->v) {
				for($this->w = 0; $this->w < $this->graph->getNumberOfVertices(); ++$this->w) {
					if ($matrix->offsetGet(array($this->v, $this->w)) !== NULL) {
						$breakOuter = true;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = false;
					break;
				}
			}
		}
	}
	
	/**
 * Aggregate that represents the edges of a DigraphAsMatrix.
 *
 * @package Opus11
 */
	class DigraphAsMatrix_EdgeAggregate implements IteratorAggregate
	{
		/**
     * @var object DigraphAsMatrix The graph.
     */
		protected $graph = NULL;
		
		/**
     * Constructs a DigraphAsMatrix_EdgeAggregate for the given graph.
     *
     * @param object DigraphAsMatrix $graph The graph.
     */
		public function __construct(DigraphAsMatrix $graph)
		{
			$this->graph = $graph;
		}
		
		
		/**
     * Returns an iterator the enumerates the edges of the graph.
     *
     * @return object Iterator An iterator.
     */
		public function getIterator()
		{
			return new DigraphAsMatrix_EdgeIterator($this->graph);
		}
	}
	
	//{
	/**
 * A directed graph implemented using an adjacency matrix.
 *
 * @package Opus11
 */
	class DigraphAsMatrix extends GraphAsMatrix implements IDigraph
	{
		//}@head
		
		//{
		//!    // ...
		//!}
		//}@tail
		
		//{
		/**
     * Constructs a DigraphAsMatrix with the specified size.
     *
     * @param size The maximum number of vertices.
     */
		public function __construct($size = 0)
		{
			parent::__construct($size);
		}
		//}>a
		
		
		/**
     * Inserts the given edge into this graph.
     *
     * @param object Edge $edge The edge to insert.
     */
		protected function insertEdge(Edge $edge)
		{
			$v = $edge->getV0()->getNumber();
			$w = $edge->getV1()->getNumber();
			if ($this->matrix->offsetGet(array($v, $w)) !== NULL) 
				throw new ArgumentError();
			$this->matrix->offsetSet(array($v, $w), $edge);
			++$this->numberOfEdges;
		}
		
		/**
     * Returns the edges in this graph.
     *
     * @return object IteratorAggregate
     * The edges in this graph.
     */
		public function getEdges()
		{
			return new DigraphAsMatrix_EdgeAggregate($this);
		}
	}
	
	
	function floydsAlgorithm(IDigraph $g)
	{
		$n = $g->getNumberOfVertices();
		$distance = new DenseMatrix($n, $n);
		for($v = 0; $v < $n; ++$v) 
			for($w = 0; $w < $n; ++$w) 
				$distance->offsetSet(array($v, $w), Limits::MAXINT);
		
		$it = $g->getEdges()->getIterator();
		$it->rewind();
		while ($it->valid()) {
			$edge = $it->current();
			$wt = $edge->getWeight();
			$distance->offsetSet(array($edge->getV0()->getNumber(), $edge->getV1()->getNumber()), unbox($wt));
			$it->next();
		}
		
		for($i = 0; $i < $n; ++$i) 
			for($v = 0; $v < $n; ++$v) 
				for($w = 0; $w < $n; ++$w) 
					if ($distance->offsetGet(array($v, $i)) != Limits::MAXINT 
						&& $distance->offsetGet(array($i, $w)) != Limits::MAXINT) {
						$d = $distance->offsetGet(array($v, $i)) 
							+ $distance->offsetGet(array($i, $w));
						if ($distance->offsetGet(array($v, $w)) > $d) 
							$distance->offsetSet(array($v, $w), $d);
					}
		
		$result = new DigraphAsMatrix($n);
		for($v = 0; $v < $n; ++$v) 
			$result->addVertex($v);
		for($v = 0; $v < $n; ++$v) 
			for($w = 0; $w < $n; ++$w) 
				if ($distance->offsetGet(array($v, $w)) != limits::MAXINT) 
					$result->addEdge($v, $w, new BoxedInteger($distance->offsetGet(array($v, $w))));
		
		return $result;
	}
	
	/*

Adjacency Matrix:

  0 1 2 3 4 5
0 0 2 5 ∞ ∞ ∞
1 ∞ 0 7 1 ∞ 8
2 ∞ ∞ 0 4 ∞ ∞
3 ∞ ∞ ∞ 0 3 ∞
4 ∞ ∞ 2 ∞ 0 3
5 ∞ 5 ∞ 2 4 0

*/
	
	$g = new DigraphAsLists(32);
	$g->addVertex(0, new BoxedInteger(1));
	$g->addVertex(1, new BoxedInteger(1));
	$g->addVertex(2, new BoxedInteger(1));
	$g->addVertex(3, new BoxedInteger(1));
	$g->addVertex(4, new BoxedInteger(1));
	$g->addVertex(5, new BoxedInteger(1));
	
	// Identity
	$g->addEdge(0, 0, new BoxedInteger(0));
	$g->addEdge(1, 1, new BoxedInteger(0));
	$g->addEdge(2, 2, new BoxedInteger(0));
	$g->addEdge(3, 3, new BoxedInteger(0));
	$g->addEdge(4, 4, new BoxedInteger(0));
	$g->addEdge(5, 5, new BoxedInteger(0));
	
	// V0
	$g->addEdge(0, 1, new BoxedInteger(2));
	$g->addEdge(0, 2, new BoxedInteger(5));
	
	// V1
	$g->addEdge(1, 2, new BoxedInteger(7));
	$g->addEdge(1, 3, new BoxedInteger(1));
	$g->addEdge(1, 5, new BoxedInteger(8));
	
	// V2
	$g->addEdge(2, 3, new BoxedInteger(4));
	
	// V3
	$g->addEdge(3, 4, new BoxedInteger(3));
	
	// V4
	$g->addEdge(4, 2, new BoxedInteger(2));
	$g->addEdge(4, 5, new BoxedInteger(3));
	
	// V5
	$g->addEdge(5, 1, new BoxedInteger(5));
	$g->addEdge(5, 3, new BoxedInteger(2));
	$g->addEdge(5, 4, new BoxedInteger(4));
	
	
	/*

Result:

  0 1  2 3 4 5
0 0 2  5 3 6 9
1 ∞ 0  6 1 4 7
2 ∞ 15 0 4 7 10
3 ∞ 11 5 0 3 6
4 ∞ 8  2 5 0 3
5 ∞ 5  6 2 4 0

*/
	
	$res = floydsAlgorithm($g);
	echo $res->__toString();
	
?>