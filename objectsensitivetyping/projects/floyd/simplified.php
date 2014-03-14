<?php
	class ArgumentError extends Exception
	{
		public function __construct()
		{
			$TLE139 = __CLASS__;
			Exception::__construct($TLE139);
		}
	}
	class ContainerEmptyException extends Exception
	{
		public function __construct()
		{
			$TLE140 = __CLASS__;
			Exception::__construct($TLE140);
		}
	}
	class ContainerFullException extends Exception
	{
		public function __construct()
		{
			$TLE141 = __CLASS__;
			Exception::__construct($TLE141);
		}
	}
	class IndexError extends Exception
	{
		public function __construct()
		{
			$TLE142 = __CLASS__;
			Exception::__construct($TLE142);
		}
	}
	class MethodNotImplementedException extends Exception
	{
		public function __construct()
		{
			$TLE143 = __CLASS__;
			Exception::__construct($TLE143);
		}
	}
	class TypeError extends Exception
	{
		public function __construct()
		{
			$TLE144 = __CLASS__;
			Exception::__construct($TLE144);
		}
	}
	class StateError extends Exception
	{
		public function __construct()
		{
			$TLE145 = __CLASS__;
			Exception::__construct($TLE145);
		}
	}
	class IllegalOperationException extends Exception
	{
		public function __construct()
		{
			$TLE146 = __CLASS__;
			Exception::__construct($TLE146);
		}
	}
	interface IObject
	{
		function getId();
		function getHashCode();
		function getClass();
	}
	function hash_($item)
	{
		$type = gettype($item);
		$TLE147 = 'object';
		$TLE148 = ($type == $TLE147);
		if ($TLE148) {
			$TLE149 = $item->getHashCode();
			return $TLE149;
		} else {
			$TLE150 = 'NULL';
			$TLE151 = ($type == $TLE150);
			if ($TLE151) {
				$TLE152 = 0;
				return $TLE152;
			} else {
				$TLE153 = new ArgumentError();
				throw $TLE153;
			}
		}
	}
	abstract class AbstractObject implements IObject
	{
		public function __construct()
		{
		}
		public function getId()
		{
			$TLE154 = '/^Object id #(\d*)$/';
			$TLE155 = strval($this);
			preg_match($TLE154, $TLE155, $matches);
			$TLE156 = 1;
			$TSNNi1393 = $matches[$TLE156];
			$TLE157 = intval($TSNNi1393);
			return $TLE157;
		}
		public function getClass()
		{
			$TLE158 = get_class($this);
			$TLE159 = new ReflectionClass($TLE158);
			return $TLE159;
		}
		public function getHashCode()
		{
			$TLE160 = $this->getId();
			return $TLE160;
		}
		public abstract function __toString();
	}
	interface IComparable
	{
		function compare(IComparable $object);
		function eq(IComparable $object);
		function ne(IComparable $object);
		function lt(IComparable $object);
		function le(IComparable $object);
		function gt(IComparable $object);
		function ge(IComparable $object);
	}
	function eq($left, $right)
	{
		$TLE161 = gettype($left);
		$TLE162 = 'object';
		$TLE65 = ($TLE161 == $TLE162);
		if ($TLE65) {
			$TLE163 = gettype($right);
			$TLE164 = 'object';
			$TEF66 = ($TLE163 == $TLE164);
		} else {
			$TEF66 = $TLE65;
		}
		$TLE165 = (bool) $TEF66;
		if ($TLE165) {
			$TLE166 = $left->eq($right);
			return $TLE166;
		} else {
			$TLE167 = ($left == $right);
			return $TLE167;
		}
	}
	function ne($left, $right)
	{
		$TLE168 = gettype($left);
		$TLE169 = 'object';
		$TLE67 = ($TLE168 == $TLE169);
		if ($TLE67) {
			$TLE170 = gettype($right);
			$TLE171 = 'object';
			$TEF68 = ($TLE170 == $TLE171);
		} else {
			$TEF68 = $TLE67;
		}
		$TLE172 = (bool) $TEF68;
		if ($TLE172) {
			$TLE173 = $left->ne($right);
			return $TLE173;
		} else {
			$TLE174 = ($left != $right);
			return $TLE174;
		}
	}
	function gt($left, $right)
	{
		$TLE175 = gettype($left);
		$TLE176 = 'object';
		$TLE69 = ($TLE175 == $TLE176);
		if ($TLE69) {
			$TLE177 = gettype($right);
			$TLE178 = 'object';
			$TEF70 = ($TLE177 == $TLE178);
		} else {
			$TEF70 = $TLE69;
		}
		$TLE179 = (bool) $TEF70;
		if ($TLE179) {
			$TLE180 = $left->gt($right);
			return $TLE180;
		} else {
			$TLE181 = ($right < $left);
			return $TLE181;
		}
	}
	function ge($left, $right)
	{
		$TLE182 = gettype($left);
		$TLE183 = 'object';
		$TLE71 = ($TLE182 == $TLE183);
		if ($TLE71) {
			$TLE184 = gettype($right);
			$TLE185 = 'object';
			$TEF72 = ($TLE184 == $TLE185);
		} else {
			$TEF72 = $TLE71;
		}
		$TLE186 = (bool) $TEF72;
		if ($TLE186) {
			$TLE187 = $left->ge($right);
			return $TLE187;
		} else {
			$TLE188 = ($right <= $left);
			return $TLE188;
		}
	}
	function lt($left, $right)
	{
		$TLE189 = gettype($left);
		$TLE190 = 'object';
		$TLE73 = ($TLE189 == $TLE190);
		if ($TLE73) {
			$TLE191 = gettype($right);
			$TLE192 = 'object';
			$TEF74 = ($TLE191 == $TLE192);
		} else {
			$TEF74 = $TLE73;
		}
		$TLE193 = (bool) $TEF74;
		if ($TLE193) {
			$TLE194 = $left->lt($right);
			return $TLE194;
		} else {
			$TLE195 = ($left < $right);
			return $TLE195;
		}
	}
	function le($left, $right)
	{
		$TLE196 = gettype($left);
		$TLE197 = 'object';
		$TLE75 = ($TLE196 == $TLE197);
		if ($TLE75) {
			$TLE198 = gettype($right);
			$TLE199 = 'object';
			$TEF76 = ($TLE198 == $TLE199);
		} else {
			$TEF76 = $TLE75;
		}
		$TLE200 = (bool) $TEF76;
		if ($TLE200) {
			$TLE201 = $left->le($right);
			return $TLE201;
		} else {
			$TLE202 = ($left <= $right);
			return $TLE202;
		}
	}
	abstract class AbstractComparable extends AbstractObject implements IComparable
	{
		public function __construct()
		{
			AbstractObject::__construct();
		}
		public function eq(IComparable $object)
		{
			$TLE203 = $this->compare($object);
			$TLE204 = 0;
			$TLE205 = ($TLE203 == $TLE204);
			return $TLE205;
		}
		public function ne(IComparable $object)
		{
			$TLE206 = $this->compare($object);
			$TLE207 = 0;
			$TLE208 = ($TLE206 != $TLE207);
			return $TLE208;
		}
		public function lt(IComparable $object)
		{
			$TLE209 = $this->compare($object);
			$TLE210 = 0;
			$TLE211 = ($TLE209 < $TLE210);
			return $TLE211;
		}
		public function le(IComparable $object)
		{
			$TLE212 = $this->compare($object);
			$TLE213 = 0;
			$TLE214 = ($TLE212 <= $TLE213);
			return $TLE214;
		}
		public function gt(IComparable $object)
		{
			$TLE215 = $this->compare($object);
			$TLE216 = 0;
			$TLE217 = ($TLE216 < $TLE215);
			return $TLE217;
		}
		public function ge(IComparable $object)
		{
			$TLE218 = $this->compare($object);
			$TLE219 = 0;
			$TLE220 = ($TLE219 <= $TLE218);
			return $TLE220;
		}
		protected abstract function compareTo(IComparable $object);
		public function compare(IComparable $object)
		{
			$result = 0;
			$TLE221 = $this->getClass();
			$TLE222 = $object->getClass();
			$TLE223 = ($TLE221 == $TLE222);
			if ($TLE223) {
				$result = $this->compareTo($object);
			} else {
				$TLE224 = $this->getClass();
				$TLE225 = $TLE224->getName();
				$TLE226 = $object->getClass();
				$TLE227 = $TLE226->getName();
				$result = strcmp($TLE225, $TLE227);
			}
			return $result;
		}
		public static function main($args)
		{
			$TLE228 = "AbstractComparable main program.\n";
			printf($TLE228);
			$TLE229 = 0;
			return $TLE229;
		}
	}
	interface IVisitor
	{
		function visit(IObject $obj);
		function isDone();
	}
	interface IReduceFunction
	{
		function invoke($acc, $item);
	}
	interface IContainer extends IComparable, IteratorAggregate
	{
		function getCount();
		function isEmpty();
		function isFull();
		function purge();
		function reduce(IReduceFunction $f, $initialState);
		function accept(IVisitor $visitor);
	}
	abstract class AbstractContainer extends AbstractComparable implements IContainer
	{
		protected $count;
		public function __construct()
		{
			AbstractComparable::__construct();
			$TLE230 = 0;
			$TSNNt1394 = $TLE230;
			$this->count = $TSNNt1394;
		}
		public function purge()
		{
			$TLE231 = 0;
			$TSNNt1395 = $TLE231;
			$this->count = $TSNNt1395;
		}
		public function getCount()
		{
			$TSNNt1396 = $this->count;
			return $TSNNt1396;
		}
		public function isEmpty()
		{
			$TLE232 = $this->getCount();
			$TLE233 = 0;
			$TLE234 = ($TLE232 == $TLE233);
			return $TLE234;
		}
		public function isFull()
		{
			$TLE235 = False;
			return $TLE235;
		}
		public function reduce(IReduceFunction $f, $initialState)
		{
			$state = $initialState;
			foreach ($this as $obj) {
				$state = $f->invoke($state, $obj);
			}
			return $state;
		}
		public function accept(IVisitor $visitor)
		{
			foreach ($this as $obj) {
				$TLE236 = $visitor->isDone();
				if ($TLE236) {
					break;
				}
				$visitor->visit($obj);
			}
		}
		public function __toString()
		{
			$TLE237 = new AbstractContainerToStringReduceFunction();
			$TLE238 = '';
			$TLE239 = '';
			unset($TSa240);
			$TSa240 = (array) $TSa240;
			$TLE1251 = 0;
			$TSNNi1397 = $TLE238;
			$TSa240[$TLE1251] = $TSNNi1397;
			$TLE1252 = 1;
			$TSNNi1398 = $TLE239;
			$TSa240[$TLE1252] = $TSNNi1398;
			$s = $this->reduce($TLE237, $TSa240);
			$TLE241 = $this->getClass();
			$TLE242 = $TLE241->getName();
			$TLE243 = '{';
			$TLE244 = ($TLE242 . $TLE243);
			$TLE245 = 0;
			$TSNNi1399 = $s[$TLE245];
			$TLE246 = ($TLE244 . $TSNNi1399);
			$TLE247 = '}';
			$TLE248 = ($TLE246 . $TLE247);
			return $TLE248;
		}
		public function getHashCode()
		{
			$TLE249 = new AbstractContainerHashCodeReduceFunction();
			$TLE250 = 0;
			$TLE251 = $this->reduce($TLE249, $TLE250);
			return $TLE251;
		}
	}
	class AbstractContainerToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			$TLE252 = 0;
			$TLE253 = 1;
			$TSNNi1400 = $s[$TLE252];
			$TSNNi1401 = $s[$TLE253];
			$TLE254 = ($TSNNi1400 . $TSNNi1401);
			$TLE255 = $item->__toString();
			$TLE256 = ($TLE254 . $TLE255);
			$TLE257 = ", ";
			unset($TSa258);
			$TSa258 = (array) $TSa258;
			$TLE1253 = 0;
			$TSNNi1402 = $TLE256;
			$TSa258[$TLE1253] = $TSNNi1402;
			$TLE1254 = 1;
			$TSNNi1403 = $TLE257;
			$TSa258[$TLE1254] = $TSNNi1403;
			return $TSa258;
		}
	}
	class AbstractContainerHashCodeReduceFunction implements IReduceFunction
	{
		public function invoke($s, $obj)
		{
			$TLE259 = $obj->getHashCode();
			$TLE260 = ($s + $TLE259);
			return $TLE260;
		}
	}
	abstract class AbstractVisitor implements IVisitor
	{
		public function __construct()
		{
		}
		public function isDone()
		{
			$TLE261 = False;
			return $TLE261;
		}
	}
	interface IEdge extends IComparable
	{
		function getV0();
		function getV1();
		function getWeight();
		function isDirected();
		function getMate(IVertex $vertex);
	}
	interface IVertex extends IComparable
	{
		function getNumber();
		function getWeight();
		function getIncidentEdges();
		function getEmanatingEdges();
		function getPredecessors();
		function getSuccessors();
	}
	interface IGraph extends IContainer
	{
		function getNumberOfEdges();
		function getNumberOfVertices();
		function isDirected();
		function addVertex($v, $weight = NULL);
		function getVertex($v);
		function addEdge($v, $w, $weight = NULL);
		function getEdge($v, $w);
		function isEdge($v, $w);
		function isConnected();
		function isCyclic();
		function getVertices();
		function getEdges();
		function depthFirstTraversal(IPrePostVisitor $visitor, $start);
		function breadthFirstTraversal(IVisitor $visitor, $start);
	}
	interface IDigraph extends IGraph
	{
		function isStronglyConnected();
		function topologicalOrderTraversal(IVisitor $visitor);
	}
	class Edge extends AbstractComparable implements IEdge
	{
		protected $graph = NULL;
		protected $v0 = 0;
		protected $v1 = 0;
		protected $weight = NULL;
		public function __construct(AbstractGraph $graph, $v0, $v1, $weight)
		{
			AbstractComparable::__construct();
			$TSNNt1404 = $graph;
			$this->graph = $TSNNt1404;
			$TSNNt1405 = $v0;
			$this->v0 = $TSNNt1405;
			$TSNNt1406 = $v1;
			$this->v1 = $TSNNt1406;
			$TSNNt1407 = $weight;
			$this->weight = $TSNNt1407;
		}
		public function getV0()
		{
			$TSNNt1408 = $this->graph;
			$TSNNt1409 = $this->v0;
			$TLE262 = $TSNNt1408->getVertex($TSNNt1409);
			return $TLE262;
		}
		public function getV1()
		{
			$TSNNt1410 = $this->graph;
			$TSNNt1411 = $this->v1;
			$TLE263 = $TSNNt1410->getVertex($TSNNt1411);
			return $TLE263;
		}
		public function getWeight()
		{
			$TSNNt1412 = $this->weight;
			return $TSNNt1412;
		}
		public function getMate(IVertex $v)
		{
			$TLE264 = $v->getNumber();
			$TSNNt1413 = $this->v0;
			$TLE265 = ($TLE264 == $TSNNt1413);
			if ($TLE265) {
				$TLE266 = $this->getV1();
				return $TLE266;
			} else {
				$TLE267 = $v->getNumber();
				$TSNNt1414 = $this->v1;
				$TLE268 = ($TLE267 == $TSNNt1414);
				if ($TLE268) {
					$TLE269 = $this->getV0();
					return $TLE269;
				} else {
					$TLE270 = new ArgumentError();
					throw $TLE270;
				}
			}
		}
		public function isDirected()
		{
			$TSNNt1415 = $this->graph;
			$TLE271 = $TSNNt1415->isDirected();
			return $TLE271;
		}
		protected function compareTo(IComparable $obj)
		{
			$TLE272 = new MethodNotImplementedException();
			throw $TLE272;
		}
		public function getHashCode()
		{
			$TSNNt1416 = $this->graph;
			$TLE273 = $TSNNt1416->getNumberOfVertices();
			$TSNNt1417 = $this->v0;
			$TLE274 = ($TSNNt1417 * $TLE273);
			$result = ($TLE274 + $v1);
			$TLE275 = NULL;
			$TSNNt1418 = $this->weight;
			$TLE276 = ($TSNNt1418 !== $TLE275);
			if ($TLE276) {
				$TLE277 = $weight->getHashCode();
				$result = ($result + $TLE277);
			}
			return $result;
		}
		public function __toString()
		{
			$s = '';
			$TLE278 = "Edge{";
			$TSNNt1419 = $this->v0;
			$TLE279 = ($TLE278 . $TSNNt1419);
			$s = ($s . $TLE279);
			$TLE280 = $this->isDirected();
			if ($TLE280) {
				$TLE281 = '->';
				$TSNNt1420 = $this->v1;
				$TLE282 = ($TLE281 . $TSNNt1420);
				$s = ($s . $TLE282);
			} else {
				$TLE283 = '--';
				$TSNNt1421 = $this->v1;
				$TLE284 = ($TLE283 . $TSNNt1421);
				$s = ($s . $TLE284);
			}
			$TLE285 = NULL;
			$TSNNt1422 = $this->weight;
			$TLE286 = ($TSNNt1422 !== $TLE285);
			if ($TLE286) {
				$TLE287 = ', weight = ';
				$TSNNt1423 = $this->weight;
				$TLE288 = $TSNNt1423->__toString();
				$TLE289 = ($TLE287 . $TLE288);
				$s = ($s . $TLE289);
			}
			$TLE290 = '}';
			$s = ($s . $TLE290);
			return $s;
		}
	}
	interface IIterator extends Iterator
	{
		function succ();
	}
	abstract class AbstractIterator implements IIterator
	{
		public function __construct()
		{
		}
		public function succ()
		{
			$result = NULL;
			$TLE291 = $this->valid();
			if ($TLE291) {
				$result = $this->current();
				$this->next();
			}
			return $result;
		}
	}
	class Vertex_Iterator extends AbstractIterator
	{
		protected $vertex = NULL;
		protected $edgeIterator = NULL;
		public function __construct(Vertex $vertex, Iterator $edgeIterator)
		{
			AbstractIterator::__construct();
			$TSNNt1424 = $vertex;
			$this->vertex = $TSNNt1424;
			$TSNNt1425 = $edgeIterator;
			$this->edgeIterator = $TSNNt1425;
		}
		public function valid()
		{
			$TSNNt1426 = $this->edgeIterator;
			$TLE292 = $TSNNt1426->valid();
			return $TLE292;
		}
		public function key()
		{
			$TSNNt1427 = $this->edgeIterator;
			$TLE293 = $TSNNt1427->key();
			return $TLE293;
		}
		public function current()
		{
			$TSNNt1428 = $this->edgeIterator;
			$TLE294 = $TSNNt1428->current();
			$TSNNt1429 = $this->vertex;
			$TLE295 = $TLE294->getMate($TSNNt1429);
			return $TLE295;
		}
		public function next()
		{
			$TSNNt1430 = $this->edgeIterator;
			$TSNNt1430->next();
		}
		public function rewind()
		{
			$TSNNt1431 = $this->edgeIterator;
			$TSNNt1431->rewind();
		}
	}
	class Vertex_IteratorAggregate implements IteratorAggregate
	{
		protected $vertex = NULL;
		protected $edges = NULL;
		public function __construct(Vertex $vertex, IteratorAggregate $edges)
		{
			$TSNNt1432 = $vertex;
			$this->vertex = $TSNNt1432;
			$TSNNt1433 = $edges;
			$this->edges = $TSNNt1433;
		}
		public function getIterator()
		{
			$TSNNt1434 = $this->edges;
			$TLE296 = $TSNNt1434->getIterator();
			$TSNNt1435 = $this->vertex;
			$TLE297 = new Vertex_Iterator($TSNNt1435, $TLE296);
			return $TLE297;
		}
	}
	class Vertex extends AbstractComparable implements IVertex
	{
		protected $graph = NULL;
		protected $number = 0;
		protected $weight = NULL;
		public function __construct(AbstractGraph $graph, $number, $weight = NULL)
		{
			AbstractComparable::__construct();
			$TSNNt1436 = $graph;
			$this->graph = $TSNNt1436;
			$TSNNt1437 = $number;
			$this->number = $TSNNt1437;
			$TSNNt1438 = $weight;
			$this->weight = $TSNNt1438;
		}
		public function getNumber()
		{
			$TSNNt1439 = $this->number;
			return $TSNNt1439;
		}
		public function getWeight()
		{
			$TSNNt1440 = $this->weight;
			return $TSNNt1440;
		}
		protected function compareTo(IComparable $obj)
		{
			$TLE298 = new MethodNotImplementedException();
			throw $TLE298;
		}
		public function getHashCode()
		{
			$TSNNt1441 = $this->number;
			$result = $TSNNt1441;
			$TLE299 = NULL;
			$TSNNt1442 = $this->weight;
			$TLE300 = ($TSNNt1442 !== $TLE299);
			if ($TLE300) {
				$TLE301 = $weight->getHashCode();
				$result = ($result + $TLE301);
			}
			return $result;
		}
		public function __toString()
		{
			$s = '';
			$TLE302 = 'Vertex{';
			$TSNNt1443 = $this->number;
			$TLE303 = ($TLE302 . $TSNNt1443);
			$s = ($s . $TLE303);
			$TLE304 = NULL;
			$TSNNt1444 = $this->weight;
			$TLE305 = ($TSNNt1444 !== $TLE304);
			if ($TLE305) {
				$TLE306 = ', weight = ';
				$TSNNt1445 = $this->weight;
				$TLE307 = $TSNNt1445->__toString();
				$TLE308 = ($TLE306 . $TLE307);
				$s = ($s . $TLE308);
			}
			$TLE309 = '}';
			$s = ($s . $TLE309);
			return $s;
		}
		public function getIncidentEdges()
		{
			$TSNNt1446 = $this->graph;
			$TSNNt1447 = $this->number;
			$TLE310 = $TSNNt1446->getIncidentEdges($TSNNt1447);
			return $TLE310;
		}
		public function getEmanatingEdges()
		{
			$TSNNt1448 = $this->graph;
			$TSNNt1449 = $this->number;
			$TLE311 = $TSNNt1448->getEmanatingEdges($TSNNt1449);
			return $TLE311;
		}
		public function getPredecessors()
		{
			$TLE312 = $this->getIncidentEdges();
			$TLE313 = new Vertex_IteratorAggregate($this, $TLE312);
			return $TLE313;
		}
		public function getSuccessors()
		{
			$TLE314 = $this->getEmanatingEdges();
			$TLE315 = new Vertex_IteratorAggregate($this, $TLE314);
			return $TLE315;
		}
	}
	class BasicArray extends AbstractObject implements ArrayAccess
	{
		protected $data = NULL;
		protected $length = 0;
		protected $baseIndex = 0;
		public function __construct($arg1 = 0, $baseIndex = 0)
		{
			AbstractObject::__construct();
			$TLE316 = gettype($arg1);
			$TLE317 = 'integer';
			$TLE318 = ($TLE316 == $TLE317);
			if ($TLE318) {
				$TSNNt1450 = $arg1;
				$this->length = $TSNNt1450;
				unset($TSa319);
				$TSa319 = (array) $TSa319;
				$TSNNt1451 = $TSa319;
				$this->data = $TSNNt1451;
				$i = 0;
				$ElcfPF0 = True;
				while (True) {
					if ($ElcfPF0) {
						$ElcfPF0 = False;
					} else {
						++$i;
					}
					$TSNNt1452 = $this->length;
					$TLE320 = ($i < $TSNNt1452);
					if ($TLE320) {
					} else {
						break;
					}
					$TLE321 = NULL;
					$TSNNt1453 = $this->data;
					$TSNNt1453[$i] = $TLE321;
					$this->data = $TSNNt1453;
				}
				$TSNNt1454 = $baseIndex;
				$this->baseIndex = $TSNNt1454;
			} else {
				$TLE322 = gettype($arg1);
				$TLE323 = 'array';
				$TLE324 = ($TLE322 == $TLE323);
				if ($TLE324) {
					$TLE325 = sizeof($arg1);
					$TSNNt1455 = $TLE325;
					$this->length = $TSNNt1455;
					unset($TSa326);
					$TSa326 = (array) $TSa326;
					$TSNNt1456 = $TSa326;
					$this->data = $TSNNt1456;
					$i = 0;
					$ElcfPF1 = True;
					while (True) {
						if ($ElcfPF1) {
							$ElcfPF1 = False;
						} else {
							++$i;
						}
						$TSNNt1457 = $this->length;
						$TLE327 = ($i < $TSNNt1457);
						if ($TLE327) {
						} else {
							break;
						}
						$TSNNt1458 = $this->data;
						$TSNNi1459 = $arg1[$i];
						$TSNNt1458[$i] = $TSNNi1459;
						$this->data = $TSNNt1458;
					}
					$TSNNt1460 = $baseIndex;
					$this->baseIndex = $TSNNt1460;
				} else {
					$TLE328 = new TypeError();
					throw $TLE328;
				}
			}
		}
		public function __clone()
		{
			$TSNNt1461 = $this->length;
			$TSNNt1462 = $this->baseIndex;
			$result = new BasicArray($TSNNt1461, $TSNNt1462);
			$i = 0;
			$ElcfPF2 = True;
			while (True) {
				if ($ElcfPF2) {
					$ElcfPF2 = False;
				} else {
					++$i;
				}
				$TSNNt1463 = $this->length;
				$TLE329 = ($i < $TSNNt1463);
				if ($TLE329) {
				} else {
					break;
				}
				$TSNNt1464 = $result->data;
				$TSNNt1465 = $this->data;
				$TSNNi1466 = $TSNNt1465[$i];
				$TSNNt1464[$i] = $TSNNi1466;
				$TSNNt1465[$i] = $TSNNi1466;
				$this->data = $TSNNt1465;
				$result->data = $TSNNt1464;
			}
			return $result;
		}
		public function offsetExists($index)
		{
			$TSNNt1467 = $this->baseIndex;
			$TLE77 = ($TSNNt1467 <= $index);
			if ($TLE77) {
				$TSNNt1468 = $this->baseIndex;
				$TSNNt1469 = $this->length;
				$TLE330 = ($TSNNt1468 + $TSNNt1469);
				$TEF78 = ($index <= $TLE330);
			} else {
				$TEF78 = $TLE77;
			}
			$TLE331 = (bool) $TEF78;
			return $TLE331;
		}
		public function offsetGet($index)
		{
			$TLE332 = $this->offsetExists($index);
			$TLE333 = !$TLE332;
			if ($TLE333) {
				$TLE334 = new IndexError();
				throw $TLE334;
			}
			$TSNNt1470 = $this->baseIndex;
			$TLE335 = ($index - $TSNNt1470);
			$TSNNt1471 = $this->data;
			$TSNNi1472 = $TSNNt1471[$TLE335];
			return $TSNNi1472;
		}
		public function offsetSet($index, $value)
		{
			$TLE336 = $this->offsetExists($index);
			$TLE337 = !$TLE336;
			if ($TLE337) {
				$TLE338 = new IndexError();
				throw $TLE338;
			}
			$TSNNt1473 = $this->baseIndex;
			$TLE339 = ($index - $TSNNt1473);
			$TSNNt1474 = $this->data;
			$TSNNt1474[$TLE339] = $value;
			$this->data = $TSNNt1474;
		}
		public function offsetUnset($index)
		{
			$TLE340 = $this->offsetExists($index);
			$TLE341 = !$TLE340;
			if ($TLE341) {
				$TLE342 = new IndexError();
				throw $TLE342;
			}
			$TSNNt1475 = $this->baseIndex;
			$TLE343 = ($index - $TSNNt1475);
			$TLE344 = NULL;
			$TSNNt1476 = $this->data;
			$TSNNt1476[$TLE343] = $TLE344;
			$this->data = $TSNNt1476;
		}
		public function &getData()
		{
			$TSNNt1477 = $this->data;
			return $TSNNt1477;
		}
		public function getBaseIndex()
		{
			$TSNNt1478 = $this->baseIndex;
			return $TSNNt1478;
		}
		public function setBaseIndex($baseIndex)
		{
			$TSNNt1479 = $baseIndex;
			$this->baseIndex = $TSNNt1479;
		}
		public function getLength()
		{
			$TSNNt1480 = $this->length;
			return $TSNNt1480;
		}
		public function setLength($length)
		{
			$TSNNt1481 = $this->length;
			$TLE345 = ($TSNNt1481 != $length);
			if ($TLE345) {
				unset($TSa346);
				$TSa346 = (array) $TSa346;
				$newData = $TSa346;
				$TSNNt1482 = $this->length;
				$TLE347 = ($TSNNt1482 < $length);
				if ($TLE347) {
					$TSNNt1483 = $this->length;
					$TEF79 = $TSNNt1483;
				} else {
					$TEF79 = $length;
				}
				$min = $TEF79;
				$i = 0;
				$ElcfPF3 = True;
				while (True) {
					if ($ElcfPF3) {
						$ElcfPF3 = False;
					} else {
						++$i;
					}
					$TLE348 = ($i < $min);
					if ($TLE348) {
					} else {
						break;
					}
					$TSNNt1484 = $this->data;
					$TSNNi1485 = $TSNNt1484[$i];
					$newData[$i] = $TSNNi1485;
					$TSNNt1484[$i] = $TSNNi1485;
					$this->data = $TSNNt1484;
				}
				$TLE349 = 1;
				$i = ($min + $TLE349);
				$ElcfPF4 = True;
				while (True) {
					if ($ElcfPF4) {
						$ElcfPF4 = False;
					} else {
						++$i;
					}
					$TLE350 = ($i < $length);
					if ($TLE350) {
					} else {
						break;
					}
					$TLE351 = NULL;
					$newData[$i] = $TLE351;
				}
				$TSNNt1486 = $newData;
				$this->data = $TSNNt1486;
				$TSNNt1487 = $length;
				$this->length = $TSNNt1487;
			}
		}
		public function reduce(IReduceFunction $f, $initialState)
		{
			$acc = $initialState;
			$i = 0;
			$ElcfPF5 = True;
			while (True) {
				if ($ElcfPF5) {
					$ElcfPF5 = False;
				} else {
					++$i;
				}
				$TSNNt1488 = $this->data;
				$TLE352 = length($TSNNt1488);
				$TLE353 = ($i < $TLE352);
				if ($TLE353) {
				} else {
					break;
				}
				$TSNNt1489 = $this->data;
				$TSNNi1490 = $TSNNt1489[$i];
				$acc = $f->invoke($acc, $TSNNi1490);
				$TSNNt1489[$i] = $TSNNi1490;
				$this->data = $TSNNt1489;
			}
			return $acc;
		}
		public function __toString()
		{
			$TLE354 = new BasicArrayToStringReduceFunction();
			$TLE355 = '';
			$TLE356 = '';
			unset($TSa357);
			$TSa357 = (array) $TSa357;
			$TLE1255 = 0;
			$TSNNi1491 = $TLE355;
			$TSa357[$TLE1255] = $TSNNi1491;
			$TLE1256 = 1;
			$TSNNi1492 = $TLE356;
			$TSa357[$TLE1256] = $TSNNi1492;
			$s = $this->reduce($TLE354, $TSa357);
			$TLE358 = 'Array{baseIndex=';
			$TSNNt1493 = $this->baseIndex;
			$TLE359 = ($TLE358 . $TSNNt1493);
			$TLE360 = ', data=(';
			$TLE361 = ($TLE359 . $TLE360);
			$TLE362 = 0;
			$TSNNi1494 = $s[$TLE362];
			$TLE363 = ($TLE361 . $TSNNi1494);
			$TLE364 = ')}';
			$TLE365 = ($TLE363 . $TLE364);
			return $TLE365;
		}
	}
	class BasicArrayToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			$TLE366 = 0;
			$TLE367 = 1;
			$TSNNi1495 = $s[$TLE366];
			$TSNNi1496 = $s[$TLE367];
			$TLE368 = ($TSNNi1495 . $TSNNi1496);
			$TLE369 = $item->__toString();
			$TLE370 = ($TLE368 . $TLE369);
			$TLE371 = ", ";
			unset($TSa372);
			$TSa372 = (array) $TSa372;
			$TLE1257 = 0;
			$TSNNi1497 = $TLE370;
			$TSa372[$TLE1257] = $TSNNi1497;
			$TLE1258 = 1;
			$TSNNi1498 = $TLE371;
			$TSa372[$TLE1258] = $TSNNi1498;
			return $TSa372;
		}
	}
	interface IQueue extends IContainer
	{
		function enqueue(IObject $obj);
		function dequeue();
		function getHead();
	}
	abstract class Box extends AbstractComparable
	{
		protected $value = NULL;
		public function __construct($value)
		{
			AbstractComparable::__construct();
			$TSNNt1499 = $value;
			$this->value = $TSNNt1499;
		}
		public function getValue()
		{
			$TSNNt1500 = $this->value;
			return $TSNNt1500;
		}
		public function __toString()
		{
			$TSNNt1501 = $this->value;
			return $TSNNt1501;
		}
	}
	class BoxedBoolean extends Box
	{
		public function __construct($value)
		{
			if ($value) {
				$TEF80 = True;
			} else {
				$TEF80 = False;
			}
			Box::__construct($TEF80);
		}
		public function setValue($value)
		{
			if ($value) {
				$TEF81 = True;
			} else {
				$TEF81 = False;
			}
			$TSNNt1502 = $TEF81;
			$this->value = $TSNNt1502;
		}
		protected function compareTo(IComparable $obj)
		{
			$TSNNt1503 = $this->value;
			if ($TSNNt1503) {
				$TEF82 = 1;
			} else {
				$TEF82 = 0;
			}
			$TSNNt1504 = $obj->value;
			if ($TSNNt1504) {
				$TEF83 = 1;
			} else {
				$TEF83 = 0;
			}
			$TLE373 = ($TEF82 - $TEF83);
			return $TLE373;
		}
		public function __toString()
		{
			if ($item) {
				$TEF84 = 'true';
			} else {
				$TEF84 = 'false';
			}
			return $TEF84;
		}
		public function getHashCode()
		{
			$TSNNt1505 = $this->value;
			if ($TSNNt1505) {
				$TEF85 = 1;
			} else {
				$TEF85 = 0;
			}
			return $TEF85;
		}
	}
	class BoxedInteger extends Box
	{
		public function __construct($value)
		{
			$TLE374 = intval($value);
			Box::__construct($TLE374);
		}
		public function setValue($value)
		{
			$TLE375 = intval($value);
			$TSNNt1506 = $TLE375;
			$this->value = $TSNNt1506;
		}
		protected function compareTo(IComparable $obj)
		{
			$TSNNt1507 = $this->value;
			$TSNNt1508 = $obj->value;
			$TLE376 = ($TSNNt1507 - $TSNNt1508);
			return $TLE376;
		}
		public function getHashCode()
		{
			$TSNNt1509 = $this->value;
			return $TSNNt1509;
		}
	}
	class BoxedFloat extends Box
	{
		public function __construct($value)
		{
			$TLE377 = floatval($value);
			Box::__construct($TLE377);
		}
		public function setValue($value)
		{
			$TLE378 = floatval($value);
			$TSNNt1510 = $TLE378;
			$this->value = $TSNNt1510;
		}
		protected function compareTo(IComparable $obj)
		{
			$TSNNt1511 = $this->value;
			$TSNNt1512 = $obj->value;
			$TLE379 = ($TSNNt1511 - $TSNNt1512);
			return $TLE379;
		}
		public function getHashCode()
		{
			$TSNNt1513 = $this->value;
			$abs = abs($TSNNt1513);
			$TLE380 = 2;
			$TLE381 = log($abs, $TLE380);
			$TLE382 = 1;
			$TLE383 = ($TLE381 + $TLE382);
			$exponent = intval($TLE383);
			$TLE384 = 2;
			$TLE385 = pow($TLE384, $exponent);
			$mantissa = ($abs / $TLE385);
			$TLE386 = 2;
			$TLE387 = ($TLE386 * $mantissa);
			$TLE388 = 1;
			$TLE389 = ($TLE387 - $TLE388);
			$TLE390 = 2147483648;
			$TLE391 = ($TLE389 * $TLE390);
			$result = intval($TLE391);
			return $result;
		}
	}
	class BoxedString extends Box
	{
		public function __construct($value)
		{
			$TLE392 = strval($value);
			Box::__construct($TLE392);
		}
		public function setValue($value)
		{
			$TLE393 = strval($value);
			$TSNNt1514 = $TLE393;
			$this->value = $TSNNt1514;
		}
		protected function compareTo(IComparable $obj)
		{
			$TSNNt1515 = $this->value;
			$TSNNt1516 = $obj->value;
			$TLE394 = strcmp($TSNNt1515, $TSNNt1516);
			return $TLE394;
		}
		const SHIFT = 6;
		const MASK = 4227858432;
		public function getHashCode()
		{
			$result = 0;
			$i = 0;
			$ElcfPF6 = True;
			while (True) {
				if ($ElcfPF6) {
					$ElcfPF6 = False;
				} else {
					++$i;
				}
				$TSNNt1517 = $this->value;
				$TLE395 = strlen($TSNNt1517);
				$TLE396 = ($i < $TLE395);
				if ($TLE396) {
				} else {
					break;
				}
				$TLE397 = BoxedString::MASK;
				$TLE398 = ($result & $TLE397);
				$TLE399 = BoxedString::SHIFT;
				$TLE400 = ($result << $TLE399);
				$TLE401 = ($TLE398 ^ $TLE400);
				$TLE402 = 1;
				$TSNNt1518 = $this->value;
				$TLE403 = substr($TSNNt1518, $i, $TLE402);
				$TLE404 = ord($TLE403);
				$result = ($TLE401 ^ $TLE404);
			}
			return $result;
		}
	}
	function unbox($box)
	{
		$TLE405 = $box->getValue();
		return $TLE405;
	}
	abstract class AbstractQueue extends AbstractContainer implements IQueue
	{
		public function __construct()
		{
			AbstractContainer::__construct();
		}
	}
	class LinkedList_Element
	{
		protected $list = NULL;
		protected $datum = NULL;
		protected $next = NULL;
		public function __construct($list, $datum, $next)
		{
			$TSNNt1519 = $list;
			$this->list = $TSNNt1519;
			$TSNNt1520 = $datum;
			$this->datum = $TSNNt1520;
			$TSNNt1521 = $next;
			$this->next = $TSNNt1521;
		}
		public function getList()
		{
			$TSNNt1522 = $this->list;
			return $TSNNt1522;
		}
		public function getDatum()
		{
			$TSNNt1523 = $this->datum;
			return $TSNNt1523;
		}
		public function getNext()
		{
			$TSNNt1524 = $this->next;
			return $TSNNt1524;
		}
		public function setNext(LinkedList_Element $next)
		{
			$TSNNt1525 = $this->list;
			$TSNNt1526 = $next->list;
			$TLE406 = ($TSNNt1525 !== $TSNNt1526);
			if ($TLE406) {
				$TLE407 = new ArgumentError();
				throw $TLE407;
			}
			$TSNNt1527 = $next;
			$this->next = $TSNNt1527;
		}
		public function unsetNext()
		{
			$TLE408 = NULL;
			$TSNNt1528 = $TLE408;
			$this->next = $TSNNt1528;
		}
		public function insertAfter($item)
		{
			$TSNNt1529 = $this->list;
			$TSNNt1530 = $this->next;
			$TLE409 = new LinkedList_Element($TSNNt1529, $item, $TSNNt1530);
			$TSNNt1531 = $TLE409;
			$this->next = $TSNNt1531;
			$TSNNt1532 = $this->list;
			$TLE410 = $TSNNt1532->getTail();
			$TLE411 = ($TLE410 === $this);
			if ($TLE411) {
				$TSNNt1533 = $this->list;
				$TSNNt1534 = $this->next;
				$TSNNt1533->setTail($TSNNt1534);
			}
		}
		public function insertBefore($item)
		{
			$TSNNt1535 = $this->list;
			$tmp = new LinkedList_Element($TSNNt1535, $item, $this);
			$TSNNt1536 = $this->list;
			$TLE412 = $TSNNt1536->getHead();
			$TLE413 = ($this === $TLE412);
			if ($TLE413) {
				$TLE414 = NULL;
				$TLE415 = ($tmp === $TLE414);
				if ($TLE415) {
					$list->unsetHead();
				} else {
					$list->setHead($tmp);
				}
			} else {
				$TSNNt1537 = $this->list;
				$prevPtr = $TSNNt1537->getHead();
				while (True) {
					$TLE416 = NULL;
					$TLE86 = ($prevPtr !== $TLE416);
					if ($TLE86) {
						$TSNNt1538 = $prevPtr->next;
						$TEF87 = ($TSNNt1538 != $this);
					} else {
						$TEF87 = $TLE86;
					}
					$TLE417 = (bool) $TEF87;
					$TLE418 = !$TLE417;
					if ($TLE418) {
						break;
					}
					$TSNNt1539 = $prevPtr->next;
					$prevPtr = $TSNNt1539;
				}
				$TSNNt1540 = $tmp;
				$prevPtr->next = $TSNNt1540;
			}
		}
		public function extract()
		{
			$prevPtr = NULL;
			$TSNNt1541 = $this->list;
			$TLE419 = $TSNNt1541->getHead();
			$TLE420 = ($TLE419 === $this);
			if ($TLE420) {
				$TLE421 = NULL;
				$TSNNt1542 = $this->next;
				$TLE422 = ($TSNNt1542 === $TLE421);
				if ($TLE422) {
					$list->unsetHead();
				} else {
					$TSNNt1543 = $this->next;
					$list->setHead($TSNNt1543);
				}
			} else {
				$TSNNt1544 = $this->list;
				$prevPtr = $TSNNt1544->getHead();
				while (True) {
					$TLE423 = NULL;
					$TLE88 = ($prevPtr !== $TLE423);
					if ($TLE88) {
						$TSNNt1545 = $prevPtr->next;
						$TEF89 = ($TSNNt1545 != $this);
					} else {
						$TEF89 = $TLE88;
					}
					$TLE424 = (bool) $TEF89;
					$TLE425 = !$TLE424;
					if ($TLE425) {
						break;
					}
					$TSNNt1546 = $prevPtr->next;
					$prevPtr = $TSNNt1546;
				}
				$TLE426 = prevPtr;
				$TLE427 = NULL;
				$TLE428 = ($TLE426 === $TLE427);
				if ($TLE428) {
					$TLE429 = new ArgumentError();
					throw $TLE429;
				}
				$TSNNt1548 = $this->next;
				$TSNNt1547 = $TSNNt1548;
				$prevPtr->next = $TSNNt1547;
			}
			$TSNNt1549 = $this->list;
			$TLE430 = $TSNNt1549->getTail();
			$TLE431 = ($TLE430 === $this);
			if ($TLE431) {
				$TLE432 = NULL;
				$TLE433 = ($prevPtr === $TLE432);
				if ($TLE433) {
					$list->unsetTail();
				} else {
					$list->setTail($prevPtr);
				}
			}
		}
	}
	class LinkedList extends AbstractObject
	{
		protected $head = NULL;
		protected $tail = NULL;
		public function __construct()
		{
			AbstractObject::__construct();
			$TLE434 = NULL;
			$TSNNt1550 = $TLE434;
			$this->head = $TSNNt1550;
			$TLE435 = NULL;
			$TSNNt1551 = $TLE435;
			$this->tail = $TSNNt1551;
		}
		public function purge()
		{
			$TLE436 = NULL;
			$TSNNt1552 = $TLE436;
			$this->head = $TSNNt1552;
			$TLE437 = NULL;
			$TSNNt1553 = $TLE437;
			$this->tail = $TSNNt1553;
		}
		public function getHead()
		{
			$TSNNt1554 = $this->head;
			return $TSNNt1554;
		}
		public function setHead(LinkedList_Element $element)
		{
			$TLE438 = $element->getList();
			$TLE439 = ($TLE438 !== $this);
			if ($TLE439) {
				$TLE440 = new ArgumentError();
				throw $TLE440;
			}
			$TSNNt1555 = $element;
			$this->head = $TSNNt1555;
		}
		public function unsetHead()
		{
			$TLE441 = NULL;
			$TSNNt1556 = $TLE441;
			$this->head = $TSNNt1556;
		}
		public function getTail()
		{
			$TSNNt1557 = $this->tail;
			return $TSNNt1557;
		}
		public function setTail(LinkedList_Element $element)
		{
			$TLE442 = $element->getList();
			$TLE443 = ($TLE442 !== $this);
			if ($TLE443) {
				$TLE444 = new ArgumentError();
				throw $TLE444;
			}
			$TSNNt1558 = $element;
			$this->tail = $TSNNt1558;
		}
		public function unsetTail()
		{
			$TLE445 = NULL;
			$TSNNt1559 = $TLE445;
			$this->tail = $TSNNt1559;
		}
		public function isEmpty()
		{
			$TLE446 = NULL;
			$TSNNt1560 = $this->head;
			$TLE447 = ($TSNNt1560 === $TLE446);
			return $TLE447;
		}
		public function getFirst()
		{
			$TLE448 = NULL;
			$TSNNt1561 = $this->head;
			$TLE449 = ($TSNNt1561 === $TLE448);
			if ($TLE449) {
				$TLE450 = new ContainerEmptyException();
				throw $TLE450;
			}
			$TSNNt1562 = $this->head;
			$TLE451 = $TSNNt1562->getDatum();
			return $TLE451;
		}
		public function getLast()
		{
			$TLE452 = NULL;
			$TSNNt1563 = $this->tail;
			$TLE453 = ($TSNNt1563 === $TLE452);
			if ($TLE453) {
				$TLE454 = new ContainerEmptyException();
				throw $TLE454;
			}
			$TSNNt1564 = $this->tail;
			$TLE455 = $TSNNt1564->getDatum();
			return $TLE455;
		}
		public function prepend($item)
		{
			$TSNNt1565 = $this->head;
			$tmp = new LinkedList_Element($this, $item, $TSNNt1565);
			$TLE456 = NULL;
			$TSNNt1566 = $this->head;
			$TLE457 = ($TSNNt1566 === $TLE456);
			if ($TLE457) {
				$TSNNt1567 = $tmp;
				$this->tail = $TSNNt1567;
			}
			$TSNNt1568 = $tmp;
			$this->head = $TSNNt1568;
		}
		public function append($item)
		{
			$TLE458 = NULL;
			$tmp = new LinkedList_Element($this, $item, $TLE458);
			$TLE459 = NULL;
			$TSNNt1569 = $this->head;
			$TLE460 = ($TSNNt1569 === $TLE459);
			if ($TLE460) {
				$TSNNt1570 = $tmp;
				$this->head = $TSNNt1570;
			} else {
				$TSNNt1571 = $this->tail;
				$TSNNt1571->setNext($tmp);
			}
			$TSNNt1572 = $tmp;
			$this->tail = $TSNNt1572;
		}
		public function __clone()
		{
			$result = new LinkedList();
			$TSNNt1573 = $this->head;
			$ptr = $TSNNt1573;
			$ElcfPF7 = True;
			while (True) {
				if ($ElcfPF7) {
					$ElcfPF7 = False;
				} else {
					$ptr = $ptr->getNext();
				}
				$TLE461 = NULL;
				$TLE462 = ($ptr !== $TLE461);
				if ($TLE462) {
				} else {
					break;
				}
				$TLE463 = $ptr->getDatum();
				$result->append($TLE463);
			}
			return $result;
		}
		public function extract($item)
		{
			$TSNNt1574 = $this->head;
			$ptr = $TSNNt1574;
			$prevPtr = NULL;
			while (True) {
				$TLE464 = NULL;
				$TLE90 = ($ptr !== $TLE464);
				if ($TLE90) {
					$TLE465 = $ptr->getDatum();
					$TEF91 = ($TLE465 !== $item);
				} else {
					$TEF91 = $TLE90;
				}
				$TLE466 = (bool) $TEF91;
				$TLE467 = !$TLE466;
				if ($TLE467) {
					break;
				}
				$prevPtr = $ptr;
				$ptr = $ptr->getNext();
			}
			$TLE468 = NULL;
			$TLE469 = ($ptr === $TLE468);
			if ($TLE469) {
				$TLE470 = new ArgumentError();
				throw $TLE470;
			}
			$TSNNt1575 = $this->head;
			$TLE471 = ($ptr === $TSNNt1575);
			if ($TLE471) {
				$TLE472 = $ptr->getNext();
				$TSNNt1576 = $TLE472;
				$this->head = $TSNNt1576;
			} else {
				$tmp = $ptr->getNext();
				$TLE473 = NULL;
				$TLE474 = ($tmp === $TLE473);
				if ($TLE474) {
					$prevPtr->unsetNext();
				} else {
					$TLE475 = $ptr->getNext();
					$prevPtr->setNext($TLE475);
				}
			}
			$TSNNt1577 = $this->tail;
			$TLE476 = ($ptr === $TSNNt1577);
			if ($TLE476) {
				$TSNNt1578 = $prevPtr;
				$this->tail = $TSNNt1578;
			}
		}
		public function reduce(IReduceFunction $f, $initialState)
		{
			$state = $initialState;
			$TSNNt1579 = $this->head;
			$ptr = $TSNNt1579;
			while (True) {
				$TLE477 = NULL;
				$TLE478 = ($ptr !== $TLE477);
				$TLE479 = !$TLE478;
				if ($TLE479) {
					break;
				}
				$TLE480 = $ptr->getDatum();
				$state = $f->invoke($state, $TLE480);
				$ptr = $ptr->getNext();
			}
			return $state;
		}
		public function __toString()
		{
			$TLE481 = new LinkedListToStringReduceFunction();
			$TLE482 = '';
			$TLE483 = '';
			unset($TSa484);
			$TSa484 = (array) $TSa484;
			$TLE1259 = 0;
			$TSNNi1580 = $TLE482;
			$TSa484[$TLE1259] = $TSNNi1580;
			$TLE1260 = 1;
			$TSNNi1581 = $TLE483;
			$TSa484[$TLE1260] = $TSNNi1581;
			$s = $this->reduce($TLE481, $TSa484);
			$TLE485 = 'LinkedList{';
			$TLE486 = 0;
			$TSNNi1582 = $s[$TLE486];
			$TLE487 = ($TLE485 . $TSNNi1582);
			$TLE488 = '}';
			$TLE489 = ($TLE487 . $TLE488);
			return $TLE489;
		}
	}
	class LinkedListToStringReduceFunction implements IReduceFunction
	{
		public function invoke($s, $item)
		{
			$TLE490 = 0;
			$TLE491 = 1;
			$TSNNi1583 = $s[$TLE490];
			$TSNNi1584 = $s[$TLE491];
			$TLE492 = ($TSNNi1583 . $TSNNi1584);
			$TLE493 = $item->__toString();
			$TLE494 = ($TLE492 . $TLE493);
			$TLE495 = ", ";
			unset($TSa496);
			$TSa496 = (array) $TSa496;
			$TLE1261 = 0;
			$TSNNi1585 = $TLE494;
			$TSa496[$TLE1261] = $TSNNi1585;
			$TLE1262 = 1;
			$TSNNi1586 = $TLE495;
			$TSa496[$TLE1262] = $TSNNi1586;
			return $TSa496;
		}
	}
	class QueueAsLinkedList_Iterator extends AbstractIterator
	{
		protected $queue = NULL;
		protected $position = NULL;
		protected $key = 0;
		public function __construct(QueueAsLinkedList $queue)
		{
			AbstractIterator::__construct();
			$TSNNt1587 = $queue;
			$this->queue = $TSNNt1587;
			$TLE497 = $queue->getList();
			$TLE498 = $TLE497->getHead();
			$TSNNt1588 = $TLE498;
			$this->position = $TSNNt1588;
			$TLE499 = 0;
			$TSNNt1589 = $TLE499;
			$this->key = $TSNNt1589;
		}
		public function valid()
		{
			$TLE500 = NULL;
			$TSNNt1590 = $this->position;
			$TLE501 = ($TSNNt1590 !== $TLE500);
			return $TLE501;
		}
		public function key()
		{
			$TSNNt1591 = $this->key;
			return $TSNNt1591;
		}
		public function current()
		{
			$TSNNt1592 = $this->position;
			$TLE502 = $TSNNt1592->getDatum();
			return $TLE502;
		}
		public function next()
		{
			$TSNNt1593 = $this->position;
			$TLE503 = $TSNNt1593->getNext();
			$TSNNt1594 = $TLE503;
			$this->position = $TSNNt1594;
			$TSNNt1595 = $this->key;
			$Toa132 = $TSNNt1595;
			$TLE504 = 1;
			$Toa132 = ($Toa132 + $TLE504);
			$TSNNt1596 = $Toa132;
			$this->key = $TSNNt1596;
		}
		public function rewind()
		{
			$TSNNt1597 = $this->queue;
			$TLE505 = $TSNNt1597->getList();
			$TLE506 = $TLE505->getHead();
			$TSNNt1598 = $TLE506;
			$this->position = $TSNNt1598;
			$TLE507 = 0;
			$TSNNt1599 = $TLE507;
			$this->key = $TSNNt1599;
		}
	}
	class QueueAsLinkedList extends AbstractQueue
	{
		protected $list = NULL;
		public function __construct()
		{
			AbstractQueue::__construct();
			$TLE508 = new LinkedList();
			$TSNNt1600 = $TLE508;
			$this->list = $TSNNt1600;
		}
		public function getList()
		{
			$TSNNt1601 = $this->list;
			return $TSNNt1601;
		}
		public function purge()
		{
			$TSNNt1602 = $this->list;
			$TSNNt1602->purge();
		}
		public function enqueue(IObject $obj)
		{
			$TSNNt1603 = $this->list;
			$TSNNt1603->append($obj);
			$TSNNt1604 = $this->count;
			$Toa133 = $TSNNt1604;
			$TLE509 = 1;
			$Toa133 = ($Toa133 + $TLE509);
			$TSNNt1605 = $Toa133;
			$this->count = $TSNNt1605;
		}
		public function dequeue()
		{
			$TLE510 = 0;
			$TSNNt1606 = $this->count;
			$TLE511 = ($TSNNt1606 == $TLE510);
			if ($TLE511) {
				$TLE512 = new ContainerEmptyException();
				throw $TLE512;
			}
			$TSNNt1607 = $this->list;
			$result = $TSNNt1607->getFirst();
			$TSNNt1608 = $this->list;
			$TSNNt1608->extract($result);
			$TSNNt1609 = $this->count;
			$Toa134 = $TSNNt1609;
			$TLE513 = 1;
			$Toa134 = ($Toa134 - $TLE513);
			$TSNNt1610 = $Toa134;
			$this->count = $TSNNt1610;
			return $result;
		}
		public function getHead()
		{
			$TLE514 = 0;
			$TSNNt1611 = $this->count;
			$TLE515 = ($TSNNt1611 == $TLE514);
			if ($TLE515) {
				$TLE516 = new ContainerEmptyException();
				throw $TLE516;
			}
			$TSNNt1612 = $this->list;
			$TLE517 = $TSNNt1612->getFirst();
			return $TLE517;
		}
		public function reduce(IReduceFunction $f, $initialState)
		{
			$TSNNt1613 = $this->list;
			$TLE518 = $TSNNt1613->reduce($f, $initialState);
			return $TLE518;
		}
		public function getIterator()
		{
			$TLE519 = new QueueAsLinkedList_Iterator($this);
			return $TLE519;
		}
		public function compareTo(IComparable $obj)
		{
			$TLE520 = new MethodNotImplementedException();
			throw $TLE520;
		}
	}
	class CountingVisitor extends AbstractVisitor
	{
		protected $count = 0;
		public function __construct($count = 0)
		{
			AbstractVisitor::__construct();
			$TLE521 = 0;
			$TSNNt1614 = $TLE521;
			$this->count = $TSNNt1614;
		}
		public function getCount()
		{
			$TSNNt1615 = $this->count;
			return $TSNNt1615;
		}
		public function setCount($count)
		{
			$TLE522 = count;
			$TSNNt1616 = $TLE522;
			$this->count = $TSNNt1616;
		}
		public function visit(IObject $obj)
		{
			$TSNNt1617 = $this->count;
			++$TSNNt1617;
			$this->count = $TSNNt1617;
		}
	}
	interface IPrePostVisitor
	{
		function preVisit(IObject $obj);
		function inVisit(IObject $obj);
		function postVisit(IObject $obj);
		function isDone();
	}
	abstract class AbstractPrePostVisitor implements IPrePostVisitor
	{
		public function __construct()
		{
		}
		public function preVisit(IObject $obj)
		{
		}
		public function inVisit(IObject $obj)
		{
		}
		public function postVisit(IObject $obj)
		{
		}
		public function isDone()
		{
			$TLE523 = False;
			return $TLE523;
		}
	}
	class PreOrder extends AbstractPrePostVisitor
	{
		protected $visitor = NULL;
		public function __construct($visitor)
		{
			$TSNNt1618 = $visitor;
			$this->visitor = $TSNNt1618;
		}
		public function preVisit(IObject $obj)
		{
			$TSNNt1619 = $this->visitor;
			$TSNNt1619->visit($obj);
		}
		public function isDone()
		{
			$TSNNt1620 = $this->visitor;
			$TLE524 = $TSNNt1620->isDone();
			return $TLE524;
		}
	}
	class PrintingVisitor extends AbstractVisitor
	{
		protected $stream = NULL;
		public function __construct($stream)
		{
			AbstractVisitor::__construct();
			$TSNNt1621 = $stream;
			$this->stream = $TSNNt1621;
		}
		public function visit(IObject $obj)
		{
			$TLE525 = "%s\n";
			$TLE526 = $obj->__toString();
			$TSNNt1622 = $this->stream;
			fprintf($TSNNt1622, $TLE525, $TLE526);
		}
	}
	class AbstractGraph_ToStringVisitor extends AbstractVisitor
	{
		protected $text = '';
		public function __construct()
		{
			AbstractVisitor::__construct();
		}
		public function getText()
		{
			$TSNNt1623 = $this->text;
			return $TSNNt1623;
		}
		public function visit(IObject $obj)
		{
			$TSNNt1624 = $this->text;
			$Toa135 = $TSNNt1624;
			$TLE527 = $obj->__toString();
			$TLE528 = "\n";
			$TLE529 = ($TLE527 . $TLE528);
			$Toa135 = ($Toa135 . $TLE529);
			$TSNNt1625 = $Toa135;
			$this->text = $TSNNt1625;
			$TLE530 = $obj->getEmanatingEdges();
			$it = $TLE530->getIterator();
			$it->rewind();
			while (True) {
				$TLE531 = $it->valid();
				$TLE532 = !$TLE531;
				if ($TLE532) {
					break;
				}
				$edge = $it->current();
				$TSNNt1626 = $this->text;
				$Toa136 = $TSNNt1626;
				$TLE533 = "    ";
				$TLE534 = $edge->__toString();
				$TLE535 = ($TLE533 . $TLE534);
				$TLE536 = "\n";
				$TLE537 = ($TLE535 . $TLE536);
				$Toa136 = ($Toa136 . $TLE537);
				$TSNNt1627 = $Toa136;
				$this->text = $TSNNt1627;
				$it->next();
			}
		}
	}
	class AbstractGraph_VertexIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		public function __construct(AbstractGraph $graph)
		{
			AbstractIterator::__construct();
			$TSNNt1628 = $graph;
			$this->graph = $TSNNt1628;
			$TLE538 = 0;
			$TSNNt1629 = $TLE538;
			$this->v = $TSNNt1629;
		}
		public function valid()
		{
			$TSNNt1630 = $this->graph;
			$TLE539 = $TSNNt1630->getNumberOfVertices();
			$TSNNt1631 = $this->v;
			$TLE540 = ($TSNNt1631 < $TLE539);
			return $TLE540;
		}
		public function key()
		{
			$TSNNt1632 = $this->v;
			return $TSNNt1632;
		}
		public function current()
		{
			$TSNNt1633 = $this->graph;
			$TSNNt1634 = $this->v;
			$TLE541 = $TSNNt1633->getVertex($TSNNt1634);
			return $TLE541;
		}
		public function next()
		{
			$TSNNt1635 = $this->v;
			++$TSNNt1635;
			$this->v = $TSNNt1635;
		}
		public function rewind()
		{
			$TLE542 = 0;
			$TSNNt1636 = $TLE542;
			$this->v = $TSNNt1636;
		}
	}
	class AbstractGraph_VertexAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		public function __construct(AbstractGraph $graph)
		{
			$TSNNt1637 = $graph;
			$this->graph = $TSNNt1637;
		}
		public function getIterator()
		{
			$TSNNt1638 = $this->graph;
			$TLE543 = $TSNNt1638->getIterator();
			return $TLE543;
		}
	}
	abstract class AbstractGraph extends AbstractContainer implements IGraph
	{
		protected $numberOfVertices = 0;
		protected $numberOfEdges = 0;
		protected $vertex = NULL;
		public function __construct($size)
		{
			AbstractContainer::__construct();
			$TLE544 = 0;
			$TSNNt1639 = $TLE544;
			$this->numberOfVertices = $TSNNt1639;
			$TLE545 = 0;
			$TSNNt1640 = $TLE545;
			$this->numberOfEdges = $TSNNt1640;
			$TLE546 = new BasicArray($size);
			$TSNNt1641 = $TLE546;
			$this->vertex = $TSNNt1641;
		}
		public abstract function getIncidentEdges($v);
		public abstract function getEmanatingEdges($v);
		public function purge()
		{
			$i = 0;
			$ElcfPF8 = True;
			while (True) {
				if ($ElcfPF8) {
					$ElcfPF8 = False;
				} else {
					++$i;
				}
				$TSNNt1642 = $this->numberOfVertices;
				$TLE547 = ($i < $TSNNt1642);
				if ($TLE547) {
				} else {
					break;
				}
				$TLE548 = NULL;
				$TSNNt1643 = $this->vertex;
				$TSNNt1643->offsetSet($i, $TLE548);
			}
			$TLE549 = 0;
			$TSNNt1644 = $TLE549;
			$this->numberOfVertices = $TSNNt1644;
			$TLE550 = 0;
			$TSNNt1645 = $TLE550;
			$this->numberOfEdges = $TSNNt1645;
		}
		protected function insertVertex(Vertex $v)
		{
			$TSNNt1646 = $this->vertex;
			$TLE551 = $TSNNt1646->getLength();
			$TSNNt1647 = $this->numberOfVertices;
			$TLE552 = ($TSNNt1647 == $TLE551);
			if ($TLE552) {
				$TLE553 = new ContainerFullException();
				throw $TLE553;
			}
			$TLE554 = $v->getNumber();
			$TSNNt1648 = $this->numberOfVertices;
			$TLE555 = ($TLE554 != $TSNNt1648);
			if ($TLE555) {
				$TLE556 = new ArgumentError();
				throw $TLE556;
			}
			$TSNNt1649 = $this->vertex;
			$TSNNt1650 = $this->numberOfVertices;
			$TSNNt1649->offsetSet($TSNNt1650, $v);
			$TSNNt1651 = $this->numberOfVertices;
			++$TSNNt1651;
			$this->numberOfVertices = $TSNNt1651;
		}
		public function addVertex($v, $weight = NULL)
		{
			$TLE557 = new Vertex($this, $v, $weight);
			$this->insertVertex($TLE557);
		}
		public function getVertex($v)
		{
			$TLE558 = 0;
			$TLE92 = ($v < $TLE558);
			if ($TLE92) {
				$TEF93 = $TLE92;
			} else {
				$TSNNt1652 = $this->numberOfVertices;
				$TEF93 = ($TSNNt1652 <= $v);
			}
			$TLE559 = (bool) $TEF93;
			if ($TLE559) {
				$TLE560 = new IndexError();
				throw $TLE560;
			}
			$TSNNt1653 = $this->vertex;
			$TLE561 = $TSNNt1653->offsetGet($v);
			return $TLE561;
		}
		public function getVertices()
		{
			$TLE562 = new AbstractGraph_VertexAggregate($this);
			return $TLE562;
		}
		protected abstract function insertEdge(Edge $edge);
		public function isDirected()
		{
			$TLE563 = $this instanceof IDigraph;
			return $TLE563;
		}
		public function addEdge($v, $w, $weight = NULL)
		{
			$TLE564 = new Edge($this, $v, $w, $weight);
			$this->insertEdge($TLE564);
		}
		public function getNumberOfVertices()
		{
			$TSNNt1654 = $this->numberOfVertices;
			return $TSNNt1654;
		}
		public function getNumberOfEdges()
		{
			$TSNNt1655 = $this->numberOfEdges;
			return $TSNNt1655;
		}
		public function accept(IVisitor $visitor)
		{
			$v = 0;
			$ElcfPF9 = True;
			while (True) {
				if ($ElcfPF9) {
					$ElcfPF9 = False;
				} else {
					++$v;
				}
				$TSNNt1656 = $this->numberOfVertices;
				$TLE565 = ($v < $TSNNt1656);
				if ($TLE565) {
				} else {
					break;
				}
				$TLE566 = $visitor->isDone();
				if ($TLE566) {
					break;
				}
				$TSNNt1657 = $this->vertex;
				$TLE567 = $TSNNt1657->offsetGet($v);
				$visitor->visit($TLE567);
			}
		}
		public function reduce(IReduceFunction $f, $initialState)
		{
			$TSNNt1658 = $this->vertex;
			$TLE568 = $TSNNt1658->reduce($f, $initialState);
			return $TLE568;
		}
		public function depthFirstTraversal(IPrePostVisitor $visitor, $start)
		{
			$TSNNt1659 = $this->numberOfVertices;
			$visited = new BasicArray($TSNNt1659);
			$v = 0;
			$ElcfPF10 = True;
			while (True) {
				if ($ElcfPF10) {
					$ElcfPF10 = False;
				} else {
					++$v;
				}
				$TSNNt1660 = $this->numberOfVertices;
				$TLE569 = ($v < $TSNNt1660);
				if ($TLE569) {
				} else {
					break;
				}
				$TLE570 = False;
				$visited->offsetSet($v, $TLE570);
			}
			$TSNNt1661 = $this->vertex;
			$TLE571 = $TSNNt1661->offsetGet($start);
			$this->doDepthFirstTraversal($visitor, $TLE571, $visited);
		}
		private function doDepthFirstTraversal(IPrePostVisitor $visitor, $v, $visited)
		{
			$TLE572 = $visitor->isDone();
			if ($TLE572) {
				$TLE573 = NULL;
				return $TLE573;
			}
			$visitor->preVisit($v);
			$TLE574 = $v->getNumber();
			$TLE575 = True;
			$visited->offsetSet($TLE574, $TLE575);
			$TLE576 = $v->getSuccessors();
			foreach ($TLE576 as $to) {
				$TLE577 = $to->getNumber();
				$TLE578 = $visited->offsetGet($TLE577);
				$TLE579 = !$TLE578;
				if ($TLE579) {
					$this->doDepthFirstTraversal($visitor, $to, $visited);
				}
			}
			$visitor->postVisit($v);
		}
		public function breadthFirstTraversal(IVisitor $visitor, $start)
		{
			$TSNNt1662 = $this->numberOfVertices;
			$enqueued = new BasicArray($TSNNt1662);
			$v = 0;
			$ElcfPF11 = True;
			while (True) {
				if ($ElcfPF11) {
					$ElcfPF11 = False;
				} else {
					++$v;
				}
				$TSNNt1663 = $this->numberOfVertices;
				$TLE580 = ($v < $TSNNt1663);
				if ($TLE580) {
				} else {
					break;
				}
				$TLE581 = False;
				$enqueued->offsetSet($v, $TLE581);
			}
			$queue = new QueueAsLinkedList();
			$TLE582 = True;
			$enqueued->offsetSet($start, $TLE582);
			$TSNNt1664 = $this->vertex;
			$TLE583 = $TSNNt1664->offsetGet($start);
			$queue->enqueue($TLE583);
			while (True) {
				$TLE584 = $queue->isEmpty();
				$TLE94 = !$TLE584;
				if ($TLE94) {
					$TLE585 = $visitor->isDone();
					$TEF95 = !$TLE585;
				} else {
					$TEF95 = $TLE94;
				}
				$TLE586 = (bool) $TEF95;
				$TLE587 = !$TLE586;
				if ($TLE587) {
					break;
				}
				$v = $queue->dequeue();
				$visitor->visit($v);
				$TLE588 = $v->getSuccessors();
				foreach ($TLE588 as $to) {
					$TLE589 = $to->getNumber();
					$TLE590 = $enqueued->offsetGet($TLE589);
					$TLE591 = !$TLE590;
					if ($TLE591) {
						$TLE592 = $to->getNumber();
						$TLE593 = True;
						$enqueued->offsetSet($TLE592, $TLE593);
						$queue->enqueue($to);
					}
				}
			}
		}
		public function topologicalOrderTraversal(IVisitor $visitor)
		{
			$TSNNt1665 = $this->numberOfVertices;
			$inDegree = new BasicArray($TSNNt1665);
			$v = 0;
			$ElcfPF12 = True;
			while (True) {
				if ($ElcfPF12) {
					$ElcfPF12 = False;
				} else {
					++$v;
				}
				$TSNNt1666 = $this->numberOfVertices;
				$TLE594 = ($v < $TSNNt1666);
				if ($TLE594) {
				} else {
					break;
				}
				$TLE595 = 0;
				$inDegree->offsetSet($v, $TLE595);
			}
			$TLE596 = $this->getEdges();
			foreach ($TLE596 as $edge) {
				$to = $edge->getV1();
				$TLE597 = $to->getNumber();
				$TLE598 = $to->getNumber();
				$TLE599 = $inDegree->offsetGet($TLE598);
				$TLE600 = 1;
				$TLE601 = ($TLE599 + $TLE600);
				$inDegree->offsetSet($TLE597, $TLE601);
			}
			$queue = new QueueAsLinkedList();
			$v = 0;
			$ElcfPF13 = True;
			while (True) {
				if ($ElcfPF13) {
					$ElcfPF13 = False;
				} else {
					++$v;
				}
				$TSNNt1667 = $this->numberOfVertices;
				$TLE602 = ($v < $TSNNt1667);
				if ($TLE602) {
				} else {
					break;
				}
				$TLE603 = $inDegree->offsetGet($v);
				$TLE604 = 0;
				$TLE605 = ($TLE603 == $TLE604);
				if ($TLE605) {
					$TSNNt1668 = $this->vertex;
					$TLE606 = $TSNNt1668->offsetGet($v);
					$queue->enqueue($TLE606);
				}
			}
			while (True) {
				$TLE607 = $queue->isEmpty();
				$TLE96 = !$TLE607;
				if ($TLE96) {
					$TLE608 = $visitor->isDone();
					$TEF97 = !$TLE608;
				} else {
					$TEF97 = $TLE96;
				}
				$TLE609 = (bool) $TEF97;
				$TLE610 = !$TLE609;
				if ($TLE610) {
					break;
				}
				$v = $queue->dequeue();
				$visitor->visit($v);
				$TLE611 = $v->getSuccessors();
				foreach ($TLE611 as $to) {
					$TLE612 = $to->getNumber();
					$TLE613 = $to->getNumber();
					$TLE614 = $inDegree->offsetGet($TLE613);
					$TLE615 = 1;
					$TLE616 = ($TLE614 - $TLE615);
					$inDegree->offsetSet($TLE612, $TLE616);
					$TLE617 = $to->getNumber();
					$TLE618 = $inDegree->offsetGet($TLE617);
					$TLE619 = 0;
					$TLE620 = ($TLE618 == $TLE619);
					if ($TLE620) {
						$queue->enqueue($to);
					}
				}
			}
		}
		public function __toString()
		{
			$visitor = new AbstractGraph_ToStringVisitor();
			$this->accept($visitor);
			$TLE621 = $this->getClass();
			$TLE622 = $TLE621->getName();
			$TLE623 = "{\n";
			$TLE624 = ($TLE622 . $TLE623);
			$TLE625 = $visitor->getText();
			$TLE626 = ($TLE624 . $TLE625);
			$TLE627 = "}";
			$TLE628 = ($TLE626 . $TLE627);
			return $TLE628;
		}
		public function isConnected()
		{
			$visitor = new CountingVisitor();
			$TLE629 = new PreOrder($visitor);
			$TLE630 = 0;
			$this->depthFirstTraversal($TLE629, $TLE630);
			$TLE631 = $visitor->getCount();
			$TSNNt1669 = $this->numberOfVertices;
			$TLE632 = ($TLE631 == $TSNNt1669);
			return $TLE632;
		}
		public function isStronglyConnected()
		{
			$visitor = new CountingVisitor();
			$v = 0;
			$ElcfPF14 = True;
			while (True) {
				if ($ElcfPF14) {
					$ElcfPF14 = False;
				} else {
					++$v;
				}
				$TSNNt1670 = $this->numberOfVertices;
				$TLE633 = ($v < $TSNNt1670);
				if ($TLE633) {
				} else {
					break;
				}
				$TLE634 = 0;
				$visitor->setCount($TLE634);
				$TLE635 = new PreOrder($visitor);
				$this->depthFirstTraversal($TLE635, $v);
				$TLE636 = $visitor->getCount();
				$TSNNt1671 = $this->numberOfVertices;
				$TLE637 = ($TLE636 != $TSNNt1671);
				if ($TLE637) {
					$TLE638 = False;
					return $TLE638;
				}
			}
			$TLE639 = True;
			return $TLE639;
		}
		public function isCyclic()
		{
			$visitor = new CountingVisitor();
			$this->topologicalOrderTraversal($visitor);
			$TLE640 = $visitor->getCount();
			$TSNNt1672 = $this->numberOfVertices;
			$TLE641 = ($TLE640 != $TSNNt1672);
			return $TLE641;
		}
		public function getIterator()
		{
			$TLE642 = new AbstractGraph_VertexIterator($this);
			return $TLE642;
		}
	}
	class GraphAsLists_EdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $ptr = NULL;
		protected $pos = 0;
		public function __construct(GraphAsLists $graph)
		{
			AbstractIterator::__construct();
			$TSNNt1673 = $graph;
			$this->graph = $TSNNt1673;
			$TSNNt1674 = $this->graph;
			$list = $TSNNt1674->getAdjacencyList();
			$TLE643 = 0;
			$TSNNt1675 = $TLE643;
			$this->v = $TSNNt1675;
			$ElcfPF15 = True;
			while (True) {
				if ($ElcfPF15) {
					$ElcfPF15 = False;
				} else {
					$TSNNt1676 = $this->v;
					++$TSNNt1676;
					$this->v = $TSNNt1676;
				}
				$TSNNt1677 = $this->graph;
				$TLE644 = $TSNNt1677->getNumberOfVertices();
				$TSNNt1678 = $this->v;
				$TLE645 = ($TSNNt1678 < $TLE644);
				if ($TLE645) {
				} else {
					break;
				}
				$TSNNt1679 = $this->v;
				$TLE646 = $list->offsetGet($TSNNt1679);
				$TLE647 = $TLE646->getHead();
				$TSNNt1680 = $TLE647;
				$this->ptr = $TSNNt1680;
				$TLE648 = 0;
				$TSNNt1681 = $TLE648;
				$this->pos = $TSNNt1681;
				$TLE649 = NULL;
				$TSNNt1682 = $this->ptr;
				$TLE650 = ($TSNNt1682 !== $TLE649);
				if ($TLE650) {
					break;
				}
			}
		}
		public function valid()
		{
			$TLE651 = NULL;
			$TSNNt1683 = $this->ptr;
			$TLE652 = ($TSNNt1683 !== $TLE651);
			return $TLE652;
		}
		public function key()
		{
			$TSNNt1684 = $this->graph;
			$TLE653 = $TSNNt1684->getNumberOfVertices();
			$TSNNt1685 = $this->v;
			$TLE654 = ($TSNNt1685 * $TLE653);
			$TSNNt1686 = $this->pos;
			$TLE655 = ($TLE654 + $TSNNt1686);
			return $TLE655;
		}
		public function current()
		{
			$TSNNt1687 = $this->ptr;
			$TLE656 = $TSNNt1687->getDatum();
			return $TLE656;
		}
		public function next()
		{
			$TSNNt1688 = $this->ptr;
			$TLE657 = $TSNNt1688->getNext();
			$TSNNt1689 = $TLE657;
			$this->ptr = $TSNNt1689;
			$TSNNt1690 = $this->pos;
			$Toa137 = $TSNNt1690;
			$TLE658 = 1;
			$Toa137 = ($Toa137 + $TLE658);
			$TSNNt1691 = $Toa137;
			$this->pos = $TSNNt1691;
			$TLE659 = NULL;
			$TSNNt1692 = $this->ptr;
			$TLE660 = ($TSNNt1692 !== $TLE659);
			if ($TLE660) {
				$TLE661 = NULL;
				return $TLE661;
			}
			$TSNNt1693 = $this->graph;
			$list = $TSNNt1693->getAdjacencyList();
			$TSNNt1694 = $this->v;
			++$TSNNt1694;
			$this->v = $TSNNt1694;
			$ElcfPF16 = True;
			while (True) {
				if ($ElcfPF16) {
					$ElcfPF16 = False;
				} else {
					$TSNNt1695 = $this->v;
					++$TSNNt1695;
					$this->v = $TSNNt1695;
				}
				$TSNNt1696 = $this->graph;
				$TLE662 = $TSNNt1696->getNumberOfVertices();
				$TSNNt1697 = $this->v;
				$TLE663 = ($TSNNt1697 < $TLE662);
				if ($TLE663) {
				} else {
					break;
				}
				$TSNNt1698 = $this->v;
				$TLE664 = $list->offsetGet($TSNNt1698);
				$TLE665 = $TLE664->getHead();
				$TSNNt1699 = $TLE665;
				$this->ptr = $TSNNt1699;
				$TLE666 = 0;
				$TSNNt1700 = $TLE666;
				$this->pos = $TSNNt1700;
				$TLE667 = NULL;
				$TSNNt1701 = $this->ptr;
				$TLE668 = ($TSNNt1701 !== $TLE667);
				if ($TLE668) {
					break;
				}
			}
		}
		public function rewind()
		{
			$TSNNt1702 = $this->graph;
			$list = $TSNNt1702->getAdjacencyList();
			$TLE669 = 0;
			$TSNNt1703 = $TLE669;
			$this->v = $TSNNt1703;
			$ElcfPF17 = True;
			while (True) {
				if ($ElcfPF17) {
					$ElcfPF17 = False;
				} else {
					$TSNNt1704 = $this->v;
					++$TSNNt1704;
					$this->v = $TSNNt1704;
				}
				$TSNNt1705 = $this->graph;
				$TLE670 = $TSNNt1705->getNumberOfVertices();
				$TSNNt1706 = $this->v;
				$TLE671 = ($TSNNt1706 < $TLE670);
				if ($TLE671) {
				} else {
					break;
				}
				$TSNNt1707 = $this->v;
				$TLE672 = $list->offsetGet($TSNNt1707);
				$TLE673 = $TLE672->getHead();
				$TSNNt1708 = $TLE673;
				$this->ptr = $TSNNt1708;
				$TLE674 = 0;
				$TSNNt1709 = $TLE674;
				$this->pos = $TSNNt1709;
				$TLE675 = NULL;
				$TSNNt1710 = $this->ptr;
				$TLE676 = ($TSNNt1710 !== $TLE675);
				if ($TLE676) {
					break;
				}
			}
		}
	}
	class GraphAsLists_EdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		public function __construct(GraphAsLists $graph)
		{
			$TSNNt1711 = $graph;
			$this->graph = $TSNNt1711;
		}
		public function getIterator()
		{
			$TSNNt1712 = $this->graph;
			$TLE677 = new GraphAsLists_EdgeIterator($TSNNt1712);
			return $TLE677;
		}
	}
	class GraphAsLists_EmanatingEdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $ptr = NULL;
		protected $pos = 0;
		public function __construct(GraphAsLists $graph, $v)
		{
			AbstractIterator::__construct();
			$TSNNt1713 = $graph;
			$this->graph = $TSNNt1713;
			$TSNNt1714 = $v;
			$this->v = $TSNNt1714;
			$TSNNt1715 = $this->graph;
			$list = $TSNNt1715->getAdjacencyList();
			$TSNNt1716 = $this->v;
			$TLE678 = $list->offsetGet($TSNNt1716);
			$TLE679 = $TLE678->getHead();
			$TSNNt1717 = $TLE679;
			$this->ptr = $TSNNt1717;
			$TLE680 = 0;
			$TSNNt1718 = $TLE680;
			$this->pos = $TSNNt1718;
		}
		public function valid()
		{
			$TLE681 = NULL;
			$TSNNt1719 = $this->ptr;
			$TLE682 = ($TSNNt1719 !== $TLE681);
			return $TLE682;
		}
		public function key()
		{
			$TSNNt1720 = $this->graph;
			$TLE683 = $TSNNt1720->getNumberOfVertices();
			$TSNNt1721 = $this->v;
			$TLE684 = ($TSNNt1721 * $TLE683);
			$TSNNt1722 = $this->pos;
			$TLE685 = ($TLE684 + $TSNNt1722);
			return $TLE685;
		}
		public function current()
		{
			$TSNNt1723 = $this->ptr;
			$TLE686 = $TSNNt1723->getDatum();
			return $TLE686;
		}
		public function next()
		{
			$TSNNt1724 = $this->ptr;
			$TLE687 = $TSNNt1724->getNext();
			$TSNNt1725 = $TLE687;
			$this->ptr = $TSNNt1725;
			$TSNNt1726 = $this->pos;
			$Toa138 = $TSNNt1726;
			$TLE688 = 1;
			$Toa138 = ($Toa138 + $TLE688);
			$TSNNt1727 = $Toa138;
			$this->pos = $TSNNt1727;
		}
		public function rewind()
		{
			$TSNNt1728 = $this->graph;
			$list = $TSNNt1728->getAdjacencyList();
			$TSNNt1729 = $this->v;
			$TLE689 = $list->offsetGet($TSNNt1729);
			$TLE690 = $TLE689->getHead();
			$TSNNt1730 = $TLE690;
			$this->ptr = $TSNNt1730;
			$TLE691 = 0;
			$TSNNt1731 = $TLE691;
			$this->pos = $TSNNt1731;
		}
	}
	class GraphAsLists_EmanatingEdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		protected $v = 0;
		public function __construct(GraphAsLists $graph, $v)
		{
			$TSNNt1732 = $graph;
			$this->graph = $TSNNt1732;
			$TSNNt1733 = $v;
			$this->v = $TSNNt1733;
		}
		public function getIterator()
		{
			$TSNNt1734 = $this->graph;
			$TSNNt1735 = $this->v;
			$TLE692 = new GraphAsLists_EmanatingEdgeIterator($TSNNt1734, $TSNNt1735);
			return $TLE692;
		}
	}
	class GraphAsLists extends AbstractGraph
	{
		protected $adjacencyList = NULL;
		public function __construct($size = 0)
		{
			AbstractGraph::__construct($size);
			$TLE693 = new BasicArray($size);
			$TSNNt1736 = $TLE693;
			$this->adjacencyList = $TSNNt1736;
			$i = 0;
			$ElcfPF18 = True;
			while (True) {
				if ($ElcfPF18) {
					$ElcfPF18 = False;
				} else {
					++$i;
				}
				$TLE694 = ($i < $size);
				if ($TLE694) {
				} else {
					break;
				}
				$TLE695 = new LinkedList();
				$TSNNt1737 = $this->adjacencyList;
				$TSNNt1737->offsetSet($i, $TLE695);
			}
		}
		public function &getAdjacencyList()
		{
			$TSNNt1738 = $this->adjacencyList;
			return $TSNNt1738;
		}
		public function purge()
		{
			$i = 0;
			$ElcfPF19 = True;
			while (True) {
				if ($ElcfPF19) {
					$ElcfPF19 = False;
				} else {
					++$i;
				}
				$TSNNt1739 = $this->numberOfVertices;
				$TLE696 = ($i < $TSNNt1739);
				if ($TLE696) {
				} else {
					break;
				}
				$TSNNt1740 = $this->adjacencyList;
				$TLE697 = $TSNNt1740->offsetGet($i);
				$TLE697->purge();
			}
			AbstractGraph::purge();
		}
		protected function insertEdge(Edge $edge)
		{
			$TLE698 = $edge->getV0();
			$v = $TLE698->getNumber();
			$TSNNt1741 = $this->adjacencyList;
			$TLE699 = $TSNNt1741->offsetGet($v);
			$TLE699->append($edge);
			$TSNNt1742 = $this->numberOfEdges;
			++$TSNNt1742;
			$this->numberOfEdges = $TSNNt1742;
		}
		public function getEdge($v, $w)
		{
			$TLE700 = 0;
			$TLE98 = ($v < $TLE700);
			if ($TLE98) {
				$TEF99 = $TLE98;
			} else {
				$TSNNt1743 = $this->numberOfVertices;
				$TEF99 = ($TSNNt1743 <= $v);
			}
			$TLE701 = (bool) $TEF99;
			if ($TLE701) {
				$TLE702 = new IndexError();
				throw $TLE702;
			}
			$TLE703 = 0;
			$TLE100 = ($w < $TLE703);
			if ($TLE100) {
				$TEF101 = $TLE100;
			} else {
				$TSNNt1744 = $this->numberOfVertices;
				$TEF101 = ($TSNNt1744 <= $w);
			}
			$TLE704 = (bool) $TEF101;
			if ($TLE704) {
				$TLE705 = new IndexError();
				throw $TLE705;
			}
			$TSNNt1745 = $this->adjacencyList;
			$TLE706 = $TSNNt1745->offsetGet($v);
			$ptr = $TLE706->getHead();
			$ElcfPF20 = True;
			while (True) {
				if ($ElcfPF20) {
					$ElcfPF20 = False;
				} else {
					$ptr = $ptr->getNext();
				}
				$TLE707 = NULL;
				$TLE708 = ($ptr !== $TLE707);
				if ($TLE708) {
				} else {
					break;
				}
				$edge = $ptr->getDatum();
				$TLE709 = $edge->getV1();
				$TLE710 = $TLE709->getNumber();
				$TLE711 = ($TLE710 == $w);
				if ($TLE711) {
					return $edge;
				}
			}
			$TLE712 = new ArgumentError();
			throw $TLE712;
		}
		public function isEdge($v, $w)
		{
			$TLE713 = 0;
			$TLE102 = ($v < $TLE713);
			if ($TLE102) {
				$TEF103 = $TLE102;
			} else {
				$TSNNt1746 = $this->numberOfVertices;
				$TEF103 = ($TSNNt1746 <= $v);
			}
			$TLE714 = (bool) $TEF103;
			if ($TLE714) {
				$TLE715 = new IndexError();
				throw $TLE715;
			}
			$TLE716 = 0;
			$TLE104 = ($w < $TLE716);
			if ($TLE104) {
				$TEF105 = $TLE104;
			} else {
				$TSNNt1747 = $this->numberOfVertices;
				$TEF105 = ($TSNNt1747 <= $w);
			}
			$TLE717 = (bool) $TEF105;
			if ($TLE717) {
				$TLE718 = new IndexError();
				throw $TLE718;
			}
			$TSNNt1748 = $this->adjacencyList;
			$TLE719 = $TSNNt1748->offsetGet($v);
			$ptr = $TLE719->getHead();
			$ElcfPF21 = True;
			while (True) {
				if ($ElcfPF21) {
					$ElcfPF21 = False;
				} else {
					$ptr = $ptr->getNext();
				}
				$TLE720 = NULL;
				$TLE721 = ($ptr !== $TLE720);
				if ($TLE721) {
				} else {
					break;
				}
				$edge = $ptr->getDatum();
				$TLE722 = $edge->getV1();
				$TLE723 = $TLE722->getNumber();
				$TLE724 = ($TLE723 == $w);
				if ($TLE724) {
					$TLE725 = True;
					return $TLE725;
				}
			}
			$TLE726 = False;
			return $TLE726;
		}
		public function getEdges()
		{
			$TLE727 = new GraphAsLists_EdgeAggregate($this);
			return $TLE727;
		}
		public function getEmanatingEdges($v)
		{
			$TLE728 = new GraphAsLists_EmanatingEdgeAggregate($this, $v);
			return $TLE728;
		}
		public function getIncidentEdges($w)
		{
			$TLE729 = new MethodNotImplementedException();
			throw $TLE729;
		}
		protected function compareTo(IComparable $arg)
		{
			$TLE730 = new MethodNotImplementedException();
			throw $TLE730;
		}
	}
	abstract class AbstractDigraph extends AbstractGraph implements IDigraph
	{
		public function __construct($size)
		{
			AbstractGraph::__construct();
		}
	}
	class DigraphAsLists extends GraphAsLists implements IDigraph
	{
		public function __construct($size = 0)
		{
			GraphAsLists::__construct($size);
		}
		protected function insertEdge(Edge $edge)
		{
			$TLE731 = $edge->getV0();
			$v = $TLE731->getNumber();
			$TSNNt1749 = $this->adjacencyList;
			$TLE732 = $TSNNt1749->offsetGet($v);
			$TLE732->append($edge);
			$TSNNt1750 = $this->numberOfEdges;
			++$TSNNt1750;
			$this->numberOfEdges = $TSNNt1750;
		}
	}
	interface IMatrix extends IObject, ArrayAccess
	{
		function getNumRows();
		function getNumCols();
		function getTranspose();
		function plus(IMatrix $matrix);
		function times(IMatrix $matrix);
	}
	abstract class AbstractMatrix extends AbstractObject implements IMatrix
	{
		protected $numRows = 0;
		protected $numCols = 0;
		public function __construct($numRows, $numCols)
		{
			AbstractObject::__construct();
			$TSNNt1751 = $numRows;
			$this->numRows = $TSNNt1751;
			$TSNNt1752 = $numCols;
			$this->numCols = $TSNNt1752;
		}
		public function getNumRows()
		{
			$TSNNt1753 = $this->numRows;
			return $TSNNt1753;
		}
		public function getNumCols()
		{
			$TSNNt1754 = $this->numCols;
			return $TSNNt1754;
		}
		public function __toString()
		{
			$s = '';
			$i = 0;
			$ElcfPF23 = True;
			while (True) {
				if ($ElcfPF23) {
					$ElcfPF23 = False;
				} else {
					++$i;
				}
				$TSNNt1755 = $this->numRows;
				$TLE733 = ($i < $TSNNt1755);
				if ($TLE733) {
				} else {
					break;
				}
				$j = 0;
				$ElcfPF22 = True;
				while (True) {
					if ($ElcfPF22) {
						$ElcfPF22 = False;
					} else {
						++$j;
					}
					$TSNNt1756 = $this->numCols;
					$TLE734 = ($j < $TSNNt1756);
					if ($TLE734) {
					} else {
						break;
					}
					unset($TSa735);
					$TSa735 = (array) $TSa735;
					$TLE1263 = 0;
					$TSNNi1757 = $i;
					$TSa735[$TLE1263] = $TSNNi1757;
					$TLE1264 = 1;
					$TSNNi1758 = $j;
					$TSa735[$TLE1264] = $TSNNi1758;
					$TLE736 = $this->offsetGet($TSa735);
					$TLE737 = str($TLE736);
					$TLE738 = ' ';
					$TLE739 = ($TLE737 . $TLE738);
					$s = ($s . $TLE739);
				}
				$TLE740 = "\n";
				$s = ($s . $TLE740);
			}
			return $s;
		}
		public static function test(IMatrix $mat)
		{
			$TLE741 = "Matrix test program.\n";
			printf($TLE741);
			try {
				$k = 0;
				$i = 0;
				$ElcfPF25 = True;
				while (True) {
					if ($ElcfPF25) {
						$ElcfPF25 = False;
					} else {
						++$i;
					}
					$TLE742 = $mat->getNumRows();
					$TLE743 = ($i < $TLE742);
					if ($TLE743) {
					} else {
						break;
					}
					$j = 0;
					$ElcfPF24 = True;
					while (True) {
						if ($ElcfPF24) {
							$ElcfPF24 = False;
						} else {
							++$j;
						}
						$TLE744 = $mat->getNumCols();
						$TLE745 = ($j < $TLE744);
						if ($TLE745) {
						} else {
							break;
						}
						unset($TSa746);
						$TSa746 = (array) $TSa746;
						$TLE1265 = 0;
						$TSNNi1759 = $i;
						$TSa746[$TLE1265] = $TSNNi1759;
						$TLE1266 = 1;
						$TSNNi1760 = $j;
						$TSa746[$TLE1266] = $TSNNi1760;
						$mat[$TSa746] = $k;
						++$k;
					}
				}
				$TLE747 = "%s\n";
				$TLE748 = str($mat);
				printf($TLE747, $TLE748);
				$mat = $mat->plus($mat);
				$TLE749 = "%s\n";
				$TLE750 = str($mat);
				printf($TLE749, $TLE750);
			} catch (Exception $e) {
				$TLE751 = "Caught %s\n";
				$TLE752 = $e->getMessage();
				printf($TLE751, $TLE752);
			}
		}
		public static function testTranspose(IMatrix $mat)
		{
			$TLE753 = "Matrix transpose test program.\n";
			printf($TLE753);
			try {
				$TLE754 = 0;
				$TLE755 = 0;
				unset($TSa756);
				$TSa756 = (array) $TSa756;
				$TLE1267 = 0;
				$TSNNi1761 = $TLE754;
				$TSa756[$TLE1267] = $TSNNi1761;
				$TLE1268 = 1;
				$TSNNi1762 = $TLE755;
				$TSa756[$TLE1268] = $TSNNi1762;
				$TLE757 = 31;
				$mat[$TSa756] = $TLE757;
				$TLE758 = 0;
				$TLE759 = 2;
				unset($TSa760);
				$TSa760 = (array) $TSa760;
				$TLE1269 = 0;
				$TSNNi1763 = $TLE758;
				$TSa760[$TLE1269] = $TSNNi1763;
				$TLE1270 = 1;
				$TSNNi1764 = $TLE759;
				$TSa760[$TLE1270] = $TSNNi1764;
				$TLE761 = 41;
				$mat[$TSa760] = $TLE761;
				$TLE762 = 0;
				$TLE763 = 3;
				unset($TSa764);
				$TSa764 = (array) $TSa764;
				$TLE1271 = 0;
				$TSNNi1765 = $TLE762;
				$TSa764[$TLE1271] = $TSNNi1765;
				$TLE1272 = 1;
				$TSNNi1766 = $TLE763;
				$TSa764[$TLE1272] = $TSNNi1766;
				$TLE765 = 59;
				$mat[$TSa764] = $TLE765;
				$TLE766 = 1;
				$TLE767 = 1;
				unset($TSa768);
				$TSa768 = (array) $TSa768;
				$TLE1273 = 0;
				$TSNNi1767 = $TLE766;
				$TSa768[$TLE1273] = $TSNNi1767;
				$TLE1274 = 1;
				$TSNNi1768 = $TLE767;
				$TSa768[$TLE1274] = $TSNNi1768;
				$TLE769 = 26;
				$mat[$TSa768] = $TLE769;
				$TLE770 = 2;
				$TLE771 = 3;
				unset($TSa772);
				$TSa772 = (array) $TSa772;
				$TLE1275 = 0;
				$TSNNi1769 = $TLE770;
				$TSa772[$TLE1275] = $TSNNi1769;
				$TLE1276 = 1;
				$TSNNi1770 = $TLE771;
				$TSa772[$TLE1276] = $TSNNi1770;
				$TLE773 = 53;
				$mat[$TSa772] = $TLE773;
				$TLE774 = 2;
				$TLE775 = 4;
				unset($TSa776);
				$TSa776 = (array) $TSa776;
				$TLE1277 = 0;
				$TSNNi1771 = $TLE774;
				$TSa776[$TLE1277] = $TSNNi1771;
				$TLE1278 = 1;
				$TSNNi1772 = $TLE775;
				$TSa776[$TLE1278] = $TSNNi1772;
				$TLE777 = 58;
				$mat[$TSa776] = $TLE777;
				$TLE778 = 4;
				$TLE779 = 2;
				unset($TSa780);
				$TSa780 = (array) $TSa780;
				$TLE1279 = 0;
				$TSNNi1773 = $TLE778;
				$TSa780[$TLE1279] = $TSNNi1773;
				$TLE1280 = 1;
				$TSNNi1774 = $TLE779;
				$TSa780[$TLE1280] = $TSNNi1774;
				$TLE781 = 97;
				$mat[$TSa780] = $TLE781;
				$TLE782 = 5;
				$TLE783 = 1;
				unset($TSa784);
				$TSa784 = (array) $TSa784;
				$TLE1281 = 0;
				$TSNNi1775 = $TLE782;
				$TSa784[$TLE1281] = $TSNNi1775;
				$TLE1282 = 1;
				$TSNNi1776 = $TLE783;
				$TSa784[$TLE1282] = $TSNNi1776;
				$TLE785 = 93;
				$mat[$TSa784] = $TLE785;
				$TLE786 = 5;
				$TLE787 = 5;
				unset($TSa788);
				$TSa788 = (array) $TSa788;
				$TLE1283 = 0;
				$TSNNi1777 = $TLE786;
				$TSa788[$TLE1283] = $TSNNi1777;
				$TLE1284 = 1;
				$TSNNi1778 = $TLE787;
				$TSa788[$TLE1284] = $TSNNi1778;
				$TLE789 = 23;
				$mat[$TSa788] = $TLE789;
				$TLE790 = "%s\n";
				$TLE791 = str($mat);
				printf($TLE790, $TLE791);
				$TLE792 = 2;
				$TLE793 = 4;
				unset($TSa794);
				$TSa794 = (array) $TSa794;
				$TLE1285 = 0;
				$TSNNi1779 = $TLE792;
				$TSa794[$TLE1285] = $TSNNi1779;
				$TLE1286 = 1;
				$TSNNi1780 = $TLE793;
				$TSa794[$TLE1286] = $TSNNi1780;
				$TLE795 = 0;
				$mat[$TSa794] = $TLE795;
				$TLE796 = 5;
				$TLE797 = 3;
				unset($TSa798);
				$TSa798 = (array) $TSa798;
				$TLE1287 = 0;
				$TSNNi1781 = $TLE796;
				$TSa798[$TLE1287] = $TSNNi1781;
				$TLE1288 = 1;
				$TSNNi1782 = $TLE797;
				$TSa798[$TLE1288] = $TSNNi1782;
				$TLE799 = 0;
				$mat[$TSa798] = $TLE799;
				$mat = $mat->getTranspose();
				$TLE800 = "%s\n";
				$TLE801 = str($mat);
				printf($TLE800, $TLE801);
			} catch (Exception $e) {
				$TLE802 = "Caught %s\n";
				$TLE803 = $e->getMessage();
				printf($TLE802, $TLE803);
			}
		}
		public static function testTimes(IMatrix $mat1, IMatrix $mat2)
		{
			try {
				$TLE804 = "Matrix multiply test program.\n";
				printf($TLE804);
				$TLE805 = 0;
				$TLE806 = 0;
				unset($TSa807);
				$TSa807 = (array) $TSa807;
				$TLE1289 = 0;
				$TSNNi1783 = $TLE805;
				$TSa807[$TLE1289] = $TSNNi1783;
				$TLE1290 = 1;
				$TSNNi1784 = $TLE806;
				$TSa807[$TLE1290] = $TSNNi1784;
				$TLE808 = 1;
				$mat1[$TSa807] = $TLE808;
				$TLE809 = 0;
				$TLE810 = 1;
				unset($TSa811);
				$TSa811 = (array) $TSa811;
				$TLE1291 = 0;
				$TSNNi1785 = $TLE809;
				$TSa811[$TLE1291] = $TSNNi1785;
				$TLE1292 = 1;
				$TSNNi1786 = $TLE810;
				$TSa811[$TLE1292] = $TSNNi1786;
				$TLE812 = 2;
				$mat1[$TSa811] = $TLE812;
				$TLE813 = 0;
				$TLE814 = 2;
				unset($TSa815);
				$TSa815 = (array) $TSa815;
				$TLE1293 = 0;
				$TSNNi1787 = $TLE813;
				$TSa815[$TLE1293] = $TSNNi1787;
				$TLE1294 = 1;
				$TSNNi1788 = $TLE814;
				$TSa815[$TLE1294] = $TSNNi1788;
				$TLE816 = 3;
				$mat1[$TSa815] = $TLE816;
				$TLE817 = 0;
				$TLE818 = 0;
				unset($TSa819);
				$TSa819 = (array) $TSa819;
				$TLE1295 = 0;
				$TSNNi1789 = $TLE817;
				$TSa819[$TLE1295] = $TSNNi1789;
				$TLE1296 = 1;
				$TSNNi1790 = $TLE818;
				$TSa819[$TLE1296] = $TSNNi1790;
				$TLE820 = 1;
				$mat2[$TSa819] = $TLE820;
				$TLE821 = 1;
				$TLE822 = 0;
				unset($TSa823);
				$TSa823 = (array) $TSa823;
				$TLE1297 = 0;
				$TSNNi1791 = $TLE821;
				$TSa823[$TLE1297] = $TSNNi1791;
				$TLE1298 = 1;
				$TSNNi1792 = $TLE822;
				$TSa823[$TLE1298] = $TSNNi1792;
				$TLE824 = 2;
				$mat2[$TSa823] = $TLE824;
				$TLE825 = 2;
				$TLE826 = 0;
				unset($TSa827);
				$TSa827 = (array) $TSa827;
				$TLE1299 = 0;
				$TSNNi1793 = $TLE825;
				$TSa827[$TLE1299] = $TSNNi1793;
				$TLE1300 = 1;
				$TSNNi1794 = $TLE826;
				$TSa827[$TLE1300] = $TSNNi1794;
				$TLE828 = 3;
				$mat2[$TSa827] = $TLE828;
				$TLE829 = "%s\n";
				$TLE830 = str($mat1);
				printf($TLE829, $TLE830);
				$TLE831 = "%s\n";
				$TLE832 = str($mat2);
				printf($TLE831, $TLE832);
				$mat1 = $mat2->times($mat1);
				$TLE833 = "%s\n";
				$TLE834 = str($mat1);
				printf($TLE833, $TLE834);
			} catch (Exception $e) {
				$TLE835 = "Caught %s\n";
				$TLE836 = $e->getMessage();
				printf($TLE835, $TLE836);
			}
		}
		public static function main($args)
		{
			$TLE837 = "AbstractMatrix main program.\n";
			printf($TLE837);
			$status = 0;
			return $status;
		}
	}
	class MultiDimensionalArray extends AbstractObject implements ArrayAccess
	{
		protected $dimensions = NULL;
		protected $factors = NULL;
		protected $data = NULL;
		public function __construct($dimensions)
		{
			AbstractObject::__construct();
			$length = sizeof($dimensions);
			$TLE838 = new BasicArray($length);
			$TSNNt1795 = $TLE838;
			$this->dimensions = $TSNNt1795;
			$TLE839 = new BasicArray($length);
			$TSNNt1796 = $TLE839;
			$this->factors = $TSNNt1796;
			$product = 1;
			$TLE840 = 1;
			$i = ($length - $TLE840);
			$ElcfPF26 = True;
			while (True) {
				if ($ElcfPF26) {
					$ElcfPF26 = False;
				} else {
					--$i;
				}
				$TLE841 = 0;
				$TLE842 = ($TLE841 <= $i);
				if ($TLE842) {
				} else {
					break;
				}
				$TSNNt1797 = $this->dimensions;
				$TSNNi1798 = $dimensions[$i];
				$TSNNt1797->offsetSet($i, $TSNNi1798);
				$TSNNt1799 = $this->factors;
				$TSNNt1799->offsetSet($i, $product);
				$TSNNt1800 = $this->dimensions;
				$TLE843 = $TSNNt1800->offsetGet($i);
				$product = ($product * $TLE843);
			}
			$TLE844 = new BasicArray($product);
			$TSNNt1801 = $TLE844;
			$this->data = $TSNNt1801;
		}
		public function __toString()
		{
			$TLE845 = "MultiDimensionalArray";
			return $TLE845;
		}
		private function getOffset($indices)
		{
			$TLE846 = sizeof($indices);
			$TSNNt1802 = $this->dimensions;
			$TLE847 = $TSNNt1802->getLength();
			$TLE848 = ($TLE846 != $TLE847);
			if ($TLE848) {
				$TLE849 = new IndexError();
				throw $TLE849;
			}
			$offset = 0;
			$i = 0;
			$ElcfPF27 = True;
			while (True) {
				if ($ElcfPF27) {
					$ElcfPF27 = False;
				} else {
					++$i;
				}
				$TSNNt1803 = $this->dimensions;
				$TLE850 = $TSNNt1803->getLength();
				$TLE851 = ($i < $TLE850);
				if ($TLE851) {
				} else {
					break;
				}
				$TLE852 = 0;
				$TSNNi1804 = $indices[$i];
				$TLE106 = ($TSNNi1804 < $TLE852);
				if ($TLE106) {
					$TEF107 = $TLE106;
				} else {
					$TSNNt1805 = $this->dimensions;
					$TLE853 = $TSNNt1805->offsetGet($i);
					$TSNNi1806 = $indices[$i];
					$TEF107 = ($TLE853 <= $TSNNi1806);
				}
				$TLE854 = (bool) $TEF107;
				if ($TLE854) {
					$TLE855 = new IndexError();
					throw $TLE855;
				}
				$TSNNt1807 = $this->factors;
				$TLE856 = $TSNNt1807->offsetGet($i);
				$TSNNi1808 = $indices[$i];
				$TLE857 = ($TLE856 * $TSNNi1808);
				$offset = ($offset + $TLE857);
			}
			return $offset;
		}
		public function offsetExists($indices)
		{
			$this->getOffset($indices);
		}
		public function offsetGet($indices)
		{
			$TLE858 = $this->getOffset($indices);
			$TSNNt1809 = $this->data;
			$TLE859 = $TSNNt1809->offsetGet($TLE858);
			return $TLE859;
		}
		public function offsetSet($indices, $value)
		{
			$TLE860 = $this->getOffset($indices);
			$TSNNt1810 = $this->data;
			$TSNNt1810->offsetSet($TLE860, $value);
		}
		public function offsetUnset($indices)
		{
			$TLE861 = $this->getOffset($indices);
			$TLE862 = NULL;
			$TSNNt1811 = $this->data;
			$TSNNt1811->offsetSet($TLE861, $TLE862);
		}
	}
	class DenseMatrix extends AbstractMatrix
	{
		protected $array = NULL;
		public function __construct($rows, $columns)
		{
			AbstractMatrix::__construct($rows, $columns);
			unset($TSa863);
			$TSa863 = (array) $TSa863;
			$TLE1301 = 0;
			$TSNNi1812 = $rows;
			$TSa863[$TLE1301] = $TSNNi1812;
			$TLE1302 = 1;
			$TSNNi1813 = $columns;
			$TSa863[$TLE1302] = $TSNNi1813;
			$TLE864 = new MultiDimensionalArray($TSa863);
			$TSNNt1814 = $TLE864;
			$this->array = $TSNNt1814;
		}
		public function offsetExists($indices)
		{
			$TSNNt1815 = $this->array;
			$TLE865 = $TSNNt1815->offsetExists($indices);
			return $TLE865;
		}
		public function offsetGet($indices)
		{
			$TSNNt1816 = $this->array;
			$TLE866 = $TSNNt1816->offsetGet($indices);
			return $TLE866;
		}
		public function offsetSet($indices, $value)
		{
			$TSNNt1817 = $this->array;
			$TSNNt1817->offsetSet($indices, $value);
		}
		public function offsetUnset($indices)
		{
			$TLE867 = NULL;
			$TSNNt1818 = $this->array;
			$TSNNt1818->offsetSet($indices, $TLE867);
		}
		public function times(IMatrix $mat)
		{
			$TLE868 = $mat instanceof DenseMatrix;
			$TLE108 = !$TLE868;
			if ($TLE108) {
				$TEF109 = $TLE108;
			} else {
				$TLE869 = $this->getNumCols();
				$TLE870 = $mat->getNumRows();
				$TEF109 = ($TLE869 != $TLE870);
			}
			$TLE871 = (bool) $TEF109;
			if ($TLE871) {
				$TLE872 = new ArgumentError();
				throw $TLE872;
			}
			$TLE873 = $this->getNumRows();
			$TLE874 = $mat->getNumCols();
			$result = new DenseMatrix($TLE873, $TLE874);
			$i = 0;
			$ElcfPF30 = True;
			while (True) {
				if ($ElcfPF30) {
					$ElcfPF30 = False;
				} else {
					++$i;
				}
				$TLE875 = $this->getNumRows();
				$TLE876 = ($i < $TLE875);
				if ($TLE876) {
				} else {
					break;
				}
				$j = 0;
				$ElcfPF29 = True;
				while (True) {
					if ($ElcfPF29) {
						$ElcfPF29 = False;
					} else {
						++$j;
					}
					$TLE877 = $mat->getNumCols();
					$TLE878 = ($j < $TLE877);
					if ($TLE878) {
					} else {
						break;
					}
					$sum = 0;
					$k = 0;
					$ElcfPF28 = True;
					while (True) {
						if ($ElcfPF28) {
							$ElcfPF28 = False;
						} else {
							++$k;
						}
						$TLE879 = $this->getNumCols();
						$TLE880 = ($k < $TLE879);
						if ($TLE880) {
						} else {
							break;
						}
						unset($TSa881);
						$TSa881 = (array) $TSa881;
						$TLE1303 = 0;
						$TSNNi1819 = $i;
						$TSa881[$TLE1303] = $TSNNi1819;
						$TLE1304 = 1;
						$TSNNi1820 = $k;
						$TSa881[$TLE1304] = $TSNNi1820;
						$TLE882 = $this->offsetGet($TSa881);
						unset($TSa883);
						$TSa883 = (array) $TSa883;
						$TLE1305 = 0;
						$TSNNi1821 = $k;
						$TSa883[$TLE1305] = $TSNNi1821;
						$TLE1306 = 1;
						$TSNNi1822 = $j;
						$TSa883[$TLE1306] = $TSNNi1822;
						$TLE884 = $mat->offsetGet($TSa883);
						$TLE885 = ($TLE882 * $TLE884);
						$sum = ($sum + $TLE885);
					}
					unset($TSa886);
					$TSa886 = (array) $TSa886;
					$TLE1307 = 0;
					$TSNNi1823 = $i;
					$TSa886[$TLE1307] = $TSNNi1823;
					$TLE1308 = 1;
					$TSNNi1824 = $j;
					$TSa886[$TLE1308] = $TSNNi1824;
					$result->offsetSet($TSa886, $sum);
				}
			}
			return $result;
		}
		public function plus(IMatrix $mat)
		{
			$TLE887 = $mat instanceof DenseMatrix;
			$TLE110 = !$TLE887;
			if ($TLE110) {
				$TEF111 = $TLE110;
			} else {
				$TLE888 = $this->getNumRows();
				$TLE889 = $mat->getNumRows();
				$TEF111 = ($TLE888 != $TLE889);
			}
			$TLE112 = (bool) $TEF111;
			if ($TLE112) {
				$TEF113 = $TLE112;
			} else {
				$TLE890 = $this->getNumCols();
				$TLE891 = $mat->getNumCols();
				$TEF113 = ($TLE890 != $TLE891);
			}
			$TLE892 = (bool) $TEF113;
			if ($TLE892) {
				$TLE893 = new ArgumentError();
				throw $TLE893;
			}
			$TLE894 = $this->getNumRows();
			$TLE895 = $this->getNumCols();
			$result = new DenseMatrix($TLE894, $TLE895);
			$i = 0;
			$ElcfPF32 = True;
			while (True) {
				if ($ElcfPF32) {
					$ElcfPF32 = False;
				} else {
					++$i;
				}
				$TLE896 = $this->getNumRows();
				$TLE897 = ($i < $TLE896);
				if ($TLE897) {
				} else {
					break;
				}
				$j = 0;
				$ElcfPF31 = True;
				while (True) {
					if ($ElcfPF31) {
						$ElcfPF31 = False;
					} else {
						++$j;
					}
					$TLE898 = $this->getNumCols();
					$TLE899 = ($j < $TLE898);
					if ($TLE899) {
					} else {
						break;
					}
					unset($TSa900);
					$TSa900 = (array) $TSa900;
					$TLE1309 = 0;
					$TSNNi1825 = $i;
					$TSa900[$TLE1309] = $TSNNi1825;
					$TLE1310 = 1;
					$TSNNi1826 = $j;
					$TSa900[$TLE1310] = $TSNNi1826;
					unset($TSa901);
					$TSa901 = (array) $TSa901;
					$TLE1311 = 0;
					$TSNNi1827 = $i;
					$TSa901[$TLE1311] = $TSNNi1827;
					$TLE1312 = 1;
					$TSNNi1828 = $j;
					$TSa901[$TLE1312] = $TSNNi1828;
					$TLE902 = $this->offsetGet($TSa901);
					unset($TSa903);
					$TSa903 = (array) $TSa903;
					$TLE1313 = 0;
					$TSNNi1829 = $i;
					$TSa903[$TLE1313] = $TSNNi1829;
					$TLE1314 = 1;
					$TSNNi1830 = $j;
					$TSa903[$TLE1314] = $TSNNi1830;
					$TLE904 = $mat->offsetGet($TSa903);
					$TLE905 = ($TLE902 + $TLE904);
					$result->offsetSet($TSa900, $TLE905);
				}
			}
			return $result;
		}
		public function getTranspose()
		{
			$TLE906 = $this->getNumCols();
			$TLE907 = $this->getNumRows();
			$result = new DenseMatrix($TLE906, $TLE907);
			$i = 0;
			$ElcfPF34 = True;
			while (True) {
				if ($ElcfPF34) {
					$ElcfPF34 = False;
				} else {
					++$i;
				}
				$TLE908 = $this->getNumRows();
				$TLE909 = ($i < $TLE908);
				if ($TLE909) {
				} else {
					break;
				}
				$j = 0;
				$ElcfPF33 = True;
				while (True) {
					if ($ElcfPF33) {
						$ElcfPF33 = False;
					} else {
						++$j;
					}
					$TLE910 = $this->getNumCols();
					$TLE911 = ($j < $TLE910);
					if ($TLE911) {
					} else {
						break;
					}
					unset($TSa912);
					$TSa912 = (array) $TSa912;
					$TLE1315 = 0;
					$TSNNi1831 = $j;
					$TSa912[$TLE1315] = $TSNNi1831;
					$TLE1316 = 1;
					$TSNNi1832 = $i;
					$TSa912[$TLE1316] = $TSNNi1832;
					unset($TSa913);
					$TSa913 = (array) $TSa913;
					$TLE1317 = 0;
					$TSNNi1833 = $i;
					$TSa913[$TLE1317] = $TSNNi1833;
					$TLE1318 = 1;
					$TSNNi1834 = $j;
					$TSa913[$TLE1318] = $TSNNi1834;
					$TLE914 = $this->offsetGet($TSa913);
					$result->offsetSet($TSa912, $TLE914);
				}
			}
			return $result;
		}
	}
	class Limits
	{
		const INTBITS = 32;
		const MAXINT = 2147483647;
		const MININT = -2147483648;
	}
	class GraphAsMatrix_EdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $w = 0;
		public function __construct(GraphAsMatrix $graph)
		{
			AbstractIterator::__construct();
			$TSNNt1835 = $graph;
			$this->graph = $TSNNt1835;
			$TSNNt1836 = $this->graph;
			$matrix = $TSNNt1836->getMatrix();
			$breakOuter = False;
			$TLE915 = 0;
			$TSNNt1837 = $TLE915;
			$this->v = $TSNNt1837;
			$ElcfPF36 = True;
			while (True) {
				if ($ElcfPF36) {
					$ElcfPF36 = False;
				} else {
					$TSNNt1838 = $this->v;
					++$TSNNt1838;
					$this->v = $TSNNt1838;
				}
				$TSNNt1839 = $this->graph;
				$TLE916 = $TSNNt1839->getNumberOfVertices();
				$TSNNt1840 = $this->v;
				$TLE917 = ($TSNNt1840 < $TLE916);
				if ($TLE917) {
				} else {
					break;
				}
				$TLE918 = 1;
				$TSNNt1841 = $this->v;
				$TLE919 = ($TSNNt1841 + $TLE918);
				$TSNNt1842 = $TLE919;
				$this->w = $TSNNt1842;
				$ElcfPF35 = True;
				while (True) {
					if ($ElcfPF35) {
						$ElcfPF35 = False;
					} else {
						$TSNNt1843 = $this->w;
						++$TSNNt1843;
						$this->w = $TSNNt1843;
					}
					$TSNNt1844 = $this->graph;
					$TLE920 = $TSNNt1844->getNumberOfVertices();
					$TSNNt1845 = $this->w;
					$TLE921 = ($TSNNt1845 < $TLE920);
					if ($TLE921) {
					} else {
						break;
					}
					unset($TSa922);
					$TSa922 = (array) $TSa922;
					$TLE1319 = 0;
					$TSNNt1847 = $this->v;
					$TSNNi1846 = $TSNNt1847;
					$TSa922[$TLE1319] = $TSNNi1846;
					$TLE1320 = 1;
					$TSNNt1849 = $this->w;
					$TSNNi1848 = $TSNNt1849;
					$TSa922[$TLE1320] = $TSNNi1848;
					$TLE923 = $matrix->offsetGet($TSa922);
					$TLE924 = NULL;
					$TLE925 = ($TLE923 !== $TLE924);
					if ($TLE925) {
						$breakOuter = True;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = False;
					break;
				}
			}
		}
		public function valid()
		{
			$TSNNt1850 = $this->graph;
			$TLE926 = $TSNNt1850->getNumberOfVertices();
			$TSNNt1851 = $this->v;
			$TLE114 = ($TSNNt1851 < $TLE926);
			if ($TLE114) {
				$TSNNt1852 = $this->graph;
				$TLE927 = $TSNNt1852->getNumberOfVertices();
				$TSNNt1853 = $this->w;
				$TEF115 = ($TSNNt1853 < $TLE927);
			} else {
				$TEF115 = $TLE114;
			}
			$TLE928 = (bool) $TEF115;
			return $TLE928;
		}
		public function key()
		{
			$TSNNt1854 = $this->graph;
			$TLE929 = $TSNNt1854->getNumberOfVertices();
			$TSNNt1855 = $this->v;
			$TLE930 = ($TSNNt1855 * $TLE929);
			$TSNNt1856 = $this->w;
			$TLE931 = ($TLE930 + $TSNNt1856);
			return $TLE931;
		}
		public function current()
		{
			$TSNNt1857 = $this->graph;
			$matrix = $TSNNt1857->getMatrix();
			unset($TSa932);
			$TSa932 = (array) $TSa932;
			$TLE1321 = 0;
			$TSNNt1859 = $this->v;
			$TSNNi1858 = $TSNNt1859;
			$TSa932[$TLE1321] = $TSNNi1858;
			$TLE1322 = 1;
			$TSNNt1861 = $this->w;
			$TSNNi1860 = $TSNNt1861;
			$TSa932[$TLE1322] = $TSNNi1860;
			$TLE933 = $matrix->offsetGet($TSa932);
			return $TLE933;
		}
		public function next()
		{
			$TSNNt1862 = $this->graph;
			$matrix = $TSNNt1862->getMatrix();
			$TSNNt1863 = $this->w;
			++$TSNNt1863;
			$this->w = $TSNNt1863;
			$ElcfPF37 = True;
			while (True) {
				if ($ElcfPF37) {
					$ElcfPF37 = False;
				} else {
					$TSNNt1864 = $this->w;
					++$TSNNt1864;
					$this->w = $TSNNt1864;
				}
				$TSNNt1865 = $this->graph;
				$TLE934 = $TSNNt1865->getNumberOfVertices();
				$TSNNt1866 = $this->w;
				$TLE935 = ($TSNNt1866 < $TLE934);
				if ($TLE935) {
				} else {
					break;
				}
				unset($TSa936);
				$TSa936 = (array) $TSa936;
				$TLE1323 = 0;
				$TSNNt1868 = $this->v;
				$TSNNi1867 = $TSNNt1868;
				$TSa936[$TLE1323] = $TSNNi1867;
				$TLE1324 = 1;
				$TSNNt1870 = $this->w;
				$TSNNi1869 = $TSNNt1870;
				$TSa936[$TLE1324] = $TSNNi1869;
				$TLE937 = $matrix->offsetGet($TSa936);
				$TLE938 = NULL;
				$TLE939 = ($TLE937 !== $TLE938);
				if ($TLE939) {
					$TLE940 = NULL;
					return $TLE940;
				}
			}
			$TSNNt1871 = $this->v;
			++$TSNNt1871;
			$this->v = $TSNNt1871;
			$ElcfPF39 = True;
			while (True) {
				if ($ElcfPF39) {
					$ElcfPF39 = False;
				} else {
					$TSNNt1872 = $this->v;
					++$TSNNt1872;
					$this->v = $TSNNt1872;
				}
				$TSNNt1873 = $this->graph;
				$TLE941 = $TSNNt1873->getNumberOfVertices();
				$TSNNt1874 = $this->v;
				$TLE942 = ($TSNNt1874 < $TLE941);
				if ($TLE942) {
				} else {
					break;
				}
				$TLE943 = 1;
				$TSNNt1875 = $this->v;
				$TLE944 = ($TSNNt1875 + $TLE943);
				$TSNNt1876 = $TLE944;
				$this->w = $TSNNt1876;
				$ElcfPF38 = True;
				while (True) {
					if ($ElcfPF38) {
						$ElcfPF38 = False;
					} else {
						$TSNNt1877 = $this->w;
						++$TSNNt1877;
						$this->w = $TSNNt1877;
					}
					$TSNNt1878 = $this->graph;
					$TLE945 = $TSNNt1878->getNumberOfVertices();
					$TSNNt1879 = $this->w;
					$TLE946 = ($TSNNt1879 < $TLE945);
					if ($TLE946) {
					} else {
						break;
					}
					unset($TSa947);
					$TSa947 = (array) $TSa947;
					$TLE1325 = 0;
					$TSNNt1881 = $this->v;
					$TSNNi1880 = $TSNNt1881;
					$TSa947[$TLE1325] = $TSNNi1880;
					$TLE1326 = 1;
					$TSNNt1883 = $this->w;
					$TSNNi1882 = $TSNNt1883;
					$TSa947[$TLE1326] = $TSNNi1882;
					$TLE948 = $matrix->offsetGet($TSa947);
					$TLE949 = NULL;
					$TLE950 = ($TLE948 !== $TLE949);
					if ($TLE950) {
						$TLE951 = NULL;
						return $TLE951;
					}
				}
			}
		}
		public function rewind()
		{
			$TSNNt1884 = $this->graph;
			$matrix = $TSNNt1884->getMatrix();
			$breakOuter = False;
			$TLE952 = 0;
			$TSNNt1885 = $TLE952;
			$this->v = $TSNNt1885;
			$ElcfPF41 = True;
			while (True) {
				if ($ElcfPF41) {
					$ElcfPF41 = False;
				} else {
					$TSNNt1886 = $this->v;
					++$TSNNt1886;
					$this->v = $TSNNt1886;
				}
				$TSNNt1887 = $this->graph;
				$TLE953 = $TSNNt1887->getNumberOfVertices();
				$TSNNt1888 = $this->v;
				$TLE954 = ($TSNNt1888 < $TLE953);
				if ($TLE954) {
				} else {
					break;
				}
				$TLE955 = 1;
				$TSNNt1889 = $this->v;
				$TLE956 = ($TSNNt1889 + $TLE955);
				$TSNNt1890 = $TLE956;
				$this->w = $TSNNt1890;
				$ElcfPF40 = True;
				while (True) {
					if ($ElcfPF40) {
						$ElcfPF40 = False;
					} else {
						$TSNNt1891 = $this->w;
						++$TSNNt1891;
						$this->w = $TSNNt1891;
					}
					$TSNNt1892 = $this->graph;
					$TLE957 = $TSNNt1892->getNumberOfVertices();
					$TSNNt1893 = $this->w;
					$TLE958 = ($TSNNt1893 < $TLE957);
					if ($TLE958) {
					} else {
						break;
					}
					unset($TSa959);
					$TSa959 = (array) $TSa959;
					$TLE1327 = 0;
					$TSNNt1895 = $this->v;
					$TSNNi1894 = $TSNNt1895;
					$TSa959[$TLE1327] = $TSNNi1894;
					$TLE1328 = 1;
					$TSNNt1897 = $this->w;
					$TSNNi1896 = $TSNNt1897;
					$TSa959[$TLE1328] = $TSNNi1896;
					$TLE960 = $matrix->offsetGet($TSa959);
					$TLE961 = NULL;
					$TLE962 = ($TLE960 !== $TLE961);
					if ($TLE962) {
						$breakOuter = True;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = False;
					break;
				}
			}
		}
	}
	class GraphAsMatrix_EdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		public function __construct(GraphAsMatrix $graph)
		{
			$TSNNt1898 = $graph;
			$this->graph = $TSNNt1898;
		}
		public function getIterator()
		{
			$TSNNt1899 = $this->graph;
			$TLE963 = new GraphAsMatrix_EdgeIterator($TSNNt1899);
			return $TLE963;
		}
	}
	class GraphAsMatrix_EmanatingEdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $w = 0;
		public function __construct(GraphAsMatrix $graph, $v)
		{
			AbstractIterator::__construct();
			$TSNNt1900 = $graph;
			$this->graph = $TSNNt1900;
			$TSNNt1901 = $v;
			$this->v = $TSNNt1901;
			$TSNNt1902 = $this->graph;
			$matrix = $TSNNt1902->getMatrix();
			$TLE964 = 0;
			$TSNNt1903 = $TLE964;
			$this->w = $TSNNt1903;
			$ElcfPF42 = True;
			while (True) {
				if ($ElcfPF42) {
					$ElcfPF42 = False;
				} else {
					$TSNNt1904 = $this->w;
					++$TSNNt1904;
					$this->w = $TSNNt1904;
				}
				$TSNNt1905 = $this->graph;
				$TLE965 = $TSNNt1905->getNumberOfVertices();
				$TSNNt1906 = $this->w;
				$TLE966 = ($TSNNt1906 < $TLE965);
				if ($TLE966) {
				} else {
					break;
				}
				unset($TSa967);
				$TSa967 = (array) $TSa967;
				$TLE1329 = 0;
				$TSNNt1908 = $this->v;
				$TSNNi1907 = $TSNNt1908;
				$TSa967[$TLE1329] = $TSNNi1907;
				$TLE1330 = 1;
				$TSNNt1910 = $this->w;
				$TSNNi1909 = $TSNNt1910;
				$TSa967[$TLE1330] = $TSNNi1909;
				$TLE968 = $matrix->offsetGet($TSa967);
				$TLE969 = NULL;
				$TLE970 = ($TLE968 !== $TLE969);
				if ($TLE970) {
					break;
				}
			}
		}
		public function valid()
		{
			$TSNNt1911 = $this->graph;
			$TLE971 = $TSNNt1911->getNumberOfVertices();
			$TSNNt1912 = $this->v;
			$TLE116 = ($TSNNt1912 < $TLE971);
			if ($TLE116) {
				$TSNNt1913 = $this->graph;
				$TLE972 = $TSNNt1913->getNumberOfVertices();
				$TSNNt1914 = $this->w;
				$TEF117 = ($TSNNt1914 < $TLE972);
			} else {
				$TEF117 = $TLE116;
			}
			$TLE973 = (bool) $TEF117;
			return $TLE973;
		}
		public function key()
		{
			$TSNNt1915 = $this->graph;
			$TLE974 = $TSNNt1915->getNumberOfVertices();
			$TSNNt1916 = $this->v;
			$TLE975 = ($TSNNt1916 * $TLE974);
			$TSNNt1917 = $this->w;
			$TLE976 = ($TLE975 + $TSNNt1917);
			return $TLE976;
		}
		public function current()
		{
			$TSNNt1918 = $this->graph;
			$matrix = $TSNNt1918->getMatrix();
			unset($TSa977);
			$TSa977 = (array) $TSa977;
			$TLE1331 = 0;
			$TSNNt1920 = $this->v;
			$TSNNi1919 = $TSNNt1920;
			$TSa977[$TLE1331] = $TSNNi1919;
			$TLE1332 = 1;
			$TSNNt1922 = $this->w;
			$TSNNi1921 = $TSNNt1922;
			$TSa977[$TLE1332] = $TSNNi1921;
			$TLE978 = $matrix->offsetGet($TSa977);
			return $TLE978;
		}
		public function next()
		{
			$TSNNt1923 = $this->graph;
			$matrix = $TSNNt1923->getMatrix();
			$TSNNt1924 = $this->w;
			++$TSNNt1924;
			$this->w = $TSNNt1924;
			$ElcfPF43 = True;
			while (True) {
				if ($ElcfPF43) {
					$ElcfPF43 = False;
				} else {
					$TSNNt1925 = $this->w;
					++$TSNNt1925;
					$this->w = $TSNNt1925;
				}
				$TSNNt1926 = $this->graph;
				$TLE979 = $TSNNt1926->getNumberOfVertices();
				$TSNNt1927 = $this->w;
				$TLE980 = ($TSNNt1927 < $TLE979);
				if ($TLE980) {
				} else {
					break;
				}
				unset($TSa981);
				$TSa981 = (array) $TSa981;
				$TLE1333 = 0;
				$TSNNt1929 = $this->v;
				$TSNNi1928 = $TSNNt1929;
				$TSa981[$TLE1333] = $TSNNi1928;
				$TLE1334 = 1;
				$TSNNt1931 = $this->w;
				$TSNNi1930 = $TSNNt1931;
				$TSa981[$TLE1334] = $TSNNi1930;
				$TLE982 = $matrix->offsetGet($TSa981);
				$TLE983 = NULL;
				$TLE984 = ($TLE982 !== $TLE983);
				if ($TLE984) {
					break;
				}
			}
		}
		public function rewind()
		{
			$TSNNt1932 = $this->graph;
			$matrix = $TSNNt1932->getMatrix();
			$TLE985 = 0;
			$TSNNt1933 = $TLE985;
			$this->w = $TSNNt1933;
			$ElcfPF44 = True;
			while (True) {
				if ($ElcfPF44) {
					$ElcfPF44 = False;
				} else {
					$TSNNt1934 = $this->w;
					++$TSNNt1934;
					$this->w = $TSNNt1934;
				}
				$TSNNt1935 = $this->graph;
				$TLE986 = $TSNNt1935->getNumberOfVertices();
				$TSNNt1936 = $this->w;
				$TLE987 = ($TSNNt1936 < $TLE986);
				if ($TLE987) {
				} else {
					break;
				}
				unset($TSa988);
				$TSa988 = (array) $TSa988;
				$TLE1335 = 0;
				$TSNNt1938 = $this->v;
				$TSNNi1937 = $TSNNt1938;
				$TSa988[$TLE1335] = $TSNNi1937;
				$TLE1336 = 1;
				$TSNNt1940 = $this->w;
				$TSNNi1939 = $TSNNt1940;
				$TSa988[$TLE1336] = $TSNNi1939;
				$TLE989 = $matrix->offsetGet($TSa988);
				$TLE990 = NULL;
				$TLE991 = ($TLE989 !== $TLE990);
				if ($TLE991) {
					break;
				}
			}
		}
	}
	class GraphAsMatrix_EmanatingEdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		protected $v = 0;
		public function __construct(GraphAsMatrix $graph, $v)
		{
			$TSNNt1941 = $graph;
			$this->graph = $TSNNt1941;
			$TSNNt1942 = $v;
			$this->v = $TSNNt1942;
		}
		public function getIterator()
		{
			$TSNNt1943 = $this->graph;
			$TSNNt1944 = $this->v;
			$TLE992 = new GraphAsMatrix_EmanatingEdgeIterator($TSNNt1943, $TSNNt1944);
			return $TLE992;
		}
	}
	class GraphAsMatrix_IncidentEdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $w = 0;
		public function __construct(GraphAsMatrix $graph, $w)
		{
			AbstractIterator::__construct();
			$TSNNt1945 = $graph;
			$this->graph = $TSNNt1945;
			$TSNNt1946 = $w;
			$this->w = $TSNNt1946;
			$TSNNt1947 = $this->graph;
			$matrix = $TSNNt1947->getMatrix();
			$TLE993 = 0;
			$TSNNt1948 = $TLE993;
			$this->v = $TSNNt1948;
			$ElcfPF45 = True;
			while (True) {
				if ($ElcfPF45) {
					$ElcfPF45 = False;
				} else {
					$TSNNt1949 = $this->v;
					++$TSNNt1949;
					$this->v = $TSNNt1949;
				}
				$TSNNt1950 = $this->graph;
				$TLE994 = $TSNNt1950->getNumberOfVertices();
				$TSNNt1951 = $this->v;
				$TLE995 = ($TSNNt1951 < $TLE994);
				if ($TLE995) {
				} else {
					break;
				}
				unset($TSa996);
				$TSa996 = (array) $TSa996;
				$TLE1337 = 0;
				$TSNNt1953 = $this->v;
				$TSNNi1952 = $TSNNt1953;
				$TSa996[$TLE1337] = $TSNNi1952;
				$TLE1338 = 1;
				$TSNNt1955 = $this->w;
				$TSNNi1954 = $TSNNt1955;
				$TSa996[$TLE1338] = $TSNNi1954;
				$TLE997 = $matrix->offsetGet($TSa996);
				$TLE998 = NULL;
				$TLE999 = ($TLE997 !== $TLE998);
				if ($TLE999) {
					break;
				}
			}
		}
		public function valid()
		{
			$TSNNt1956 = $this->graph;
			$TLE1000 = $TSNNt1956->getNumberOfVertices();
			$TSNNt1957 = $this->v;
			$TLE118 = ($TSNNt1957 < $TLE1000);
			if ($TLE118) {
				$TSNNt1958 = $this->graph;
				$TLE1001 = $TSNNt1958->getNumberOfVertices();
				$TSNNt1959 = $this->w;
				$TEF119 = ($TSNNt1959 < $TLE1001);
			} else {
				$TEF119 = $TLE118;
			}
			$TLE1002 = (bool) $TEF119;
			return $TLE1002;
		}
		public function key()
		{
			$TSNNt1960 = $this->graph;
			$TLE1003 = $TSNNt1960->getNumberOfVertices();
			$TSNNt1961 = $this->v;
			$TLE1004 = ($TSNNt1961 * $TLE1003);
			$TSNNt1962 = $this->w;
			$TLE1005 = ($TLE1004 + $TSNNt1962);
			return $TLE1005;
		}
		public function current()
		{
			$TSNNt1963 = $this->graph;
			$matrix = $TSNNt1963->getMatrix();
			unset($TSa1006);
			$TSa1006 = (array) $TSa1006;
			$TLE1339 = 0;
			$TSNNt1965 = $this->v;
			$TSNNi1964 = $TSNNt1965;
			$TSa1006[$TLE1339] = $TSNNi1964;
			$TLE1340 = 1;
			$TSNNt1967 = $this->w;
			$TSNNi1966 = $TSNNt1967;
			$TSa1006[$TLE1340] = $TSNNi1966;
			$TLE1007 = $matrix->offsetGet($TSa1006);
			return $TLE1007;
		}
		public function next()
		{
			$TSNNt1968 = $this->graph;
			$matrix = $TSNNt1968->getMatrix();
			$TSNNt1969 = $this->v;
			++$TSNNt1969;
			$this->v = $TSNNt1969;
			$ElcfPF46 = True;
			while (True) {
				if ($ElcfPF46) {
					$ElcfPF46 = False;
				} else {
					$TSNNt1970 = $this->v;
					++$TSNNt1970;
					$this->v = $TSNNt1970;
				}
				$TSNNt1971 = $this->graph;
				$TLE1008 = $TSNNt1971->getNumberOfVertices();
				$TSNNt1972 = $this->v;
				$TLE1009 = ($TSNNt1972 < $TLE1008);
				if ($TLE1009) {
				} else {
					break;
				}
				unset($TSa1010);
				$TSa1010 = (array) $TSa1010;
				$TLE1341 = 0;
				$TSNNt1974 = $this->v;
				$TSNNi1973 = $TSNNt1974;
				$TSa1010[$TLE1341] = $TSNNi1973;
				$TLE1342 = 1;
				$TSNNt1976 = $this->w;
				$TSNNi1975 = $TSNNt1976;
				$TSa1010[$TLE1342] = $TSNNi1975;
				$TLE1011 = $matrix->offsetGet($TSa1010);
				$TLE1012 = NULL;
				$TLE1013 = ($TLE1011 !== $TLE1012);
				if ($TLE1013) {
					break;
				}
			}
		}
		public function rewind()
		{
			$TSNNt1977 = $this->graph;
			$matrix = $TSNNt1977->getMatrix();
			$TLE1014 = 0;
			$TSNNt1978 = $TLE1014;
			$this->v = $TSNNt1978;
			$ElcfPF47 = True;
			while (True) {
				if ($ElcfPF47) {
					$ElcfPF47 = False;
				} else {
					$TSNNt1979 = $this->v;
					++$TSNNt1979;
					$this->v = $TSNNt1979;
				}
				$TSNNt1980 = $this->graph;
				$TLE1015 = $TSNNt1980->getNumberOfVertices();
				$TSNNt1981 = $this->v;
				$TLE1016 = ($TSNNt1981 < $TLE1015);
				if ($TLE1016) {
				} else {
					break;
				}
				unset($TSa1017);
				$TSa1017 = (array) $TSa1017;
				$TLE1343 = 0;
				$TSNNt1983 = $this->v;
				$TSNNi1982 = $TSNNt1983;
				$TSa1017[$TLE1343] = $TSNNi1982;
				$TLE1344 = 1;
				$TSNNt1985 = $this->w;
				$TSNNi1984 = $TSNNt1985;
				$TSa1017[$TLE1344] = $TSNNi1984;
				$TLE1018 = $matrix->offsetGet($TSa1017);
				$TLE1019 = NULL;
				$TLE1020 = ($TLE1018 !== $TLE1019);
				if ($TLE1020) {
					break;
				}
			}
		}
	}
	class GraphAsMatrix_IncidentEdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		protected $w = 0;
		public function __construct(GraphAsMatrix $graph, $w)
		{
			$TSNNt1986 = $graph;
			$this->graph = $TSNNt1986;
			$TSNNt1987 = $w;
			$this->w = $TSNNt1987;
		}
		public function getIterator()
		{
			$TSNNt1988 = $this->graph;
			$TSNNt1989 = $this->w;
			$TLE1021 = new GraphAsMatrix_IncidentEdgeIterator($TSNNt1988, $TSNNt1989);
			return $TLE1021;
		}
	}
	class GraphAsMatrix extends AbstractGraph
	{
		protected $matrix = NULL;
		public function __construct($size = 0)
		{
			AbstractGraph::__construct($size);
			$TLE1022 = new DenseMatrix($size, $size);
			$TSNNt1990 = $TLE1022;
			$this->matrix = $TSNNt1990;
		}
		public function &getMatrix()
		{
			$TSNNt1991 = $this->matrix;
			return $TSNNt1991;
		}
		public function purge()
		{
			$i = 0;
			$ElcfPF49 = True;
			while (True) {
				if ($ElcfPF49) {
					$ElcfPF49 = False;
				} else {
					++$i;
				}
				$TSNNt1992 = $this->numberOfVertices;
				$TLE1023 = ($i < $TSNNt1992);
				if ($TLE1023) {
				} else {
					break;
				}
				$j = 0;
				$ElcfPF48 = True;
				while (True) {
					if ($ElcfPF48) {
						$ElcfPF48 = False;
					} else {
						++$j;
					}
					$TSNNt1993 = $this->numberOfVertices;
					$TLE1024 = ($j < $TSNNt1993);
					if ($TLE1024) {
					} else {
						break;
					}
					unset($TSa1025);
					$TSa1025 = (array) $TSa1025;
					$TLE1345 = 0;
					$TSNNi1994 = $i;
					$TSa1025[$TLE1345] = $TSNNi1994;
					$TLE1346 = 1;
					$TSNNi1995 = $j;
					$TSa1025[$TLE1346] = $TSNNi1995;
					$TLE1026 = NULL;
					$TSNNt1996 = $this->matrix;
					$TSNNt1996->offsetSet($TSa1025, $TLE1026);
				}
			}
			AbstractGraph::purge();
		}
		protected function insertEdge(Edge $edge)
		{
			$TLE1027 = $edge->getV0();
			$v = $TLE1027->getNumber();
			$TLE1028 = $edge->getV1();
			$w = $TLE1028->getNumber();
			unset($TSa1029);
			$TSa1029 = (array) $TSa1029;
			$TLE1347 = 0;
			$TSNNi1997 = $v;
			$TSa1029[$TLE1347] = $TSNNi1997;
			$TLE1348 = 1;
			$TSNNi1998 = $w;
			$TSa1029[$TLE1348] = $TSNNi1998;
			$TSNNt1999 = $this->matrix;
			$TLE1030 = $TSNNt1999->offsetGet($TSa1029);
			$TLE1031 = NULL;
			$TLE1032 = ($TLE1030 !== $TLE1031);
			if ($TLE1032) {
				$TLE1033 = new ArgumentError();
				throw $TLE1033;
			}
			unset($TSa1034);
			$TSa1034 = (array) $TSa1034;
			$TLE1349 = 0;
			$TSNNi2000 = $v;
			$TSa1034[$TLE1349] = $TSNNi2000;
			$TLE1350 = 1;
			$TSNNi2001 = $w;
			$TSa1034[$TLE1350] = $TSNNi2001;
			$TSNNt2002 = $this->matrix;
			$TSNNt2002->offsetSet($TSa1034, $edge);
			unset($TSa1035);
			$TSa1035 = (array) $TSa1035;
			$TLE1351 = 0;
			$TSNNi2003 = $w;
			$TSa1035[$TLE1351] = $TSNNi2003;
			$TLE1352 = 1;
			$TSNNi2004 = $v;
			$TSa1035[$TLE1352] = $TSNNi2004;
			$TSNNt2005 = $this->matrix;
			$TSNNt2005->offsetSet($TSa1035, $edge);
			$TSNNt2006 = $this->numberOfEdges;
			++$TSNNt2006;
			$this->numberOfEdges = $TSNNt2006;
		}
		public function getEdge($v, $w)
		{
			$TLE1036 = 0;
			$TLE120 = ($v < $TLE1036);
			if ($TLE120) {
				$TEF121 = $TLE120;
			} else {
				$TSNNt2007 = $this->numberOfVertices;
				$TEF121 = ($TSNNt2007 <= $v);
			}
			$TLE1037 = (bool) $TEF121;
			if ($TLE1037) {
				$TLE1038 = new IndexError();
				throw $TLE1038;
			}
			$TLE1039 = 0;
			$TLE122 = ($w < $TLE1039);
			if ($TLE122) {
				$TEF123 = $TLE122;
			} else {
				$TSNNt2008 = $this->numberOfVertices;
				$TEF123 = ($TSNNt2008 <= $w);
			}
			$TLE1040 = (bool) $TEF123;
			if ($TLE1040) {
				$TLE1041 = new IndexError();
				throw $TLE1041;
			}
			unset($TSa1042);
			$TSa1042 = (array) $TSa1042;
			$TLE1353 = 0;
			$TSNNi2009 = $v;
			$TSa1042[$TLE1353] = $TSNNi2009;
			$TLE1354 = 1;
			$TSNNi2010 = $w;
			$TSa1042[$TLE1354] = $TSNNi2010;
			$TSNNt2011 = $this->matrix;
			$TLE1043 = $TSNNt2011->offsetGet($TSa1042);
			$TLE1044 = NULL;
			$TLE1045 = ($TLE1043 === $TLE1044);
			if ($TLE1045) {
				$TLE1046 = new ArgumentError();
				throw $TLE1046;
			}
			unset($TSa1047);
			$TSa1047 = (array) $TSa1047;
			$TLE1355 = 0;
			$TSNNi2012 = $v;
			$TSa1047[$TLE1355] = $TSNNi2012;
			$TLE1356 = 1;
			$TSNNi2013 = $w;
			$TSa1047[$TLE1356] = $TSNNi2013;
			$TSNNt2014 = $this->matrix;
			$TLE1048 = $TSNNt2014->offsetGet($TSa1047);
			return $TLE1048;
		}
		public function isEdge($v, $w)
		{
			$TLE1049 = 0;
			$TLE124 = ($v < $TLE1049);
			if ($TLE124) {
				$TEF125 = $TLE124;
			} else {
				$TSNNt2015 = $this->numberOfVertices;
				$TEF125 = ($TSNNt2015 <= $v);
			}
			$TLE1050 = (bool) $TEF125;
			if ($TLE1050) {
				$TLE1051 = new IndexError();
				throw $TLE1051;
			}
			$TLE1052 = 0;
			$TLE126 = ($w < $TLE1052);
			if ($TLE126) {
				$TEF127 = $TLE126;
			} else {
				$TSNNt2016 = $this->numberOfVertices;
				$TEF127 = ($TSNNt2016 <= $w);
			}
			$TLE1053 = (bool) $TEF127;
			if ($TLE1053) {
				$TLE1054 = new IndexError();
				throw $TLE1054;
			}
			unset($TSa1055);
			$TSa1055 = (array) $TSa1055;
			$TLE1357 = 0;
			$TSNNi2017 = $v;
			$TSa1055[$TLE1357] = $TSNNi2017;
			$TLE1358 = 1;
			$TSNNi2018 = $w;
			$TSa1055[$TLE1358] = $TSNNi2018;
			$TSNNt2019 = $this->matrix;
			$TLE1056 = $TSNNt2019->offsetGet($TSa1055);
			$TLE1057 = NULL;
			$TLE1058 = ($TLE1056 !== $TLE1057);
			return $TLE1058;
		}
		public function getEdges()
		{
			$TLE1059 = new GraphAsMatrix_EdgeAggregate($this);
			return $TLE1059;
		}
		public function getEmanatingEdges($v)
		{
			$TLE1060 = new GraphAsMatrix_EmanatingEdgeAggregate($this, $v);
			return $TLE1060;
		}
		public function getIncidentEdges($w)
		{
			$TLE1061 = new GraphAsMatrix_IncidentEdgeAggregate($this, $w);
			return $TLE1061;
		}
		protected function compareTo(IComparable $arg)
		{
			$TLE1062 = new MethodNotImplementedException();
			throw $TLE1062;
		}
	}
	class DigraphAsMatrix_EdgeIterator extends AbstractIterator
	{
		protected $graph = NULL;
		protected $v = 0;
		protected $w = 0;
		public function __construct(DigraphAsMatrix $graph)
		{
			AbstractIterator::__construct();
			$TSNNt2020 = $graph;
			$this->graph = $TSNNt2020;
			$TSNNt2021 = $this->graph;
			$matrix = $TSNNt2021->getMatrix();
			$breakOuter = False;
			$TLE1063 = 0;
			$TSNNt2022 = $TLE1063;
			$this->v = $TSNNt2022;
			$ElcfPF51 = True;
			while (True) {
				if ($ElcfPF51) {
					$ElcfPF51 = False;
				} else {
					$TSNNt2023 = $this->v;
					++$TSNNt2023;
					$this->v = $TSNNt2023;
				}
				$TSNNt2024 = $this->graph;
				$TLE1064 = $TSNNt2024->getNumberOfVertices();
				$TSNNt2025 = $this->v;
				$TLE1065 = ($TSNNt2025 < $TLE1064);
				if ($TLE1065) {
				} else {
					break;
				}
				$TLE1066 = 0;
				$TSNNt2026 = $TLE1066;
				$this->w = $TSNNt2026;
				$ElcfPF50 = True;
				while (True) {
					if ($ElcfPF50) {
						$ElcfPF50 = False;
					} else {
						$TSNNt2027 = $this->w;
						++$TSNNt2027;
						$this->w = $TSNNt2027;
					}
					$TSNNt2028 = $this->graph;
					$TLE1067 = $TSNNt2028->getNumberOfVertices();
					$TSNNt2029 = $this->w;
					$TLE1068 = ($TSNNt2029 < $TLE1067);
					if ($TLE1068) {
					} else {
						break;
					}
					unset($TSa1069);
					$TSa1069 = (array) $TSa1069;
					$TLE1359 = 0;
					$TSNNt2031 = $this->v;
					$TSNNi2030 = $TSNNt2031;
					$TSa1069[$TLE1359] = $TSNNi2030;
					$TLE1360 = 1;
					$TSNNt2033 = $this->w;
					$TSNNi2032 = $TSNNt2033;
					$TSa1069[$TLE1360] = $TSNNi2032;
					$TLE1070 = $matrix->offsetGet($TSa1069);
					$TLE1071 = NULL;
					$TLE1072 = ($TLE1070 !== $TLE1071);
					if ($TLE1072) {
						$breakOuter = True;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = False;
					break;
				}
			}
		}
		public function valid()
		{
			$TSNNt2034 = $this->graph;
			$TLE1073 = $TSNNt2034->getNumberOfVertices();
			$TSNNt2035 = $this->v;
			$TLE128 = ($TSNNt2035 < $TLE1073);
			if ($TLE128) {
				$TSNNt2036 = $this->graph;
				$TLE1074 = $TSNNt2036->getNumberOfVertices();
				$TSNNt2037 = $this->w;
				$TEF129 = ($TSNNt2037 < $TLE1074);
			} else {
				$TEF129 = $TLE128;
			}
			$TLE1075 = (bool) $TEF129;
			return $TLE1075;
		}
		public function key()
		{
			$TSNNt2038 = $this->graph;
			$TLE1076 = $TSNNt2038->getNumberOfVertices();
			$TSNNt2039 = $this->v;
			$TLE1077 = ($TSNNt2039 * $TLE1076);
			$TSNNt2040 = $this->w;
			$TLE1078 = ($TLE1077 + $TSNNt2040);
			return $TLE1078;
		}
		public function current()
		{
			$TSNNt2041 = $this->graph;
			$matrix = $TSNNt2041->getMatrix();
			unset($TSa1079);
			$TSa1079 = (array) $TSa1079;
			$TLE1361 = 0;
			$TSNNt2043 = $this->v;
			$TSNNi2042 = $TSNNt2043;
			$TSa1079[$TLE1361] = $TSNNi2042;
			$TLE1362 = 1;
			$TSNNt2045 = $this->w;
			$TSNNi2044 = $TSNNt2045;
			$TSa1079[$TLE1362] = $TSNNi2044;
			$TLE1080 = $matrix->offsetGet($TSa1079);
			return $TLE1080;
		}
		public function next()
		{
			$TSNNt2046 = $this->graph;
			$matrix = $TSNNt2046->getMatrix();
			$TSNNt2047 = $this->w;
			++$TSNNt2047;
			$this->w = $TSNNt2047;
			$ElcfPF52 = True;
			while (True) {
				if ($ElcfPF52) {
					$ElcfPF52 = False;
				} else {
					$TSNNt2048 = $this->w;
					++$TSNNt2048;
					$this->w = $TSNNt2048;
				}
				$TSNNt2049 = $this->graph;
				$TLE1081 = $TSNNt2049->getNumberOfVertices();
				$TSNNt2050 = $this->w;
				$TLE1082 = ($TSNNt2050 < $TLE1081);
				if ($TLE1082) {
				} else {
					break;
				}
				unset($TSa1083);
				$TSa1083 = (array) $TSa1083;
				$TLE1363 = 0;
				$TSNNt2052 = $this->v;
				$TSNNi2051 = $TSNNt2052;
				$TSa1083[$TLE1363] = $TSNNi2051;
				$TLE1364 = 1;
				$TSNNt2054 = $this->w;
				$TSNNi2053 = $TSNNt2054;
				$TSa1083[$TLE1364] = $TSNNi2053;
				$TLE1084 = $matrix->offsetGet($TSa1083);
				$TLE1085 = NULL;
				$TLE1086 = ($TLE1084 !== $TLE1085);
				if ($TLE1086) {
					$TLE1087 = NULL;
					return $TLE1087;
				}
			}
			$TSNNt2055 = $this->v;
			++$TSNNt2055;
			$this->v = $TSNNt2055;
			$ElcfPF54 = True;
			while (True) {
				if ($ElcfPF54) {
					$ElcfPF54 = False;
				} else {
					$TSNNt2056 = $this->v;
					++$TSNNt2056;
					$this->v = $TSNNt2056;
				}
				$TSNNt2057 = $this->graph;
				$TLE1088 = $TSNNt2057->getNumberOfVertices();
				$TSNNt2058 = $this->v;
				$TLE1089 = ($TSNNt2058 < $TLE1088);
				if ($TLE1089) {
				} else {
					break;
				}
				$TLE1090 = 0;
				$TSNNt2059 = $TLE1090;
				$this->w = $TSNNt2059;
				$ElcfPF53 = True;
				while (True) {
					if ($ElcfPF53) {
						$ElcfPF53 = False;
					} else {
						$TSNNt2060 = $this->w;
						++$TSNNt2060;
						$this->w = $TSNNt2060;
					}
					$TSNNt2061 = $this->graph;
					$TLE1091 = $TSNNt2061->getNumberOfVertices();
					$TSNNt2062 = $this->w;
					$TLE1092 = ($TSNNt2062 < $TLE1091);
					if ($TLE1092) {
					} else {
						break;
					}
					unset($TSa1093);
					$TSa1093 = (array) $TSa1093;
					$TLE1365 = 0;
					$TSNNt2064 = $this->v;
					$TSNNi2063 = $TSNNt2064;
					$TSa1093[$TLE1365] = $TSNNi2063;
					$TLE1366 = 1;
					$TSNNt2066 = $this->w;
					$TSNNi2065 = $TSNNt2066;
					$TSa1093[$TLE1366] = $TSNNi2065;
					$TLE1094 = $matrix->offsetGet($TSa1093);
					$TLE1095 = NULL;
					$TLE1096 = ($TLE1094 !== $TLE1095);
					if ($TLE1096) {
						$TLE1097 = NULL;
						return $TLE1097;
					}
				}
			}
		}
		public function rewind()
		{
			$TSNNt2067 = $this->graph;
			$matrix = $TSNNt2067->getMatrix();
			$breakOuter = False;
			$TLE1098 = 0;
			$TSNNt2068 = $TLE1098;
			$this->v = $TSNNt2068;
			$ElcfPF56 = True;
			while (True) {
				if ($ElcfPF56) {
					$ElcfPF56 = False;
				} else {
					$TSNNt2069 = $this->v;
					++$TSNNt2069;
					$this->v = $TSNNt2069;
				}
				$TSNNt2070 = $this->graph;
				$TLE1099 = $TSNNt2070->getNumberOfVertices();
				$TSNNt2071 = $this->v;
				$TLE1100 = ($TSNNt2071 < $TLE1099);
				if ($TLE1100) {
				} else {
					break;
				}
				$TLE1101 = 0;
				$TSNNt2072 = $TLE1101;
				$this->w = $TSNNt2072;
				$ElcfPF55 = True;
				while (True) {
					if ($ElcfPF55) {
						$ElcfPF55 = False;
					} else {
						$TSNNt2073 = $this->w;
						++$TSNNt2073;
						$this->w = $TSNNt2073;
					}
					$TSNNt2074 = $this->graph;
					$TLE1102 = $TSNNt2074->getNumberOfVertices();
					$TSNNt2075 = $this->w;
					$TLE1103 = ($TSNNt2075 < $TLE1102);
					if ($TLE1103) {
					} else {
						break;
					}
					unset($TSa1104);
					$TSa1104 = (array) $TSa1104;
					$TLE1367 = 0;
					$TSNNt2077 = $this->v;
					$TSNNi2076 = $TSNNt2077;
					$TSa1104[$TLE1367] = $TSNNi2076;
					$TLE1368 = 1;
					$TSNNt2079 = $this->w;
					$TSNNi2078 = $TSNNt2079;
					$TSa1104[$TLE1368] = $TSNNi2078;
					$TLE1105 = $matrix->offsetGet($TSa1104);
					$TLE1106 = NULL;
					$TLE1107 = ($TLE1105 !== $TLE1106);
					if ($TLE1107) {
						$breakOuter = True;
						break;
					}
				}
				if ($breakOuter) {
					$breakOuter = False;
					break;
				}
			}
		}
	}
	class DigraphAsMatrix_EdgeAggregate implements IteratorAggregate
	{
		protected $graph = NULL;
		public function __construct(DigraphAsMatrix $graph)
		{
			$TSNNt2080 = $graph;
			$this->graph = $TSNNt2080;
		}
		public function getIterator()
		{
			$TSNNt2081 = $this->graph;
			$TLE1108 = new DigraphAsMatrix_EdgeIterator($TSNNt2081);
			return $TLE1108;
		}
	}
	class DigraphAsMatrix extends GraphAsMatrix implements IDigraph
	{
		public function __construct($size = 0)
		{
			GraphAsMatrix::__construct($size);
		}
		protected function insertEdge(Edge $edge)
		{
			$TLE1109 = $edge->getV0();
			$v = $TLE1109->getNumber();
			$TLE1110 = $edge->getV1();
			$w = $TLE1110->getNumber();
			unset($TSa1111);
			$TSa1111 = (array) $TSa1111;
			$TLE1369 = 0;
			$TSNNi2082 = $v;
			$TSa1111[$TLE1369] = $TSNNi2082;
			$TLE1370 = 1;
			$TSNNi2083 = $w;
			$TSa1111[$TLE1370] = $TSNNi2083;
			$TSNNt2084 = $this->matrix;
			$TLE1112 = $TSNNt2084->offsetGet($TSa1111);
			$TLE1113 = NULL;
			$TLE1114 = ($TLE1112 !== $TLE1113);
			if ($TLE1114) {
				$TLE1115 = new ArgumentError();
				throw $TLE1115;
			}
			unset($TSa1116);
			$TSa1116 = (array) $TSa1116;
			$TLE1371 = 0;
			$TSNNi2085 = $v;
			$TSa1116[$TLE1371] = $TSNNi2085;
			$TLE1372 = 1;
			$TSNNi2086 = $w;
			$TSa1116[$TLE1372] = $TSNNi2086;
			$TSNNt2087 = $this->matrix;
			$TSNNt2087->offsetSet($TSa1116, $edge);
			$TSNNt2088 = $this->numberOfEdges;
			++$TSNNt2088;
			$this->numberOfEdges = $TSNNt2088;
		}
		public function getEdges()
		{
			$TLE1117 = new DigraphAsMatrix_EdgeAggregate($this);
			return $TLE1117;
		}
	}
	function floydsAlgorithm(IDigraph $g)
	{
		$n = $g->getNumberOfVertices();
		$distance = new DenseMatrix($n, $n);
		$v = 0;
		$ElcfPF58 = True;
		while (True) {
			if ($ElcfPF58) {
				$ElcfPF58 = False;
			} else {
				++$v;
			}
			$TLE1118 = ($v < $n);
			if ($TLE1118) {
			} else {
				break;
			}
			$w = 0;
			$ElcfPF57 = True;
			while (True) {
				if ($ElcfPF57) {
					$ElcfPF57 = False;
				} else {
					++$w;
				}
				$TLE1119 = ($w < $n);
				if ($TLE1119) {
				} else {
					break;
				}
				unset($TSa1120);
				$TSa1120 = (array) $TSa1120;
				$TLE1373 = 0;
				$TSNNi2089 = $v;
				$TSa1120[$TLE1373] = $TSNNi2089;
				$TLE1374 = 1;
				$TSNNi2090 = $w;
				$TSa1120[$TLE1374] = $TSNNi2090;
				$TLE1121 = Limits::MAXINT;
				$distance->offsetSet($TSa1120, $TLE1121);
			}
		}
		$TLE1122 = $g->getEdges();
		$it = $TLE1122->getIterator();
		$it->rewind();
		while (True) {
			$TLE1123 = $it->valid();
			$TLE1124 = !$TLE1123;
			if ($TLE1124) {
				break;
			}
			$edge = $it->current();
			$wt = $edge->getWeight();
			$TLE1125 = $edge->getV0();
			$TLE1126 = $TLE1125->getNumber();
			$TLE1127 = $edge->getV1();
			$TLE1128 = $TLE1127->getNumber();
			unset($TSa1129);
			$TSa1129 = (array) $TSa1129;
			$TLE1375 = 0;
			$TSNNi2091 = $TLE1126;
			$TSa1129[$TLE1375] = $TSNNi2091;
			$TLE1376 = 1;
			$TSNNi2092 = $TLE1128;
			$TSa1129[$TLE1376] = $TSNNi2092;
			$TLE1130 = unbox($wt);
			$distance->offsetSet($TSa1129, $TLE1130);
			$it->next();
		}
		$i = 0;
		$ElcfPF61 = True;
		while (True) {
			if ($ElcfPF61) {
				$ElcfPF61 = False;
			} else {
				++$i;
			}
			$TLE1131 = ($i < $n);
			if ($TLE1131) {
			} else {
				break;
			}
			$v = 0;
			$ElcfPF60 = True;
			while (True) {
				if ($ElcfPF60) {
					$ElcfPF60 = False;
				} else {
					++$v;
				}
				$TLE1132 = ($v < $n);
				if ($TLE1132) {
				} else {
					break;
				}
				$w = 0;
				$ElcfPF59 = True;
				while (True) {
					if ($ElcfPF59) {
						$ElcfPF59 = False;
					} else {
						++$w;
					}
					$TLE1133 = ($w < $n);
					if ($TLE1133) {
					} else {
						break;
					}
					unset($TSa1134);
					$TSa1134 = (array) $TSa1134;
					$TLE1377 = 0;
					$TSNNi2093 = $v;
					$TSa1134[$TLE1377] = $TSNNi2093;
					$TLE1378 = 1;
					$TSNNi2094 = $i;
					$TSa1134[$TLE1378] = $TSNNi2094;
					$TLE1135 = $distance->offsetGet($TSa1134);
					$TLE1136 = Limits::MAXINT;
					$TLE130 = ($TLE1135 != $TLE1136);
					if ($TLE130) {
						unset($TSa1137);
						$TSa1137 = (array) $TSa1137;
						$TLE1379 = 0;
						$TSNNi2095 = $i;
						$TSa1137[$TLE1379] = $TSNNi2095;
						$TLE1380 = 1;
						$TSNNi2096 = $w;
						$TSa1137[$TLE1380] = $TSNNi2096;
						$TLE1138 = $distance->offsetGet($TSa1137);
						$TLE1139 = Limits::MAXINT;
						$TEF131 = ($TLE1138 != $TLE1139);
					} else {
						$TEF131 = $TLE130;
					}
					$TLE1140 = (bool) $TEF131;
					if ($TLE1140) {
						unset($TSa1141);
						$TSa1141 = (array) $TSa1141;
						$TLE1381 = 0;
						$TSNNi2097 = $v;
						$TSa1141[$TLE1381] = $TSNNi2097;
						$TLE1382 = 1;
						$TSNNi2098 = $i;
						$TSa1141[$TLE1382] = $TSNNi2098;
						$TLE1142 = $distance->offsetGet($TSa1141);
						unset($TSa1143);
						$TSa1143 = (array) $TSa1143;
						$TLE1383 = 0;
						$TSNNi2099 = $i;
						$TSa1143[$TLE1383] = $TSNNi2099;
						$TLE1384 = 1;
						$TSNNi2100 = $w;
						$TSa1143[$TLE1384] = $TSNNi2100;
						$TLE1144 = $distance->offsetGet($TSa1143);
						$d = ($TLE1142 + $TLE1144);
						unset($TSa1145);
						$TSa1145 = (array) $TSa1145;
						$TLE1385 = 0;
						$TSNNi2101 = $v;
						$TSa1145[$TLE1385] = $TSNNi2101;
						$TLE1386 = 1;
						$TSNNi2102 = $w;
						$TSa1145[$TLE1386] = $TSNNi2102;
						$TLE1146 = $distance->offsetGet($TSa1145);
						$TLE1147 = ($d < $TLE1146);
						if ($TLE1147) {
							unset($TSa1148);
							$TSa1148 = (array) $TSa1148;
							$TLE1387 = 0;
							$TSNNi2103 = $v;
							$TSa1148[$TLE1387] = $TSNNi2103;
							$TLE1388 = 1;
							$TSNNi2104 = $w;
							$TSa1148[$TLE1388] = $TSNNi2104;
							$distance->offsetSet($TSa1148, $d);
						}
					}
				}
			}
		}
		$result = new DigraphAsMatrix($n);
		$v = 0;
		$ElcfPF62 = True;
		while (True) {
			if ($ElcfPF62) {
				$ElcfPF62 = False;
			} else {
				++$v;
			}
			$TLE1149 = ($v < $n);
			if ($TLE1149) {
			} else {
				break;
			}
			$result->addVertex($v);
		}
		$v = 0;
		$ElcfPF64 = True;
		while (True) {
			if ($ElcfPF64) {
				$ElcfPF64 = False;
			} else {
				++$v;
			}
			$TLE1150 = ($v < $n);
			if ($TLE1150) {
			} else {
				break;
			}
			$w = 0;
			$ElcfPF63 = True;
			while (True) {
				if ($ElcfPF63) {
					$ElcfPF63 = False;
				} else {
					++$w;
				}
				$TLE1151 = ($w < $n);
				if ($TLE1151) {
				} else {
					break;
				}
				unset($TSa1152);
				$TSa1152 = (array) $TSa1152;
				$TLE1389 = 0;
				$TSNNi2105 = $v;
				$TSa1152[$TLE1389] = $TSNNi2105;
				$TLE1390 = 1;
				$TSNNi2106 = $w;
				$TSa1152[$TLE1390] = $TSNNi2106;
				$TLE1153 = $distance->offsetGet($TSa1152);
				$TLE1154 = limits::MAXINT;
				$TLE1155 = ($TLE1153 != $TLE1154);
				if ($TLE1155) {
					unset($TSa1156);
					$TSa1156 = (array) $TSa1156;
					$TLE1391 = 0;
					$TSNNi2107 = $v;
					$TSa1156[$TLE1391] = $TSNNi2107;
					$TLE1392 = 1;
					$TSNNi2108 = $w;
					$TSa1156[$TLE1392] = $TSNNi2108;
					$TLE1157 = $distance->offsetGet($TSa1156);
					$TLE1158 = new BoxedInteger($TLE1157);
					$result->addEdge($v, $w, $TLE1158);
				}
			}
		}
		return $result;
	}
	$TLE1159 = 32;
	$g = new DigraphAsLists($TLE1159);
	$TLE1160 = 0;
	$TLE1161 = 1;
	$TLE1162 = new BoxedInteger($TLE1161);
	$g->addVertex($TLE1160, $TLE1162);
	$TLE1163 = 1;
	$TLE1164 = 1;
	$TLE1165 = new BoxedInteger($TLE1164);
	$g->addVertex($TLE1163, $TLE1165);
	$TLE1166 = 2;
	$TLE1167 = 1;
	$TLE1168 = new BoxedInteger($TLE1167);
	$g->addVertex($TLE1166, $TLE1168);
	$TLE1169 = 3;
	$TLE1170 = 1;
	$TLE1171 = new BoxedInteger($TLE1170);
	$g->addVertex($TLE1169, $TLE1171);
	$TLE1172 = 4;
	$TLE1173 = 1;
	$TLE1174 = new BoxedInteger($TLE1173);
	$g->addVertex($TLE1172, $TLE1174);
	$TLE1175 = 5;
	$TLE1176 = 1;
	$TLE1177 = new BoxedInteger($TLE1176);
	$g->addVertex($TLE1175, $TLE1177);
	$TLE1178 = 0;
	$TLE1179 = 0;
	$TLE1180 = 0;
	$TLE1181 = new BoxedInteger($TLE1180);
	$g->addEdge($TLE1178, $TLE1179, $TLE1181);
	$TLE1182 = 1;
	$TLE1183 = 1;
	$TLE1184 = 0;
	$TLE1185 = new BoxedInteger($TLE1184);
	$g->addEdge($TLE1182, $TLE1183, $TLE1185);
	$TLE1186 = 2;
	$TLE1187 = 2;
	$TLE1188 = 0;
	$TLE1189 = new BoxedInteger($TLE1188);
	$g->addEdge($TLE1186, $TLE1187, $TLE1189);
	$TLE1190 = 3;
	$TLE1191 = 3;
	$TLE1192 = 0;
	$TLE1193 = new BoxedInteger($TLE1192);
	$g->addEdge($TLE1190, $TLE1191, $TLE1193);
	$TLE1194 = 4;
	$TLE1195 = 4;
	$TLE1196 = 0;
	$TLE1197 = new BoxedInteger($TLE1196);
	$g->addEdge($TLE1194, $TLE1195, $TLE1197);
	$TLE1198 = 5;
	$TLE1199 = 5;
	$TLE1200 = 0;
	$TLE1201 = new BoxedInteger($TLE1200);
	$g->addEdge($TLE1198, $TLE1199, $TLE1201);
	$TLE1202 = 0;
	$TLE1203 = 1;
	$TLE1204 = 2;
	$TLE1205 = new BoxedInteger($TLE1204);
	$g->addEdge($TLE1202, $TLE1203, $TLE1205);
	$TLE1206 = 0;
	$TLE1207 = 2;
	$TLE1208 = 5;
	$TLE1209 = new BoxedInteger($TLE1208);
	$g->addEdge($TLE1206, $TLE1207, $TLE1209);
	$TLE1210 = 1;
	$TLE1211 = 2;
	$TLE1212 = 7;
	$TLE1213 = new BoxedInteger($TLE1212);
	$g->addEdge($TLE1210, $TLE1211, $TLE1213);
	$TLE1214 = 1;
	$TLE1215 = 3;
	$TLE1216 = 1;
	$TLE1217 = new BoxedInteger($TLE1216);
	$g->addEdge($TLE1214, $TLE1215, $TLE1217);
	$TLE1218 = 1;
	$TLE1219 = 5;
	$TLE1220 = 8;
	$TLE1221 = new BoxedInteger($TLE1220);
	$g->addEdge($TLE1218, $TLE1219, $TLE1221);
	$TLE1222 = 2;
	$TLE1223 = 3;
	$TLE1224 = 4;
	$TLE1225 = new BoxedInteger($TLE1224);
	$g->addEdge($TLE1222, $TLE1223, $TLE1225);
	$TLE1226 = 3;
	$TLE1227 = 4;
	$TLE1228 = 3;
	$TLE1229 = new BoxedInteger($TLE1228);
	$g->addEdge($TLE1226, $TLE1227, $TLE1229);
	$TLE1230 = 4;
	$TLE1231 = 2;
	$TLE1232 = 2;
	$TLE1233 = new BoxedInteger($TLE1232);
	$g->addEdge($TLE1230, $TLE1231, $TLE1233);
	$TLE1234 = 4;
	$TLE1235 = 5;
	$TLE1236 = 3;
	$TLE1237 = new BoxedInteger($TLE1236);
	$g->addEdge($TLE1234, $TLE1235, $TLE1237);
	$TLE1238 = 5;
	$TLE1239 = 1;
	$TLE1240 = 5;
	$TLE1241 = new BoxedInteger($TLE1240);
	$g->addEdge($TLE1238, $TLE1239, $TLE1241);
	$TLE1242 = 5;
	$TLE1243 = 3;
	$TLE1244 = 2;
	$TLE1245 = new BoxedInteger($TLE1244);
	$g->addEdge($TLE1242, $TLE1243, $TLE1245);
	$TLE1246 = 5;
	$TLE1247 = 4;
	$TLE1248 = 4;
	$TLE1249 = new BoxedInteger($TLE1248);
	$g->addEdge($TLE1246, $TLE1247, $TLE1249);
	$res = floydsAlgorithm($g);
	$TLE1250 = $res->__toString();
	print($TLE1250);
?>
/home/henkerik/Development/haskell/objectsensitivetyping
