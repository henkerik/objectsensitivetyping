<?php

class UndefinedVariableException extends Exception {}

class Env {
	protected $env;

	public function __construct()
	{
		$this->env = array();
	}

	public function lookup ($var)
	{
		if (isset($this->env[$var])) {
			return $this->env[$var];
		} else {
			throw new UndefinedVariableException ();
		}
	}

	public function store($var, $value)
	{
		$this->env[$var] = $value;
	}
}

abstract class Expr { 

	protected $hashCode;

	public function __construct ($hashCode)
	{
		$this->hashCode = $hashCode & 0x1FFF;
	}

	public function hashCode()
	{
		return $this->hashCode;
	}

	abstract function evaluate (Env $env);

	abstract function visit(Visitor $visitor);
}

abstract class BinaryExpr extends Expr {
	protected $left;
	protected $right;

	public function __construct ($hashCode, Expr $left, Expr $right) 
	{
		parent::__construct($hashCode);

		$this->left = $left;
		$this->right = $right;
	}

	public function getLeft()
	{
		return $this->left;
	}

	public function getRight()
	{
		return $this->right;
	}
}

class Factor extends Expr {
	protected $expr;

	public function __construct(Expr $expr)
	{
		$this->expr = $expr;
	}

	public function getExpr()
	{
		return $this->expr;
	}

	public function evaluate (Env $env) 
	{
		$n      = $this->expr->evaluate($env);
		$factor = 1; 
		while ($n > 1) {
			$factor = $factor * $n;
			$n--;
		}
		return $factor;
	}

	public function equals ($other)
	{
		if ($other instanceof Factor) {
			return $this->expr->equals($other->getExpr());
		} else {
			return false;
		}
	}

	public function hashCode ()
	{
		return 17 * $this->expr->hashCode();
	}

	public function visit (Visitor $visitor)
	{
		$visitor->onFactor($this);
	}
}

class Min extends BinaryExpr {
	public function __construct(Expr $left, Expr $right)
	{
		parent::__construct($left->hashCode() * $right->hashCode(), $left, $right);
	}

	public function evaluate (Env $env)
	{
		return $this->left->evaluate($env) - $this->right->evaluate($env);
	}

	public function equals ($other)
	{
		if ($other instanceof Min) {
			return $this->left->equals($other->getLeft()) 
			    && $this->right->equals($other->getRight());
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onMin($this);
	}
} 

class Plus extends BinaryExpr {
	public function __construct(Expr $left, Expr $right)
	{
		parent::__construct(3 * $left->hashCode() * $right->hashCode(), $left, $right);
	}

	public function evaluate (Env $env)
	{
		return $this->left->evaluate($env) + $this->right->evaluate($env);
	}

	public function equals ($other)
	{
		if ($other instanceof Plus) {
			return $this->left->equals($other->getLeft()) 
			    && $this->right->equals($other->getRight());
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onPlus($this);
	}
} 

class Mul extends BinaryExpr {
	public function __construct(Expr $left, Expr $right)
	{
		parent::__construct(5 * $left->hashCode() * $right->hashCode(), $left, $right);
	}

	public function evaluate (Env $env)
	{
		return $this->left->evaluate($env) * $this->right->evaluate($env);
	}

	public function equals ($other)
	{
		if ($other instanceof Mul) {
			return $this->left->equals($other->getLeft()) 
			    && $this->right->equals($other->getRight());
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onMul($this);
	}
} 

class Div extends BinaryExpr {
	public function __construct(Expr $left, Expr $right)
	{
		parent::__construct(7 * $left->hashCode() * $right->hashCode(), $left, $right);
	}

	public function evaluate (Env $env)
	{
		return $this->left->evaluate($env) / $this->right->evaluate($env);
	}

	public function equals ($other)
	{
		if ($other instanceof Div) {
			return $this->left->equals($other->getLeft()) 
			    && $this->right->equals($other->getRight());
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onDiv($this);
	}
} 

class Constant extends Expr {
	protected $value;

	public function __construct($value)
	{
		parent::__construct(11);

		$this->value = $value;
	}

	public function getValue()
	{
		return $this->value;
	}

	public function evaluate (Env $env)
	{
		return $this->value;
	}

	public function equals($other)
	{
		if ($other instanceof Constant) {
			return $this->value == $other->getValue();
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onConst($this);
	}
}

class Variable extends Expr {
	protected $var;

	public function __construct($var)
	{
		parent::__construct(13);

		$this->var = $var;
	}

	public function getVar ()
	{
		return $this->var;
	}

	public function evaluate(Env $env)
	{
		return $env->lookup($this->var);
	}

	public function equals ($other)
	{
		if ($other instanceof Variable) {
			return $this->var == $other->getVar();
		} else {
			return false;
		}
	}

	public function visit(Visitor $visitor)
	{
		$visitor->onVar($this);
	}
}

class Entry {

	protected $next = null;

	protected $epxr;

	public function __construct($expr, $next)
	{
		$this->expr = $expr;
		$this->next = $next;
	}

	public function getExpr()
	{
		return $this->expr;
	}

	public function getNext ()
	{
		return $this->next;
	}
}

class HashTable {
	protected $table;
	protected $size;

	public function __construct($size)
	{
		$this->table = array_fill(0, $size, null);
		$this->size = $size;
	}

	protected function rehash ($hc)
	{
		return $hc & $this->size;
	}

	public function contains ($expr)
	{
		return $this->lookup($expr) != null;
	}

	public function insert ($expr) 
	{
		$entry = $this->table[$this->rehash($expr->hashCode())];
		$this->table[$this->rehash($expr->hashCode())] = new Entry($expr, $entry);
	}

	public function lookup ($expr)
	{
		$entry = $this->table[$this->rehash($expr->hashCode())];
		if ($entry) {
			while ($entry) {
				if ($entry->getExpr()->equals($expr)) {
					return $entry->getExpr();
				}
				$entry = $entry->getNext();
			}
		} 
		return null;
	}
}


class ExprBuilder {
	protected $table;

	public function __construct()
	{
		$this->table = new HashTable (0x1FFF);
	}

	public function normalize ($expr)
	{
		if (!$this->table->contains ($expr)) {
			$this->table->insert($expr);
		}

		return $this->table->lookup($expr);
	}

	public function createMin ($left, $right) 
	{
		return $this->normalize(new Min($left, $right));
	}

	public function createPlus ($left, $right) 
	{
		return $this->normalize(new Plus($left, $right));
	}

	public function createMul ($left, $right) 
	{
		return $this->normalize(new Mul($left, $right));
	}

	public function createDiv ($left, $right) 
	{
		return $this->normalize(new Div($left, $right));
	}

	public function createVar ($var)
	{
		return $this->normalize(new Variable($var));
	}

	public function createConst ($value)
	{
		return $this->normalize(new Constant($value));
	}

	public function createFactor ($expr) 
	{
		return $this->normalize(new Factor($expr));
	}
}

class EmptyStackException extends Exception {}

class Stack {

	protected $table;
	protected $size;

	public function __construct()
	{
		$this->size = 0;
		$this->table = array();
	}

	public function pop()
	{
		if ($this->size > 0) {
			$pos = $this->size - 1;
			$this->size--;
			return $this->table[$pos];
		} else {
			throw new EmptyStackException();
		}
	}

	public function peek()
	{
		if ($this->size > 0) {
			return $this->table[$this->size - 1];
		} else {
			throw new EmptyStackException();
		}
	}

	public function push($value)
	{
		$this->table[$this->size++] = $value;
	}
}

interface Visitor {
	function onMin ($expr);
	function onPlus ($expr);
	function onDiv ($expr);
	function onMul ($expr);
	function onConst ($expr);
	function onVar ($expr);
}

class PrettyPrintingVisitor implements Visitor {
	public function onMin ($expr)
	{
		echo "(";
		echo $expr->getLeft()->visit($this);
		echo " - ";
		echo $expr->getRight()->visit($this);
		echo ")";
	}
	public function onPlus ($expr)
	{
		echo "(";
		echo $expr->getLeft()->visit($this);
		echo " + ";
		echo $expr->getRight()->visit($this);
		echo ")";
	}
	public function onDiv ($expr)
	{
		echo "(";
		echo $expr->getLeft()->visit($this);
		echo " / ";
		echo $expr->getRight()->visit($this);
		echo ")";
	}
	public function onMul ($expr)
	{
		echo "(";
		echo $expr->getLeft()->visit($this);
		echo " * ";
		echo $expr->getRight()->visit($this);
		echo ")";
	}
	public function onFactor ($expr) 
	{
		echo "!";
		echo $expr->getExpr()->visit($this);
	}
	public function onConst ($expr)
	{
		echo $expr->getValue();
	}
	public function onVar ($expr)
	{
		echo $expr->getVar();
	}
}

class EvaluationVisitor implements Visitor {
	protected $stack;
	protected $env;

	public function __construct(Env $env)
	{
		$this->stack = new Stack ();
		$this->env = $env;
	}

	public function onMin ($expr)
	{
		$expr->getLeft()->visit($this);
		$expr->getRight()->visit($this);

		$right = $this->stack->pop();
		$left = $this->stack->pop();
		$this->stack->push($left - $right);
	}

	public function onPlus ($expr)
	{
		$expr->getLeft()->visit($this);
		$expr->getRight()->visit($this);

		$right = $this->stack->pop();
		$left = $this->stack->pop();
		$this->stack->push($left + $right);
	}

	public function onDiv ($expr)
	{
		$expr->getLeft()->visit($this);
		$expr->getRight()->visit($this);

		$right = $this->stack->pop();
		$left = $this->stack->pop();
		$this->stack->push($left / $right);
	}

	public function onMul ($expr)
	{
		$expr->getLeft()->visit($this);
		$expr->getRight()->visit($this);

		$right = $this->stack->pop();
		$left = $this->stack->pop();
		$this->stack->push($left * $right);
	}

	public function onFactor ($expr)
	{
		$expr->getExpr()->visit($this);

		$n = $this->stack->pop();
		$factor = 1;
		while ($n > 1) {
			$factor *= $n;
			$n--;
		}
		$this->stack->push($factor);
	}

	public function onVar ($expr) 
	{
		$this->stack->push($this->env->lookup($expr->getVar()));
	}

	public function onConst ($expr)
	{
		$this->stack->push($expr->getValue());
	}

	public function getResult()
	{
		return $this->stack->peek();
	}
}

class Token {
	const MIN = 1;
	const PLUS = 2;
	const DIV = 3;
	const MUL = 4;
	const LEFTPARENT = 5;
	const RIGHTPARENT = 6;
	const VARIABLE = 7;
	const CONSTANT = 9;
	const EXCLAMATIONMARK = 10;
	const EOF = 11;

	protected $kind;
	protected $value;

	public function __construct($kind, $value)
	{
		$this->kind  = $kind;
		$this->value = $value;
	}

	public function getKind ()
	{
		return $this->kind;
	}

	public function getValue()
	{
		return $this->value;
	}
}

class TokenizeException extends Exception {}

class Tokenizer {
	protected $pos;
	protected $inp;
	protected $current;

	public function __construct($inp) 
	{
		$this->inp = $inp;
		$this->pos = -1;
		$this->next();
	}

	protected function lookAhead()
	{
		return substr($this->inp, $this->pos + 1, 1);
	}

	protected function accept()
	{
		$this->pos++;
	}

	protected function isAlpha($char)
	{
		return ctype_alpha ($char);
	}

	protected function isNumeric($char) 
	{
		return ctype_digit ($char);
	}

	protected function parse ()
	{
		$char = $this->lookAhead();
		switch ($char) {
			case null:
				return new Token (Token::EOF, $char);
			case "(": 
				$this->accept();
				return new Token (Token::LEFTPARENT, $char);
			case ")":
				$this->accept();
				return new Token (Token::RIGHTPARENT, $char);
			case "+":
				$this->accept();
				return new Token (Token::PLUS, $char);
			case "-":
				$this->accept();
				return new Token (Token::MIN, $char);
			case "*":
				$this->accept();
				return new Token (Token::MUL, $char);
			case "/":
				$this->accept();
				return new Token (Token::DIV, $char);
			case "!":
				$this->accept();
				return new Token (Token::EXCLAMATIONMARK, $char);
			default:
				if ($this->isAlpha($char)) {
					$ident = '';
					while ($this->isAlpha($char)) {
						$this->accept();
						$ident .= $char;
						$char = $this->lookAhead();
					}

					return new Token (Token::VARIABLE, $ident);
				}

				if ($this->isNumeric($char)) {
					$value = '';
					while ($this->isNumeric($char)) {
						$this->accept();
						$value .= $char;
						$char = $this->lookAhead();
					}
					return new Token (Token::CONSTANT, (int) $value);
				}

				throw new TokenizeException ("Unexpected character: " . $this->pos);
		}
	}

	public function next ()
	{
		$this->current = $this->parse ();
	}

	public function peek()
	{
		return $this->current;
	}
}

class ParseException extends Exception {}

class Parser {

	protected $builder;

	public function __construct()
	{
		$this->builder = new ExprBuilder();
	}

	public function parse ($inp)
	{
		$this->tokenizer = new Tokenizer($inp);

		$expr = $this->parseExpr();
		$this->match(Token::EOF);
		return $expr;
	}

	protected function lookAhead()
	{
		return $this->tokenizer->peek();
	}

	protected function accept()
	{
		$this->tokenizer->next();
	}

	protected function match ($kind) 
	{
		$current = $this->tokenizer->peek();
		if ($current->getKind() == $kind) {
			$this->tokenizer->next();
			return $current;
		} else {
			throw new ParseException ("Expecting: " . $token);
		}
	}

	protected function parseExpr ()
	{
		$token = $this->lookAhead();
		switch ($token->getKind()) {
			case Token::VARIABLE:
				$this->accept();
				return $this->builder->createVar($token->getValue());

			case Token::CONSTANT:
				$this->accept();
				return $this->builder->createConst($token->getValue());

			case Token::EXCLAMATIONMARK:
				$this->accept();
				$expr = $this->parseExpr();
				return $this->builder->createFactor($expr);

			case Token::LEFTPARENT:
				$this->accept();

				$left  = $this->parseExpr();
				$op    = $this->parseOperator();
				$right = $this->parseExpr();

				$this->match(Token::RIGHTPARENT);

				switch ($op->getKind()) {
					case Token::PLUS: return $this->builder->createPlus ($left, $right);
					case Token::MIN:  return $this->builder->createMin ($left, $right);
					case Token::MUL:  return $this->builder->createMul ($left, $right);
					case Token::DIV:  return $this->builder->createDiv ($left, $right);
				}
				break;
			default:
				throw new ParseException ("Unable to parse expression, expecting: variable, constant or '('");
		}
	}

	protected function parseOperator ()
	{
		$token = $this->lookAhead();
		switch ($token->getKind()) {
			case Token::MIN: 
			case Token::PLUS:
			case Token::MUL:
			case Token::DIV:
				$this->accept();
				return $token;

			default:
				throw new ParseException("Unable to parse operator, expecting: '+', '-', '*' or '/'");
		}
	}
}

/*
$default = new Env();
$default->store("x", 2);
$default->store("y", 5);

$parser = new Parser();

$inp = array ();
$inp[] = array ("(100/2)", 50);
$inp[] = array ("(100/y)", 20);
$inp[] = array ("(10+(20+(5*x)))", 40);
$inp[] = array ("!y", 120);

for ($i = 0; $i < sizeof ($inp); $i++) {
	$evaluator = new EvaluationVisitor ($default);

	$expr = $parser->parse($inp[$i][0]);
	$expr->visit(new PrettyPrintingVisitor ());
	$expr->visit($evaluator);

	echo " = ";
	echo $expr->evaluate ($default);
	echo "\n";

	if ($evaluator->getResult() == $inp[$i][1] && $expr->evaluate($default) == $inp[$i][1]) {
		echo "\nPASS\n";
	} else {
		echo "\nFAIL\n";
	}
}
*/

abstract class TestCase {
    public function setup () {
        
    }
    
    public function tearDown () {
        
    }
    
    public function assertEquals ($mExpected, $mFound) 
    {
        if ($mExpected != $mFound) {
            echo "FAIL\n\tExpected: ";
            var_dump ($mExpected);
            echo "\tFound: ";
            var_dump ($mFound);
        } else {
            echo "PASS\n";
        }
    }
}

class EvaluatorTest extends TestCase {
	protected $parser;
	protected $env;

	public function setup ()
	{
		$this->parser = new Parser ();

		$this->env = new Env();
		$this->env->store("x", 2);
		$this->env->store("y", 5);
	}



	public function testEvaluationVistor ($inp, $res)
	{
		$evaluator = new EvaluationVisitor ($this->env);

		$expr = $this->parser->parse($inp);
		$expr->visit($evaluator);

		$this->assertEquals($res, $evaluator->getResult());
	}

	public function testEvaluateMethod ($inp, $res)
	{
		$expr = $this->parser->parse($inp);
		$this->assertEquals($res, $expr->evaluate($this->env));
	}

	public function testPrettyPrinterVisitor ($inp)
	{
		$expr = $this->parser->parse($inp);
		$expr->visit(new PrettyPrintingVisitor());
		echo "\n";
	}
}

$test = new EvaluatorTest();
$test->setup();
$test->testEvaluationVistor("(100/2)", 50);
$test->tearDown();
$test->setup();
$test->testEvaluationVistor("(100/y)", 20);
$test->tearDown();
$test->setup();
$test->testEvaluationVistor("(10+(20+(5*x)))", 40);
$test->tearDown();
$test->setup();
$test->testEvaluationVistor("!y", 120);
$test->tearDown();

$test->setup();
$test->testEvaluateMethod("(100/2)", 50);
$test->tearDown();
$test->setup();
$test->testEvaluateMethod("(100/y)", 20);
$test->tearDown();
$test->setup();
$test->testEvaluateMethod("(10+(20+(5*x)))", 40);
$test->tearDown();
$test->setup();
$test->testEvaluateMethod("!y", 120);
$test->tearDown();

$test->setup();
$test->testPrettyPrinterVisitor("(100/2)");
$test->tearDown();
$test->setup();
$test->testPrettyPrinterVisitor("(100/y)");
$test->tearDown();
$test->setup();
$test->testPrettyPrinterVisitor("(10+(20+(5*x)))");
$test->tearDown();
$test->setup();
$test->testPrettyPrinterVisitor("!y");
$test->tearDown();

?>